param (
    [string]$hostname = "localhost"
)

Write-Host "Performing tests on host at $hostname using local environment variables."


.\start_win.ps1

$response = Invoke-RestMethod -Uri "http://$hostname/api/health-check/" -Method Get


#$response | ConvertTo-Json
if ($response.PSObject.Properties['healthy'] -and $response.healthy -eq $true) {
    Write-Host "Django server Health check passed"
} else {
    Write-Host "Django server Health check failed"
    exit 1
}

Write-Host "acoustic-calcs user email    : $env:DJANGO_NORMAL_USER_EMAIL"
Write-Host "acoustic-calcs user password : $env:DJANGO_NORMAL_USER_PASSWORD"

# Create the basic authentication header
$body = @{
    email = $env:DJANGO_NORMAL_USER_EMAIL
    password = $env:DJANGO_NORMAL_USER_PASSWORD
} | ConvertTo-Json

$response = Invoke-RestMethod -Uri "http://$hostname/api/user/token/" -Method Post -Body $body -ContentType "application/json"

#$response | ConvertTo-Json
if ($response.PSObject.Properties['access'] ) {
    $access_token = $response.access
    Write-Host "Received JWT for user : $access_token"
} else {
    Write-Host "Failed to login and get token."
    exit 1
}

# Create the basic authentication header
$body = @{
    email = $env:DJANGO_NORMAL_USER_EMAIL
    password = $env:DJANGO_NORMAL_USER_PASSWORD
} | ConvertTo-Json

Write-Host "Testing the JWT token on restricted access URL."

$headers = @{
    Authorization = "Bearer $access_token"
}

$response = Invoke-RestMethod -Uri "http://$hostname/api/user/me/" -Method Get -Headers $headers

$response | ConvertTo-Json
if ($response.PSObject.Properties['email'] -and $response.email -ne $env:DJANGO_NORMAL_USER_EMAIL) {
    Write-Host "Failed to get user details."
    exit 1
}
Write-Host "Successfully got user details."

$jsonSendContent = Get-Content -Path .\scripts-ec2\modelPayload.json -Raw | ConvertFrom-Json
$parentObject = @{
    data = $jsonSendContent
} | ConvertTo-Json -Depth 100

#Write-Host "Testing the model endpoint."
#Write-Host $parentObject

$response = Invoke-RestMethod -Uri "http://$hostname/api/composite/" -Method Post -Body $parentObject -Headers $headers -ContentType "application/json"

$response | ConvertTo-Json -Depth 100 | Set-Content -Path .\test.json

#$response | Out-File -FilePath .\test.json -Encoding utf8

python .\scripts-ec2\compare_json.py  .\scripts-ec2\resultsPayload.json .\test.json --tolerance 1e-9


# Compare the JSON files
$result = python .\scripts-ec2\compare_json.py .\scripts-ec2\resultsPayload.json .\test.json --tolerance 1e-9

# Print the result of the Python script
if ($result -match "The JSON objects are equal within the given tolerance.") {
    Write-Host "Comparison successful: The JSON objects are equal within the given tolerance."
} else {
    Write-Host "Comparison failed: The JSON objects are not equal within the given tolerance."
}

# Delete the test.json file
Remove-Item -Path .\test.json -Force


