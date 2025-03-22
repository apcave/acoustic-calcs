Write-Host "Building the fortran files for the physics engine."

Set-Location composite-sim\src
f2py -c -m levesque levesque.F90
Set-Location ..\..

# Delete the existing levesque.*.pyd files in the acoustic\composite\utils directory
Remove-Item -Path ".\acoustic\composite\utils\levesque.*.pyd" -ErrorAction SilentlyContinue

# Move the new levesque.*.pyd file to the acoustic\composite\utils directory
Move-Item -Path ".\composite-sim\src\levesque.*.pyd" -Destination ".\acoustic\composite\utils\"

Get-ChildItem -Path "acoustic\composite\utils"
