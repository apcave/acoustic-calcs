.\start_win.ps1

$running_containers = docker ps -q

if ($running_containers) {
    Write-Host "Stopping all running Docker containers"
    docker stop $running_containers
    docker rm $running_containers
}

docker remove acoustic
# Build the Docker image
# sudo docker build --no-cache -t acoustic .
docker build -t acoustic .

# Run the Docker container
Write-Host "Running the Docker container, connecting to the host for the database"
docker run -d -p 80:80 `
    -e DB_HOST=host.docker.internal `
    -e DB_NAME=$env:DB_NAME `
    -e DB_USER=$env:DB_USER `
    -e DB_PASS=$env:DB_PASS `
    -e DB_PORT=$env:DB_PORT `
    -e DJANGO_SECRET_KEY=$env:DJANGO_SECRET_KEY `
    -e DJANGO_DEBUG=$env:DJANGO_DEBUG `
    -e DJANGO_ALLOWED_HOSTS=$env:DJANGO_ALLOWED_HOSTS `
    --name acoustic acoustic


Write-Host "Checking the docker logs, look for the result of the internal health check (wait 10 seconds)"
Start-Sleep -Seconds 10
docker logs acoustic