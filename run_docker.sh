#!/bin/bash

set -e

source .env

echo "Checking for running Docker containers and stopping them."
running_containers=$(sudo docker ps -q)
if [ -n "$running_containers" ]; then
    echo "Stopping all running Docker containers"
    sudo docker stop $running_containers
fi

echo "Check for existing Docker containers with the name 'acoustic' and remove them"
existing_container=$(sudo docker ps -a -q -f name=acoustic)
if [ -n "$existing_container" ]; then
    echo "Removing existing Docker container with name 'acoustic'"
    sudo docker rm -f $existing_container
fi


# Build the Docker image
# sudo docker build --no-cache -t acoustic .
echo "Building the Docker container."
sudo docker build -t acoustic .

echo "Run the Docker container. (Setup for PostgreSQL on host)"
# Get the host's IP address
HOST_IP=$(hostname -I | awk '{print $1}')


sudo docker run -d --network host \
    -e DB_HOST=$HOST_IP \
    -e DB_NAME=$DB_NAME \
    -e DB_USER=$DB_USER \
    -e DB_PASS=$DB_PASS \
    -e DB_PORT=$DB_PORT \
    -e DJANGO_SECRET_KEY=$DJANGO_SECRET_KEY \
    -e DJANGO_DEBUG=$DJANGO_DEBUG \
    -e DJANGO_ALLOWED_HOSTS=$DJANGO_ALLOWED_HOSTS \
    --name acoustic acoustic

# Check container logs
echo "Checking the docker has an open port"
lsof -i -P -n | grep LISTEN
# ss -tuln

echo "Checking the docker logs, look for the result of the internal health check"
sleep 10
sudo docker logs acoustic
