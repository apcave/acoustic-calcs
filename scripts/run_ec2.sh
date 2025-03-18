#!/bin/bash
source .env

# Stop all running Docker containers
sudo docker stop $(sudo docker ps -q) || true
# Remove all stopped Docker containers
sudo docker rm $(sudo docker ps -a -q) || true

# Define the Docker image and label
DOCKER_IMAGE="apcave/acoustic-calc:latest"
DOCKER_LABEL="acoustic"

# Log the Docker image that will be started
echo "Starting Docker container with image: $DOCKER_IMAGE"

# Run the Docker container with a label
sudo docker run -d -p 80:80 \
    -e DB_HOST=$DB_HOST \
    -e DB_NAME=$DB_NAME \
    -e DB_USER=$DB_USER \
    -e DB_PASS=$DB_PASS \
    -e DB_PORT=$DB_PORT \
    -e DJANGO_SECRET_KEY=$DJANGO_SECRET_KEY \
    -e DJANGO_DEBUG=$DJANGO_DEBUG \
    -e DJANGO_ALLOWED_HOSTS=$DJANGO_ALLOWED_HOSTS \
    --label $DOCKER_LABEL \
    --name acoustic \
    $DOCKER_IMAGE

sudo docker logs $DOCKER_LABEL

# Check open ports
ss -tuln