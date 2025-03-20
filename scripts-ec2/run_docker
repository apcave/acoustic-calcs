#!/bin/bash
echo "Launching new acoustic-calc image."


# Check if .env file exists
if [ -f .env ]; then
    echo ".env file found. Sourcing .env"
    source .env
else
    echo ".env file not found. Sourcing alternative_env"
    source ../.env
fi


running_containers=$(sudo docker ps -q)
if [ -n "$running_containers" ]; then
    echo "Stopping all running Docker containers"
    sudo docker stop $running_containers
    sudo docker remove $running_containers
fi

sudo docker run -d -p 80:80 \
    -e DB_HOST=$DB_HOST \
    -e DB_NAME=$DB_NAME \
    -e DB_USER=$DB_USER \
    -e DB_PASS=$DB_PASS \
    -e DB_PORT=$DB_PORT \
    -e DJANGO_SECRET_KEY=$DJANGO_SECRET_KEY \
    -e DJANGO_DEBUG=$DJANGO_DEBUG \
    -e DJANGO_ALLOWED_HOSTS=$DJANGO_ALLOWED_HOSTS \
    --rm apcave/acoustic-calc:latest



echo "Last Deployed - "$(date) >> test.txt

echo "Waiting for the server to start..."
sleep 20

if [ -f .env ]; then
    ./test_server
else
    ./test_server localhost
fi

