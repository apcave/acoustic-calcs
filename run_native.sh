#!/bin/bash
set -e

source .env
export $(cat .env | xargs)

echo "The database host is : $DB_HOST"

if [ ! -d "./venv" ]; then
    echo "Creating virtual environment"
    python3 -m venv ./venv
    source ./venv/bin/activate
    pip install -r requirements.txt
else
    echo "Activating existing virtual environment"
    source ./venv/bin/activate
fi

echo "Killing all Python processes"
pkill -f python

cd acoustic

# Run the server in the background
echo "Starting the Django server in the background"
python manage.py runserver 0.0.0.0:8080 &

# Wait for the server to start
sleep 2

cd ..
# Run the test script to check the server
echo "Running the test script to check the server"
./scripts-ec2/test_server.sh localhost:8080
