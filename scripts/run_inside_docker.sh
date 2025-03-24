#!/bin/sh
set -e

#ls -lah
#pwd 

# echo DB_NAME=$DB_NAME
# echo DB_USER=$DB_USER
# echo DB_PASS=$DB_PASS
# echo DB_HOST=$DB_HOST
# echo DB_PORT=$DB_PORT
# echo DJANGO_SECRET_KEY=$DJANGO_SECRET_KEY
# echo DJANGO_DEBUG=$DJANGO_DEBUG
# echo DJANGO_ALLOWED_HOSTS=$DJANGO_ALLOWED_HOSTS

#DJANGO_DEBUG=0

python --version

# echo "Sleep for 2 days to get a shell for debugging."
# echo "Run : sudo docker exec -it acoustic /bin/bash"
# sleep 2d

# Start Nginx
echo "Starting Nginx"
service nginx start || { echo 'Starting Nginx failed' ; cat /var/log/nginx/error.log ; exit 1; }

su django-user

echo "Waiting for postgres..."
python manage.py wait_for_db

echo "Collecting static files"
python manage.py collectstatic --noinput

echo "Running migrations"
python manage.py migrate

# Do not run tests unless required as they slow the boot time.
# echo "Running tests"
# python manage.py test


echo "Starting server"
# Note server runs in background
exec su -c "uwsgi --socket 127.0.0.1:8000 --workers 4 --master --enable-threads --module acoustic.wsgi" django-user & 

# Capture the PID of uwsgi
UWSGI_PID=$!

# Note the python server will not work with Nginx!
# python manage.py runserver 127.0.0.1:8000 &

# The curl command results shows in the logs "docker logs <container_id>"
# Wait for the server to start
sleep 4

echo "Checking the internal ports are open."
lsof -i -P -n | grep LISTEN


service nginx status 

echo "Checking the response from the re-routed Nginx server."
response=$(curl -G http://0.0.0.0:80/api/health-check/)
if echo "$response" | jq -e '.healthy == true' > /dev/null; then
    echo "Nginx server is ok" 
else
    echo "Nginx server failed" 
fi

# Wait for the uwsgi process to complete
wait $UWSGI_PID