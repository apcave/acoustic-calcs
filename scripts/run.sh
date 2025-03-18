#!/bin/sh
set -e

ls -lah
pwd 

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


echo "Starting server"
exec su -c "uwsgi --socket 127.0.0.1:8000 --workers 4 --master --enable-threads --module acoustic.wsgi" django-user