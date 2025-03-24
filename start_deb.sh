#!/bin/bash
source .env
source ./venv/bin/activate
export $(cat .env | xargs)

echo "DB_NAME=$DB_NAME"
echo "DB_USER=$DB_USER"
echo "DB_PASS=$DB_PASS"
echo "DB_HOST=$DB_HOST"
echo "DB_PORT=$DB_PORT"
echo "DJANGO_DEBUG=$DJANGO_DEBUG"
echo "DJANGO_ALLOWED_HOSTS=$DJANGO_ALLOWED_HOSTS"
echo "DJANGO_SECRET_KEY=$DJANGO_SECRET_KEY"
echo "DJANGO_SUPERUSER_EMAIL=$DJANGO_SUPERUSER_EMAIL"
echo "DJANGO_SUPERUSER_PASSWORD=$DJANGO_SUPERUSER_PASSWORD"
echo "DJANGO_NORMAL_USER_NAME=$DJANGO_NORMAL_USER_NAME"
echo "DJANGO_NORMAL_USER_EMAIL=$DJANGO_NORMAL_USER_EMAIL"
echo "DJANGO_NORMAL_USER_PASSWORD=$DJANGO_NORMAL_USER_PASSWORD"

echo "Testing the database connection"
cd acoustic
python manage.py test_db_connection