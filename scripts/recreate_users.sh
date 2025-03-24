#!/bin/bash
set -e
source .env

sudo -u postgres psql <<EOF
CREATE USER $DB_USER WITH PASSWORD '$DB_PASS';
ALTER USER $DB_USER CREATEDB;
ALTER USER $DB_USER WITH PASSWORD '$DB_PASS';
DROP DATABASE $DB_NAME;
CREATE DATABASE $DB_NAME OWNER $DB_USER;
GRANT ALL PRIVILEGES ON DATABASE $DB_NAME TO $DB_USER;
GRANT ALL PRIVILEGES ON SCHEMA public TO $DB_USER;
EOF

cd acoustic
echo "Creating the acoustic API database tables"
python manage.py makemigrations
python manage.py migrate

# Run the Python script to create the superuser
python manage.py create_super_user "$DJANGO_SUPERUSER_EMAIL" "$DJANGO_SUPERUSER_PASSWORD"
python manage.py create_user "$DJANGO_NORMAL_USER_EMAIL" "$DJANGO_NORMAL_USER_PASSWORD"

# sudo docker stop acoustic
# python manage.py runserver 0.0.0.0:8080

# ./scripts-ec2/test_server.sh localhost:8080