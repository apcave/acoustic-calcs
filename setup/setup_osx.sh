#!/bin/bash
set -e

# /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/apcave/acoustic-calcs/refs/heads/main/setup/setup_osx)"

# Define color codes
GREEN='\033[0;32m'
NC='\033[0m' # No Color

/bin/bash -c $(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)
brew install git
brew install python
brew install gcc
brew install gfortran
brew install jq
brew install --cask docker
brew install postgresql@14
brew install ninja

if [ ! -f ".env" ]; then
    echo "Creating environment variables for build and run time"
    echo "Values may be changed as needed"
    echo "DB_NAME=acoustic_api" > .env
    echo "DB_USER=acoustic" >> .env
    echo "DB_PASS=$(uuidgen)" >> .env
    echo "DB_HOST=localhost" >> .env
    echo "DB_PORT=5432" >> .env
    echo "DJANGO_DEBUG=1" >> .env
    echo "DJANGO_ALLOWED_HOSTS=*" >> .env
    echo "DJANGO_SECRET_KEY=$(uuidgen)" >> .env
    echo "DJANGO_SUPERUSER_EMAIL=super@user.org" >> .env
    echo "DJANGO_SUPERUSER_PASSWORD=$(uuidgen)" >> .env
    echo "DJANGO_NORMAL_USER_NAME=normal" >> .env
    echo "DJANGO_NORMAL_USER_EMAIL=normal@user.org" >> .env
    echo "DJANGO_NORMAL_USER_PASSWORD=$(uuidgen)" >> .env
fi
echo "Environment variables:"
source .env
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

# Start PostgreSQL service

brew services start postgresql
sql_config=$(echo "SHOW config_file;" | psql postgres | sed 's|^[^/]*||')
hba_config=$(echo "SHOW hba_file;" | psql postgres | sed 's|^[^/]*||')

echo $sql_config
echo $hba_config

brew services stop postgresql
echo "listen_addresses = '*'" | sudo tee -a $sql_config
echo "port = $DB_PORT" | sudo tee -a $sql_config
echo "host all all 0.0.0.0/0 scram-sha-256" | sudo tee -a $hba_config

brew services start postgresql
sleep 5
lsof -i -P -n | grep postgres


# Log in to PostgreSQL and execute commands
# Create PostgreSQL user and database
psql postgres <<EOF
CREATE USER $DB_USER WITH PASSWORD '$DB_PASS';
ALTER USER $DB_USER CREATEDB;
ALTER USER $DB_USER WITH PASSWORD '$DB_PASS';
DROP DATABASE $DB_NAME;
CREATE DATABASE $DB_NAME OWNER $DB_USER;
GRANT ALL PRIVILEGES ON DATABASE $DB_NAME TO $DB_USER;
GRANT ALL PRIVILEGES ON SCHEMA public TO $DB_USER;
EOF



# Clone the repository
echo "Clone the API repository"
git clone https://github.com/apcave/acoustic-calcs.git
cp .env acoustic-calcs/
cd acoustic-calcs

echo "Install the python packages to the virtual environment."
python3 -m venv ./venv
echo "Don't forget to run 'source ./venv/bin/activate' to activate the virtual environment in the future."
source ./venv/bin/activate
pip install -r requirements.txt

echo "Create the python module from the Fortran code...."
cd composite-sim/src
f2py -c -m levesque levesque.F90
mv levesque*.so ../../acoustic/composite/utils/
cd ../../

cd acoustic
python manage.py test_db_connection


echo "Creating the acoustic API database tables"
python manage.py makemigrations
python manage.py migrate


# Run the Python script to create the superuser
python manage.py create_super_user "$DJANGO_SUPERUSER_EMAIL" "$DJANGO_SUPERUSER_PASSWORD"
# Run the Python script to create the superuser
python manage.py create_user "$DJANGO_NORMAL_USER_EMAIL" "$DJANGO_NORMAL_USER_PASSWORD"

echo "Running TDD tests"
python manage.py test
echo -e "${GREEN}Native builds completed.${NC}"

cd ..
echo "Run the Django server in a Docker container."
./run_docker.sh

echo -e "${GREEN}Docker and native builds completed.${NC}"
echo -e "${GREEN}Run "python manage.py runserver 0.0.0.0:8080" in the acoustic-calcs/acoustic directory${NC}"
echo -e "${GREEN}The dockfile is running on 0.0.0.0:80${NC}"

./scripts-ec2/test_server.sh localhost