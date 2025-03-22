# Define color codes
$GREEN = "`e[0;32m"
$NC = "`e[0m" # No Color

# Install Homebrew
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew install git
brew install python
brew install gcc
brew install gfortran
brew install jq
brew install --cask docker
brew install postgresql@14
brew install ninja

if (-Not (Test-Path ".env")) {
    Write-Host "Creating environment variables for build and run time"
    Write-Host "Values may be changed as needed"
    $env:DB_NAME = "acoustic_api"
    $env:DB_USER = "acoustic"
    $env:DB_PASS = [guid]::NewGuid().ToString()
    $env:DB_HOST = "localhost"
    $env:DB_PORT = "5432"
    $env:DJANGO_DEBUG = "1"
    $env:DJANGO_ALLOWED_HOSTS = "*"
    $env:DJANGO_SECRET_KEY = [guid]::NewGuid().ToString()
    $env:DJANGO_SUPERUSER_EMAIL = "super@user.org"
    $env:DJANGO_SUPERUSER_PASSWORD = [guid]::NewGuid().ToString()
    $env:DJANGO_NORMAL_USER_NAME = "normal"
    $env:DJANGO_NORMAL_USER_EMAIL = "normal@user.org"
    $env:DJANGO_NORMAL_USER_PASSWORD = [guid]::NewGuid().ToString()

    @"
DB_NAME=$($env:DB_NAME)
DB_USER=$($env:DB_USER)
DB_PASS=$($env:DB_PASS)
DB_HOST=$($env:DB_HOST)
DB_PORT=$($env:DB_PORT)
DJANGO_DEBUG=$($env:DJANGO_DEBUG)
DJANGO_ALLOWED_HOSTS=$($env:DJANGO_ALLOWED_HOSTS)
DJANGO_SECRET_KEY=$($env:DJANGO_SECRET_KEY)
DJANGO_SUPERUSER_EMAIL=$($env:DJANGO_SUPERUSER_EMAIL)
DJANGO_SUPERUSER_PASSWORD=$($env:DJANGO_SUPERUSER_PASSWORD)
DJANGO_NORMAL_USER_NAME=$($env:DJANGO_NORMAL_USER_NAME)
DJANGO_NORMAL_USER_EMAIL=$($env:DJANGO_NORMAL_USER_EMAIL)
DJANGO_NORMAL_USER_PASSWORD=$($env:DJANGO_NORMAL_USER_PASSWORD)
"@ | Out-File -FilePath .env -Encoding ascii
}

# Function to load .env file and set environment variables
function Set-EnvVarsFromFile {
    param (
        [string]$envFilePath
    )
    Get-Content $envFilePath | ForEach-Object {
        if ($_ -match "^\s*([^#][^=]+?)\s*=\s*(.+?)\s*$") {
            [System.Environment]::SetEnvironmentVariable($matches[1], $matches[2])
        }
    }
}

# Load environment variables from .env file
Set-EnvVarsFromFile -envFilePath ".env"

Write-Host "Environment variables:"
Get-Content .env

# Add ./windows directory to the PATH
$windowsPath = (Resolve-Path "./windows").Path
$env:PATH = "$windowsPath;$env:PATH"

# Verify the PATH update
Write-Host "Updated PATH: $env:PATH"

# Start PostgreSQL service
brew services start postgresql@14

# Wait for PostgreSQL to start
Start-Sleep -Seconds 5

# Log in to PostgreSQL and execute commands
# Create PostgreSQL user and database
$psqlCommand = "psql -U postgres -h $env:DB_HOST -p $env:DB_PORT -c"
Invoke-Expression "$psqlCommand `"CREATE USER $env:DB_USER WITH PASSWORD '$env:DB_PASS';`""
Invoke-Expression "$psqlCommand `"ALTER USER $env:DB_USER CREATEDB;`""
Invoke-Expression "$psqlCommand `"CREATE DATABASE $env:DB_NAME OWNER $env:DB_USER;`""
Invoke-Expression "$psqlCommand `"GRANT ALL PRIVILEGES ON DATABASE $env:DB_NAME TO $env:DB_USER;`""
Invoke-Expression "$psqlCommand `"GRANT ALL PRIVILEGES ON SCHEMA public TO $env:DB_USER;`""

# Clone the repository
Write-Host "Clone the API repository"
git clone https://github.com/apcave/acoustic-calcs.git
Copy-Item -Path .env -Destination acoustic-calcs/
Set-Location -Path acoustic-calcs

Write-Host "Install the python packages to the virtual environment."
if (-Not (Test-Path "./venv")) {
    python3 -m venv ./venv
    Write-Host "Don't forget to run 'source ./venv/bin/activate' to activate the virtual environment in the future."
}
& ./venv/Scripts/Activate.ps1
pip install -r requirements.txt

Write-Host "Create the python module from the Fortran code...."
Set-Location -Path composite-sim/src
f2py -c -m levesque levesque.F90
Move-Item -Path levesque*.so -Destination ../../acoustic/composite/utils/
Set-Location -Path ../../

Set-Location -Path acoustic
python manage.py test_db_connection

Write-Host "Creating the acoustic API database tables"
python manage.py makemigrations
python manage.py migrate

# Run the Python script to create the superuser
python manage.py create_super_user "$env:DJANGO_SUPERUSER_EMAIL" "$env:DJANGO_SUPERUSER_PASSWORD"

Write-Host "Running TDD tests"
python manage.py test
Write-Host "${GREEN}Native builds completed.${NC}"

Set-Location -Path ..
Write-Host "Run the Django server in a Docker container."
./run_docker

Write-Host "${GREEN}Docker and native builds completed.${NC}"
Write-Host "${GREEN}Run 'python manage.py runserver 0.0.0.0:8080' in the acoustic-calcs/acoustic directory${NC}"
Write-Host "${GREEN}The dockfile is running on 0.0.0.0:80${NC}"

Set-Location -Path scripts-ec2
./test_server localhost
