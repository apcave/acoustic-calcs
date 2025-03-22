
if (-Not (Test-Path ".env")) {
    Write-Host "Creating environment variables for build and run time"
    Write-Host "Values may be changed as needed"
    $env:DB_NAME = "acoustic_api"
    $env:DB_USER = "acoustic"
    $env:DB_PASS = [guid]::NewGuid().ToString()
    $env:DB_HOST = "localhost"
    $env:DB_PORT = "5432"
    $env:POSTGRES_PASS = [guid]::NewGuid().ToString()
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
POSTGRES_PASS=$($env:POSTGRES_PASS)
DJANGO_DEBUG=$($env:DJANGO_DEBUG)
DJANGO_ALLOWED_HOSTS=$($env:DJANGO_ALLOWED_HOSTS)
DJANGO_SECRET_KEY=$($env:DJANGO_SECRET_KEY)
DJANGO_SUPERUSER_EMAIL=$($env:DJANGO_SUPERUSER_EMAIL)
DJANGO_SUPERUSER_PASSWORD=$($env:DJANGO_SUPERUSER_PASSWORD)
DJANGO_NORMAL_USER_NAME=$($env:DJANGO_NORMAL_USER_NAME)
DJANGO_NORMAL_USER_EMAIL=$($env:DJANGO_NORMAL_USER_EMAIL)
DJANGO_NORMAL_USER_PASSWORD=$($env:DJANGO_NORMAL_USER_PASSWORD)
"@ | Out-File -FilePath .env -Encoding ascii
} else {
    Get-Content ".env" | ForEach-Object {
        if ($_ -match "^\s*([^#][^=]+?)\s*=\s*(.+?)\s*$") {
            [System.Environment]::SetEnvironmentVariable($matches[1], $matches[2])
        }
    }
}

Write-Host "All environment variables:"
Get-ChildItem Env:


# Set the password for the postgres user
# $postgresPassword = "581f66f99ce64bc981f24b23130cf386" # Replace with the new password you set
# $Env:PGPASSWORD="581f66f99ce64bc981f24b23130cf386"; '\conninfo' | psql -Upostgres
# Log in to PostgreSQL and execute commands
# Create PostgreSQL user and database
# psql postgres -c "CREATE USER $env:DB_USER WITH PASSWORD '$env:DB_PASS';"
# psql postgres -c "ALTER USER $env:DB_USER CREATEDB;"
# psql postgres -c "CREATE DATABASE $env:DB_NAME OWNER $env:DB_USER;"
# psql postgres -c "GRANT ALL PRIVILEGES ON DATABASE $env:DB_NAME TO $env:DB_USER;"
# psql postgres -c "GRANT ALL PRIVILEGES ON SCHEMA public TO $env:DB_USER;"