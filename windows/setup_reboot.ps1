.\windows\source.ps1

.\windows\build_fortran.ps1

Write-Host "Setting up the database."
$Env:PGPASSWORD=$env:POSTGRES_PASS; '\conninfo' | psql -Upostgres -c "CREATE USER $env:DB_USER WITH PASSWORD '$env:DB_PASS';"
$Env:PGPASSWORD=$env:POSTGRES_PASS; '\conninfo' | psql -Upostgres -c "ALTER USER $env:DB_USER CREATEDB;"
$Env:PGPASSWORD=$env:POSTGRES_PASS; '\conninfo' | psql -Upostgres -c "CREATE DATABASE $env:DB_NAME OWNER $env:DB_USER;"
$Env:PGPASSWORD=$env:POSTGRES_PASS; '\conninfo' | psql -Upostgres -c "GRANT ALL PRIVILEGES ON DATABASE $env:DB_NAME TO $env:DB_USER;"
$Env:PGPASSWORD=$env:POSTGRES_PASS; '\conninfo' | psql -Upostgres -c "GRANT ALL PRIVILEGES ON SCHEMA public TO $env:DB_USER;"

.\start_win.ps1

Write-Host "Creating the superuser"
python manage.py create_super_user $env:DJANGO_SUPERUSER_EMAIL $env:DJANGO_SUPERUSER_PASSWORD

.\windows\run_native.ps1




