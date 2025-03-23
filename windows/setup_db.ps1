Write-Host "Setting up the database."
$Env:PGPASSWORD=$env:POSTGRES_PASS; '\conninfo' | psql -Upostgres -c "DROP DATABASE IF EXISTS $env:DB_NAME;"
$Env:PGPASSWORD=$env:POSTGRES_PASS; '\conninfo' | psql -Upostgres -c "CREATE USER $env:DB_USER WITH PASSWORD '$env:DB_PASS';"
$Env:PGPASSWORD=$env:POSTGRES_PASS; '\conninfo' | psql -Upostgres -c "ALTER USER $env:DB_USER CREATEDB;"
$Env:PGPASSWORD=$env:POSTGRES_PASS; '\conninfo' | psql -Upostgres -c "CREATE DATABASE $env:DB_NAME OWNER $env:DB_USER;"
$Env:PGPASSWORD=$env:POSTGRES_PASS; '\conninfo' | psql -Upostgres -c "GRANT ALL PRIVILEGES ON DATABASE $env:DB_NAME TO $env:DB_USER;"
$Env:PGPASSWORD=$env:POSTGRES_PASS; '\conninfo' | psql -Upostgres -c "GRANT ALL PRIVILEGES ON SCHEMA public TO $env:DB_USER;"

