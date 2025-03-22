.\start_win.ps1

$Env:PGPASSWORD="test"; '\conninfo' | psql -U postgres -c "alter user postgres with password '$env:POSTGRES_PASS';"

.\windows\build_fortran.ps1
.\windows\setup_db.ps1

Set-Location .\acoustic
Write-Host "Creating the acoustic API database tables"
python manage.py makemigrations
python manage.py migrate

# Write-Host "Creating the superuser"
python manage.py create_super_user $env:DJANGO_SUPERUSER_EMAIL $env:DJANGO_SUPERUSER_PASSWORD
python manage.py create_user $env:DJANGO_NORMAL_USER_EMAIL $env:DJANGO_NORMAL_USER_PASSWORD
Set-Location ..

.\windows\run_native.ps1




