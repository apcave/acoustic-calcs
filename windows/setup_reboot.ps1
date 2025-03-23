.\start_win.ps1
.\windows\build_fortran.ps1
.\windows\setup_db.ps1


# Write-Host "Creating the superuser"
python manage.py create_super_user $env:DJANGO_SUPERUSER_EMAIL $env:DJANGO_SUPERUSER_PASSWORD

.\windows\run_native.ps1




