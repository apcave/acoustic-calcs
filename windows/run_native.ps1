Write-Host "Testing and running acoustic-calcs natively"
Set-Location .\acoustic
python manage.py test

Write-Host "Testing the db connection"
python manage.py test_db_connection

Write-Host "Collecting static files"
python manage.py collectstatic --noinput

Write-Host "Creating the acoustic API database tables"
python manage.py makemigrations
python manage.py migrate

Write-Host "Running TDD tests"
python manage.py test

python manage.py runserver 0.0.0.0:80