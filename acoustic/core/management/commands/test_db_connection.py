# filepath: /home/apcave/recipe-app-api/app/core/management/commands/test_db_connection.py
from django.core.management.base import BaseCommand
from django.db import connections
from django.db.utils import OperationalError

class Command(BaseCommand):
    help = 'Tests the database connection'

    def handle(self, *args, **kwargs):
        db_conn = connections['default']
        try:
            db_conn.cursor()
            self.stdout.write(self.style.SUCCESS('Database connection successful'))
        except OperationalError:
            self.stdout.write(self.style.ERROR('Database connection failed'))