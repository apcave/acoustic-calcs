from django.core.management.base import BaseCommand
from django.contrib.auth import get_user_model


class Command(BaseCommand):
    help = 'Create a superuser non-interactively'

    def add_arguments(self, parser):
        parser.add_argument('email', type=str,
                            help='The email address of the superuser')
        parser.add_argument('password', type=str,
                            help='The password for the superuser')

    def handle(self, *args, **kwargs):
        User = get_user_model()
        email = kwargs['email']
        password = kwargs['password']

        if not email or not password:
            self.stdout.write(
                self.style.ERROR('Email and password are required'))
            return

        if not User.objects.filter(email=email).exists():
            User.objects.create_superuser(email=email, password=password)
            self.stdout.write(
                self.style.SUCCESS('Superuser created successfully.'))
        else:
            self.stdout.write(
                self.style.WARNING('Superuser already exists.'))
