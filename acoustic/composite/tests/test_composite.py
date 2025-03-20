"""
Tests for composite APIs.
"""

from django.contrib.auth import get_user_model
from django.urls import reverse
from django.test import TestCase
import os

from rest_framework import status
from rest_framework.test import APIClient

COMPOSITE_URL = reverse('composite:run_model')


def create_user(email="user@example.com", password="password123"):
    """Helper function to create a user."""
    return get_user_model().objects.create_user(email, password)


def get_model_input_data():
    """Helper function to return model input data."""
    file_path = os.path.join(os.path.dirname(__file__), "modelPayload.json")
    with open(file_path, 'r', encoding='utf-8') as file:
        return file.read()


class PublicCompositeAPITests(TestCase):
    """Test the publicly available composite API."""

    def setUp(self):
        """Set up the PUBLIC test."""
        self.client = APIClient()

    def test_auth_required(self):
        """Test that login is required to access the ingredients."""
        res = self.client.get(COMPOSITE_URL)

        self.assertEqual(res.status_code, status.HTTP_403_FORBIDDEN)


class PrivateCompositeAPITests(TestCase):
    """Test the authorized user ingredients API."""

    def setUp(self):
        """Set up the test."""
        self.user = create_user()
        self.client = APIClient()
        self.client.force_authenticate(self.user)

    def test_run_model(self):
        """Test running a test model."""

        payload = {
            'data': get_model_input_data()
        }
        res = self.client.get(COMPOSITE_URL, payload)

        # print( "Response:", res.data )

        self.assertEqual(res.status_code, status.HTTP_200_OK)
