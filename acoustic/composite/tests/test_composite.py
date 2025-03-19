"""
Tests for ingredient APIs.
"""
from decimal import Decimal

from django.contrib.auth import get_user_model
from django.urls import reverse
from django.test import TestCase

from rest_framework import status
from rest_framework.test import APIClient



COMPOSITE_URL = reverse('composite:composite')


def detail_url(ingredient_id):
    """Create and return an ingredient detail URL."""
    return reverse('recipe:ingredient-detail', args=[ingredient_id])


def create_user(email="user@example.com", password="password123"):
    """Helper function to create a user."""
    return get_user_model().objects.create_user(email, password)


class PublicCompositeAPITests(TestCase):
    """Test the publicly available ingredients API."""

    def setUp(self):
        """Set up the test."""
        self.client = APIClient()

    def test_auth_required(self):
        """Test that login is required to access the ingredients."""
        res = self.client.get(COMPOSITE_URL)

        self.assertEqual(res.status_code, status.HTTP_401_UNAUTHORIZED)

