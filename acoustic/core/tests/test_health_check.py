""""
Tests for the health check API.
"""
from django.test import TestCase
from django.urls import reverse

from rest_framework import status
from rest_framework.test import APIClient


class HealthCheckTestCase(TestCase):
    """Test the health check API."""

    def test_health_check(self):
        """Test the health check API."""

        client = APIClient()
        url = reverse('health-check')
        response = client.get(url)

        self.assertEqual(response.status_code, status.HTTP_200_OK)
        self.assertEqual(response.data, {'healthy': True})
