"""
Tests for composite APIs.
"""
from ..utils.compare_json import compare_json
from django.contrib.auth import get_user_model
from django.urls import reverse
from django.test import TestCase
import os
import json

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


def get_model_output_data():
    """Helper function to return model output data."""
    file_path = os.path.join(os.path.dirname(__file__), "resultsPayload.json")
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

        self.assertEqual(res.status_code, status.HTTP_401_UNAUTHORIZED)


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
        res = self.client.post(COMPOSITE_URL, payload, format='json')

        model_str = json.dumps(res.data)
        test_str = get_model_output_data()

        json1 = json.loads(model_str)
        json2 = json.loads(test_str)

        self.assertTrue(compare_json(json1, json2, tolerance=1e-6))

        # file_path = os.path.join(os.path.dirname(__file__),
        # "resultsPayload.json")
        # with open(file_path, 'w', encoding='utf-8') as file:
        #    file.write(model_str)

        self.assertEqual(res.status_code, status.HTTP_200_OK)
