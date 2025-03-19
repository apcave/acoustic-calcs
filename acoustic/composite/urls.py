"""
URL mappings for the composite app.
"""
from django.urls import path
from .views import run_model_endpoint

app_name = 'composite'

urlpatterns = [
    path('', run_model_endpoint, name='run_model'),
]
