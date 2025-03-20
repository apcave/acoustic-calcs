"""
Core views for app.
"""
from drf_spectacular.utils import (
    extend_schema,
    extend_schema_view,
    OpenApiParameter,
    OpenApiTypes,
)

import json
from rest_framework.decorators import api_view, permission_classes
from rest_framework.authentication import TokenAuthentication
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response
from .utils.run_sim import run_simulation





@api_view(['POST'])
@permission_classes([IsAuthenticated])
def run_model_endpoint(request):
    """Authenticated endpoint to run the acoustic model."""
    # Get the JSON data from the query parameters
    data = request.data

    # Ensure the data is a valid JSON object
    if not isinstance(data, dict):
        return Response({'error': 'Invalid JSON data'}, status=400)

    # Process the data
    result = run_simulation(data)

    # print("Data:", data)

    # result = data

    # Convert the result to a JSON string
    result = json.dumps(result)

    return Response({'message': 'This is a protected endpoint.',
                    'result': result})
