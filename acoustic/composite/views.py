"""
Core views for app.
"""
import json
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response
from utils import run_sim

@api_view(['GET'])
@permission_classes([IsAuthenticated])
def run_model_endpoint(request):
    """Authenticated endpoint to run the acoustic model."""
    # Get the JSON data from the query parameters
    json_data = request.query_params.get('data', '{}')
    
    try:
        # Convert the JSON data to a Python object
        data = json.loads(json_data)
    except json.JSONDecodeError:
        return Response({'error': 'Invalid JSON data'}, status=400)
    
    # Process the data (example)
    result = run_sim(data)
    
    return Response({'message': 'This is a protected endpoint.', 'result': result})