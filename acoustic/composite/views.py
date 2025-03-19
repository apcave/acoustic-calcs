"""
Core views for app.
"""
import json
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response
from .utils.run_sim import run_simulation


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

    # print("Data:", data)

    # Process the data (example)
    result = run_simulation(data)

    # result = data

    # Convert the result to a JSON string
    result = json.dumps(result)

    return Response({'message': 'This is a protected endpoint.',
                    'result': result})
