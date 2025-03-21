import json
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response
from drf_spectacular.utils import extend_schema, OpenApiExample
from drf_spectacular.types import OpenApiTypes

from .utils.run_sim import run_simulation


def convert_json_strings(obj):
    """Recursively convert JSON strings in a dictionary to Python objects."""
    if isinstance(obj, str):
        try:
            obj = json.loads(obj)
        except json.JSONDecodeError:
            return obj
    if isinstance(obj, dict):
        return {k: convert_json_strings(v) for k, v in obj.items()}
    if isinstance(obj, list):
        return [convert_json_strings(i) for i in obj]
    return obj


@extend_schema(
    request=OpenApiTypes.OBJECT,
    responses={200: OpenApiTypes.OBJECT},
    examples=[
        OpenApiExample(
            'Example Request',
            value={
                'data': {}
            }
        ),
    ]
)
@api_view(['POST'])
@permission_classes([IsAuthenticated])
def run_model_endpoint(request):
    """Authenticated endpoint to run the acoustic model."""
    print('request:', request)
    data = request.data

    if not isinstance(data, dict):
        return Response({'error': 'Invalid JSON data'}, status=400)

    try:
        # Recursively convert JSON strings to Python objects
        data = convert_json_strings(data['data'])

        result = run_simulation(data)
    except Exception as e:
        return Response({'error': str(e)}, status=400)

    # Convert the result to a JSON string
    # result_json = json.dumps(result, indent=4)

    # Write the result to a file
    # with open('simulation_result.json', 'w') as file:
    #    file.write(result_json)

    return Response(result)
