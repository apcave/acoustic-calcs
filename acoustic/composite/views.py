"""
Core views for app.
"""
from rest_framework.decorators import api_view, permission_classes
from rest_framework.permissions import IsAuthenticated
from rest_framework.response import Response

@api_view(['GET'])
@permission_classes([IsAuthenticated])
def run_model_endpoint(request):
    """Authenticated endpoint to run the acoustic model."""
    return Response({'message': 'This is a protected endpoint.'})