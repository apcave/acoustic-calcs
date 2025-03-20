"""
Views for the user API.
"""
from rest_framework import generics
from rest_framework.permissions import IsAuthenticated
from .serializers import UserSerializer


class CreateUserView(generics.CreateAPIView):
    """Create a new user in the system. Existing users can create new users."""
    serializer_class = UserSerializer
    permission_classes = [IsAuthenticated]


class ManageUserView(generics.RetrieveUpdateAPIView):
    """Manage the authenticated user."""
    serializer_class = UserSerializer
    permission_classes = [IsAuthenticated]

    def get_object(self):
        """Retrieve and return the authenticated user."""
        return self.request.user
