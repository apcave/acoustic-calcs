#!/bin/bash

# Set EC2 server IP to the first argument if provided, otherwise default to first EC2 server.
HOSTNAME=${1:-3.104.75.129}

# Check if .env file exists
if [ -f .env ]; then
    echo ".env file found. Sourcing .env"
    source .env
else
    echo ".env file not found. Sourcing alternative_env"
    source ../.env
fi

# Define color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo "Checking the internal ports are open."
lsof -i -P -n | grep LISTEN
echo "The server is located at $HOSTNAME"
echo "Checking the response from the django server."
response=$(curl -G http://$HOSTNAME/api/health-check/)
if echo "$response" | jq -e '.healthy == true' > /dev/null; then
    echo -e "${GREEN}django server is ok${NC}"
else
    echo -e "${RED}django server failed${NC}"
    exit 1
fi

echo "acoustic-calcs password: $DJANGO_NORMAL_USER_PASSWORD"
echo "acoustic-calcs email   : $DJANGO_NORMAL_USER_EMAIL"

# echo "Creating a new user"
# curl -X 'POST' \
#  'http://'$HOSTNAME'/api/user/create/' \
#  -H 'accept: application/json' \
#  -H 'Content-Type: multipart/form-data' \
#  -F 'email='$CALC_EMAIL \
#  -F 'password='$CALC_PASS \
#  -F 'name='$CALC_USER

token_response=$(curl -X 'POST' \
  'http://'$HOSTNAME'/api/user/token/' \
  -H 'accept: application/json' \
  -H 'Content-Type: application/json' \
  -d '{
  "email": "'$DJANGO_NORMAL_USER_EMAIL'",
  "password": "'$DJANGO_NORMAL_USER_PASSWORD'"
}')

#echo "Token response: $token_response"

# Extract the token value from the response
access_token=$(echo "$token_response" | jq -r '.access')

# Check if the token was successfully extracted
if [ "$access_token" != "null" ]; then
    echo -e "${GREEN}Token request successful. Token: $access_token${NC}"
else
    echo -e "${RED}Token request failed${NC}"
    exit 1
fi

echo "Testing an authenticated request, user details"
user_details=$(curl -X 'GET' \
  'http://'$HOSTNAME'/api/user/me/' \
  -H "Authorization: Bearer $access_token" \
  -H 'accept: application/json')
echo "User details: $user_details"

# Read the JSON payload from the file
input_model=$(cat modelPayload.json)

# Prepare the JSON payload
payload=$(jq -n --argjson data "$input_model" '{data: $data}')

# Perform a POST request with the access token and JSON payload
sim_result=$(curl -s -X POST \
    'http://'$HOSTNAME'/api/composite/' \
    -H "Authorization: Bearer $access_token" \
    -H 'accept: application/json' \
    -H 'Content-Type: application/json' \
    -d "$payload")

# echo "Simulation result: $sim_result"
input_results=$(cat resultsPayload.json)

python3 compare_json.py "$input_results" "$sim_result" --tolerance 1e-9
compare_exit_code=$?

if [ $compare_exit_code -eq 0 ]; then
    echo -e "${GREEN}JSON comparison successful${NC}"
else
    echo -e "${RED}JSON comparison failed${NC}"
    exit 1
fi
echo -e "${GREEN}Server IP Tested: $HOSTNAME.${NC}"
echo -e "${GREEN}Runtime Tests OK...Server Ready.${NC}"