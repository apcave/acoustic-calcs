# Use the official Python image from the Docker Hub
FROM python:3.11-slim
# AMD64
LABEL maintainer="Alex Cave"

# Set environment variables
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# Set work directory
WORKDIR /acoustic

# Install dependencies
COPY requirements.txt /acoustic/
RUN apt-get update && apt-get install -y libpq-dev gcc nginx && rm -rf /var/lib/apt/lists/* \
    && pip install --upgrade pip \
    && pip install -r requirements.txt \
    && pip install uwsgi \
    && apt-get remove -y gcc && apt-get autoremove -y

# Copy project
COPY . /acoustic/

# Copy Nginx configuration
COPY ./nginx/default /etc/nginx/sites-available/default
RUN ln -s /etc/nginx/sites-available/default /etc/nginx/sites-enabled



# Expose port 80
EXPOSE 80

# Start Nginx and uWSGI
CMD ["sh", "/scripts/run.sh"]