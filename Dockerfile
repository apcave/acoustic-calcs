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

RUN apt-get update && \
    apt-get install -y nginx python3-dev gcc && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get autoremove -y

RUN pip install --upgrade pip && \
    pip install -r requirements.txt

RUN apt-get remove -y gcc python3-dev

RUN  adduser --no-create-home django-user


# Copy project
COPY /acoustic/ /acoustic/
RUN chown -R django-user:django-user /acoustic/staticfiles
#USER django-user

# Copy Nginx configuration
COPY ./nginx/default /etc/nginx/sites-available/default
COPY ./nginx/default /etc/nginx/sites-enabled/default

COPY ./scripts/run.sh .



# Expose port 80
EXPOSE 80

# Start Nginx and uWSGI
CMD ["sh", "run.sh"]