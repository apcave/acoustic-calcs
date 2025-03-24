# Use the official Python image from the Docker Hub
FROM python:3.11-slim
# AMD64
LABEL maintainer="Alex Cave"

# Set environment variables
ENV PYTHONDONTWRITEBYTECODE=1
ENV PYTHONUNBUFFERED=1

# Set work directory
WORKDIR /acoustic
ENV PATH="/py/bin:$PATH"
# Install dependencies
COPY requirements.txt /acoustic/
COPY ./composite-sim/src/levesque.F90 /acoustic/

RUN adduser --no-create-home --disabled-password django-user && \
    apt-get update && \
    apt-get install -y nginx python3-dev gcc gfortran curl jq lsof netcat-openbsd && \
    rm -rf /var/lib/apt/lists/* && \
    python -m venv /py && \
    pip install --upgrade pip && \
    pip install -r requirements.txt && \
    f2py -c -m levesque levesque.F90 && \
    rm levesque.F90 && \
    apt-get remove -y gcc gfortran python3-dev

# Copy project
COPY /acoustic/ /acoustic/
RUN mkdir /acoustic/staticfiles && \
    chown -R django-user:django-user /acoustic/staticfiles && \
    rm -rf /acoustic/composite/utils/*.so && \
    mv levesque*.so /acoustic/composite/utils/ && \
    ls -lah && \
    ls -lah /acoustic/composite/utils/

# Copy Nginx configuration
COPY ./nginx/default /etc/nginx/sites-available/default
COPY ./nginx/default /etc/nginx/sites-enabled/default

COPY ./scripts/run_inside_docker.sh .

# Expose port 80
EXPOSE 80

# Start Nginx and uWSGI
CMD ["bash", "run_inside_docker.sh"]