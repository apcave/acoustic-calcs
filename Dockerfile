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

RUN adduser --no-create-home --disabled-password django-user && \
    apt-get update && \
    apt-get install -y nginx python3-dev gcc gfortran curl jq lsof && \
    rm -rf /var/lib/apt/lists/* && \
    apt-get autoremove -y

    
RUN python -m venv /py && \
    pip install --upgrade pip && \
    pip install -r requirements.txt

# Compile the Physics model
RUN mkdir ./tmp-lp 
COPY ./composite-sim/src/levesque.F90 ./tmp-lp/
RUN cd ./tmp-lp && \
    f2py -c -m levesque levesque.F90

RUN apt-get remove -y gfortran gcc python3-dev

# Copy project
COPY /acoustic/ /acoustic/
RUN chown -R django-user:django-user /acoustic/staticfiles && \
    mv /acoustic/tmp-lp/levesque*.so /acoustic/composite/utils/ && \
    rm -rf /acoustic/tmp-lp
#

# Copy Nginx configuration
COPY ./nginx/default /etc/nginx/sites-available/default
COPY ./nginx/default /etc/nginx/sites-enabled/default

COPY ./scripts/run.sh .



# Expose port 80
EXPOSE 80

# Start Nginx and uWSGI
CMD ["sh", "run.sh"]