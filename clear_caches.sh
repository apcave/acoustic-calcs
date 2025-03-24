#!/bin/bash

# Remove all __pycache__ directories
find . -type d -name "__pycache__" -exec rm -r {} +

# Remove all .pyc and .pyo files
find . -type f -name "*.pyc" -exec rm -f {} +
find . -type f -name "*.pyo" -exec rm -f {} +