#!/bin/bash
set -e

if [ ! -d "./venv" ]; then
    echo "Creating virtual environment"
    python3 -m venv ./venv
    source ./venv/bin/activate
    pip install -r requirements.txt
else
    echo "Activating existing virtual environment"
    source ./venv/bin/activate
fi

echo "Create the python module from the Fortran code...."
cd composite-sim/src
f2py -c -m levesque levesque.F90
mv levesque*.so ../../acoustic/composite/utils/
cd ../../