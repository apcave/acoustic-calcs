# Fortran-F2Py Project

This project provides a Fortran binary module for acoustic calculations, utilizing the f2py tool to create a Python interface for the Fortran code.

## Project Structure

```
fortran-f2py-project
├── src
│   ├── levesque.f90       # Fortran source code defining the binary module
│   └── __init__.py        # Marks the directory as a Python package
├── setup.py               # Setup script for building the Fortran module
└── README.md              # Documentation for the project
```

## Installation

To install the Fortran binary module, you need to have Python and NumPy installed. You can then build the module using the following command:

```bash
python setup.py build_ext --inplace
```

This command will compile the Fortran code and create a shared library that can be imported in Python.

## Usage

Once the module is built, you can import it in your Python scripts as follows:

```python
from src import levesque
```

You can then use the functions and classes defined in the Fortran code for your acoustic calculations.

## Requirements

- Python (>= 3.6)
- NumPy
- A Fortran compiler (e.g., gfortran)

## Contributing

Contributions to the project are welcome. Please feel free to submit a pull request or open an issue for any enhancements or bug fixes.

## License

This project is licensed under the MIT License. See the LICENSE file for more details.