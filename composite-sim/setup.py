from numpy.distutils.core import setup, Extension

# Define the Fortran extension
fortran_extension = Extension(
    name='levesque',  # Replace with the desired module name
    sources=['levesque.f90']  # Replace with the path to your Fortran file
)

# Setup script
setup(
    name='levesque',  # Replace with the desired package name
    version='1.0',
    description='A package with Fortran extension',
    ext_modules=[fortran_extension]
)