from numpy.distutils.core import Extension, setup
# Define the Fortran extension module
fortran_extension = Extension(
    name='levesque',  # Name of the module
    sources=['src/levesque.F90'],  # Source file
    extra_compile_args=['-O3'],  # Optimization flags
)

# Setup function to configure the package
setup(
    name='fortran-f2py-project',  # Package name
    version='0.1',  # Version number
    description='A Fortran module for acoustic calculations',  # Short description
    ext_modules=[fortran_extension],  # List of extension modules
    zip_safe=False,  # Not safe to zip
)