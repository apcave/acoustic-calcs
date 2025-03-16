from numpy.distutils.core import Extension, setup

# Define the extension module
levesque_module = Extension(
    name='levesque',
    sources=['./levesque.f90']
)

# Set up the package
setup(
    name='levesque',
    version='1.0',
    description='Fortran module compiled with f2py',
    ext_modules=[levesque_module]
)