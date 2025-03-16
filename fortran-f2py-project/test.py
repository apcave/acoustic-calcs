import numpy as np
import levesque
# Call a function from the levesque module
# Replace 'example_function' with the actual function you want to call
levesque.hello_module.example_function()


# Define the inputs
flag = True
real_array = np.array([1.0, 2.0, 3.0], dtype=np.float64)
complex_array = np.array([1.0 + 1.0j, 2.0 + 2.0j, 3.0 + 3.0j], dtype=np.complex128)


# Call the Fortran subroutine
real_result,complex_result,message = levesque.my_module.process_data(flag, real_array, complex_array)

# Print the results
print("Real input:", real_array)
print("Real result:", real_result)
print("Complex input:", complex_array)
print("Complex result:", complex_result)
print("Message:", message.strip())