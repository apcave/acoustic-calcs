import numpy as np
import acoustic_calcs

# Define input parameters
Input = 1
cosinc = 0.5
omega = 1.0
Nthe = 10
MediumIndex = 1

# Prepare output variables
rp = np.complex128(0)
tp = np.complex128(0)
rs = np.complex128(0)
ts = np.complex128(0)
RpE = np.float64(0)
RsE = np.float64(0)
TpE = np.float64(0)
TsE = np.float64(0)

# Call the Fortran subroutine
acoustic_calcs.response(Input, cosinc, omega, Nthe, MediumIndex, rp, tp, rs, ts, RpE, TpE, RsE, TsE)

# Print the results
print("rp:", rp)
print("tp:", tp)
print("rs:", rs)
print("ts:", ts)
print("RpE:", RpE)
print("RsE:", RsE)
print("TpE:", TpE)
print("TsE:", TsE)