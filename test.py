import numpy as np
import levesque





# Water steel water.
isComp = True
angle = 0
freq = 20e3
density = np.array([1000, 7850, 1000], dtype=np.float64)
thick = np.array([0, 5, 0], dtype=np.float64)  # Add validation for thickness
cp = np.array([1500, 5960, 1500], dtype=np.float64)
attp = np.array([0, 0, 0], dtype=np.float64)
LongM = np.array([0 + 0j, 0 + 0j, 0 + 0j], dtype=np.complex128)
cs = np.array([0, 3235, 0], dtype=np.float64)
atts = np.array([0, 0, 0], dtype=np.float64)
mu = np.array([0 + 0j, 0 + 0j, 0 + 0j], dtype=np.complex128)

rp = np.array([0 + 0j, 0 + 0j, 0 + 0j], dtype=np.complex128)
tp = np.array([0 + 0j, 0 + 0j, 0 + 0j], dtype=np.complex128)
rs = np.array([0 + 0j, 0 + 0j, 0 + 0j], dtype=np.complex128)
ts = np.array([0 + 0j, 0 + 0j, 0 + 0j], dtype=np.complex128)
RpE = np.array([0, 0, 0], dtype=np.float64)
TpE = np.array([0, 0, 0], dtype=np.float64)
RsE = np.array([0, 0, 0], dtype=np.float64)
TsE = np.array([0, 0, 0], dtype=np.float64)


ret_code, ret_message = levesque.acoustic_sim.simulate_composite(isComp, angle, freq, density, thick, cp, attp, LongM, cs, atts, mu, rp, tp, rs, ts,RpE, TpE, RsE, TsE)

# Print the results
print("Reflection coefficient (P-wave):", rp)
print("Transmission coefficient (P-wave):", tp)
print("Reflection coefficient (S-wave):", rs)
print("Transmission coefficient (S-wave):", ts)
print("Energy of reflected P-wave:", RpE)
print("Energy of transmitted P-wave:", TpE)
print("Energy of reflected S-wave:", RsE)
print("Energy of transmitted S-wave:", TsE)
print("Return code:", ret_code)
print("Return message:", ret_message.strip())