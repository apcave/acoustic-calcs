
import numpy as np
import matplotlib.pyplot as plt
import sys
import os

#sys.path.append(os.path.abspath('.'))

import levesque


def run_acoustic_simulation(is_comp: bool, angle, freq, density, thick, cp, attp, LongM, cs, atts, mu):
    angle = np.array(angle, dtype=np.float64)
    freq = np.array(freq, dtype=np.float64)
    density = np.array(density, dtype=np.float64)
    thick = np.array(thick, dtype=np.float64)
    cp = np.array(cp, dtype=np.float64)
    attp = np.array(attp, dtype=np.float64)
    LongM = np.array(LongM, dtype=np.complex128)
    cs = np.array(cs, dtype=np.float64)
    atts = np.array(atts, dtype=np.float64)
    mu = np.array(mu, dtype=np.complex128)

    # Determine the size of the output arrays
    numCalcs = max(len(angle), len(freq))

    # Allocate the output arrays
    rp = np.empty(numCalcs, dtype=np.complex128)
    tp = np.empty(numCalcs, dtype=np.complex128)
    rs = np.empty(numCalcs, dtype=np.complex128)
    ts = np.empty(numCalcs, dtype=np.complex128)
    RpE = np.empty(numCalcs, dtype=np.float64)
    TpE = np.empty(numCalcs, dtype=np.float64)
    RsE = np.empty(numCalcs, dtype=np.float64)
    TsE = np.empty(numCalcs, dtype=np.float64)

    ret_code, ret_message = levesque.acoustic_sim.simulate_composite(is_comp, angle, freq, density, thick, cp, attp, LongM, cs, atts, mu, rp, tp, rs, ts,RpE, TpE, RsE, TsE)


    print( "Reflection coefficient (P-wave):", rp )
    print( "Transmission coefficient (P-wave):", tp)

    # Convert rp and tp to dB
    rp_db = 20 * np.log10(np.abs(rp))
    tp_db = 20 * np.log10(np.abs(tp))

    if (len(rp) > 10):
        # Plot the results
        plt.figure(figsize=(10, 6))
        plt.plot(freq, np.abs(rp), label='Reflection Coefficient (P-wave) in abs(Rp)')
        plt.plot(freq, np.abs(tp), label='Transmission Coefficient (P-wave) in abs(Tp)')
        plt.xlabel('Frequency (Hz)')
        plt.ylabel('Magnitude (abs)')
        plt.title('Reflection and Transmission Coefficients vs Frequency')
        plt.legend()
        plt.ylim(0,1)
        plt.grid(True)
        plt.show()

    # Print the results

    #print("Reflection coefficient (S-wave):", rs)
    #print("Transmission coefficient (S-wave):", ts)
    #print("Energy of reflected P-wave:", RpE)
    #print("Energy of transmitted P-wave:", TpE)
    #print("Energy of reflected S-wave:", RsE)
    #print("Energy of transmitted S-wave:", TsE)
    print("Return code:", ret_code)
    print("Return message:", ret_message.strip())

def water_steel_water_test():
    # Water steel water.
    eps = 2.2204e-16
    isComp = True
    angle = [40]
    freq = np.linspace(0, 20e3, 101)
    density = [1000, 7850, 1000]
    thick = [0, 0.2, 0]
    cp = [1500, 5960, 1500]
    attp = [eps, eps, eps]
    LongM = [0, 0, 0]
    cs = [eps, 3235, eps]
    atts = [eps, eps, eps]
    mu = [0,0,0]
    run_acoustic_simulation(isComp, angle, freq, density, thick, cp, attp, LongM, cs, atts, mu)

water_steel_water_test()
