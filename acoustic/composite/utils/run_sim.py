
import numpy as np
import levesque


def run_sim( model ):

        # Run the model
        model.run()
        # Return the results
        return model.results



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
