
import sys
import os
import numpy as np

# Add the directory containing the .so file to the system path
current_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(current_dir)

import levesque  # noqa: E402


def run_simulation(model):
    """Call the simulation with a python object."""

    # print("Running the model...")
    # print("Model input data:", model)
    if (len(model['sweep']['values']) != model['sweep']['numSteps']):
        raise ValueError("Number of steps doesn't match the number of values")

    is_comp = bool(model['incidentCompression'])
    if model['sweep']['isFrequency']:
        print("Frequency sweep")
        freq = model['sweep']['values']
        angle = [model['sweep']['angle']]
    else:
        print("Angle sweep")
        freq = [model['sweep']['frequency']]
        angle = model['sweep']['values']

    density = []
    thick = []
    cp = []
    attp = []
    LongM = []
    cs = []
    atts = []
    mu = []
    layers = model['composite']['layers']
    for lay in layers:
        # print("Layer:", lay)
        mat = lay['material']

        density.append(mat['density'])
        if len(density) == 1 or len(layers) == len(density):
            thick.append(0)
        else:
            thick.append(lay['thickness'])

        comp = mat['compression']
        if comp['type'] == 'vacumm':
            cp.append(2.2204e-16)
            attp.append(2.2204e-16)
            LongM.append(0)

        if comp['type'] == 'wave':
            if comp['waveSpeed'] == 0:
                cp.append(2.2204e-16)
            else:
                cp.append(comp['waveSpeed'])
            if comp['attenuation'] == 0:
                attp.append(2.2204e-16)
            else:
                attp.append(comp['attenuation'])
            LongM.append(0)

        if comp['type'] == 'modulus':
            cp.append(0)
            attp.append(0)
            LongM.append(np.complex128(comp['real'], comp['imag']))

        shear = mat['shear']
        if shear['type'] == 'fluid' or shear['type'] == 'vacumm':
            cs.append(2.2204e-16)
            atts.append(2.2204e-16)
            mu.append(0)

        if shear['type'] == 'wave':
            if shear['waveSpeed'] == 0:
                cs.append(2.2204e-16)
            else:
                cs.append(shear['waveSpeed'])

            if shear['attenuation'] == 0:
                atts.append(2.2204e-16)
            else:
                atts.append(shear['attenuation'])
            mu.append(0)

        if shear['type'] == 'modulus':
            cs.append(0)
            atts.append(0)
            mu.append(np.complex128(shear['real'], shear['imag']))

    ret = run_acoustic_simulation(is_comp, angle, freq, density, thick,
                                  cp, attp, LongM, cs, atts, mu)

    # Filter out the NaN values
    mask = (np.isnan(ret['Tp']['real']) | np.isnan(ret['Tp']['imag']) |
            np.isnan(ret['Rp']['real']) | np.isnan(ret['Rp']['imag']) |
            np.isnan(ret['Ts']['real']) | np.isnan(ret['Ts']['imag']) |
            np.isnan(ret['Rs']['real']) | np.isnan(ret['Rs']['imag']) |
            np.isnan(ret['TpE']) | np.isnan(ret['RpE']) |
            np.isnan(ret['TsE']) | np.isnan(ret['RsE']))

    mask = ~mask
    MaxIndex = len(mask)
    ret['Tp']['real'] = [ret['Tp']['real'][i]
                         for i in range(MaxIndex) if mask[i]]
    ret['Tp']['imag'] = [ret['Tp']['imag'][i]
                         for i in range(MaxIndex) if mask[i]]
    ret['Rp']['real'] = [ret['Rp']['real'][i]
                         for i in range(MaxIndex) if mask[i]]
    ret['Rp']['imag'] = [ret['Rp']['imag'][i]
                         for i in range(MaxIndex) if mask[i]]
    ret['Ts']['real'] = [ret['Ts']['real'][i]
                         for i in range(MaxIndex) if mask[i]]
    ret['Ts']['imag'] = [ret['Ts']['imag'][i]
                         for i in range(MaxIndex) if mask[i]]
    ret['Rs']['real'] = [ret['Rs']['real'][i]
                         for i in range(MaxIndex) if mask[i]]
    ret['Rs']['imag'] = [ret['Rs']['imag'][i]
                         for i in range(MaxIndex) if mask[i]]
    ret['TpE'] = [ret['TpE'][i] for i in range(MaxIndex) if mask[i]]
    ret['RpE'] = [ret['RpE'][i] for i in range(MaxIndex) if mask[i]]
    ret['TsE'] = [ret['TsE'][i] for i in range(MaxIndex) if mask[i]]
    ret['RsE'] = [ret['RsE'][i] for i in range(MaxIndex) if mask[i]]

    model['sweep']['values'] = [model['sweep']['values'][i]
                                for i in range(MaxIndex) if mask[i]]

    model['results'] = ret
    # Return the results
    return model


def complex_array_to_simple(complex_array):
    """Convert a complex array to a simple array."""
    res = dict()
    res["real"] = [float(x.real) for x in complex_array]
    res["imag"] = [float(x.imag) for x in complex_array]
    return res


def run_acoustic_simulation(is_comp: bool, angle, freq, density, thick,
                            cp, attp, LongM, cs, atts, mu):
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

    ret_code, ret_message = levesque.acoustic_sim.simulate_composite(
                                                is_comp,
                                                angle, freq, density, thick,
                                                cp, attp, LongM, cs, atts, mu,
                                                rp, tp, rs, ts, RpE, TpE, RsE,
                                                TsE)

    ret = dict()
    ret['return_code'] = int(ret_code)
    ret['return_message'] = str(ret_message)
    ret['Tp'] = complex_array_to_simple(tp)
    ret['Ts'] = complex_array_to_simple(ts)
    ret['TpE'] = [float(x) for x in TpE]
    ret['TsE'] = [float(x) for x in TsE]
    ret['Rp'] = complex_array_to_simple(rp)
    ret['Rs'] = complex_array_to_simple(rs)
    ret['RpE'] = [float(x) for x in RpE]
    ret['RsE'] = [float(x) for x in RsE]

    # Print the results
    # print("Reflection coefficient (S-wave):", rs)
    # print("Transmission coefficient (S-wave):", ts)
    # print("Energy of reflected P-wave:", RpE)
    # print("Energy of transmitted P-wave:", TpE)
    # print("Energy of reflected S-wave:", RsE)
    # print("Energy of transmitted S-wave:", TsE)
    # print("Return code:", ret_code)
    # print("Return message:", ret_message.strip())
    return ret
