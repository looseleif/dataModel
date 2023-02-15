# Chase Anderson
# Biomedical Optics HW1
# monte.py

import random
import numpy as np
import matplotlib.pyplot as plt

def monte_carlo_simulation(n, mu_a, d, delta_z):
    
    photons = n
    z_values = []
    absorbed = 0

    for j in range(photons):
        for z in delta_z:
            if random.uniform(0, 1) < ((mu_a)*delta):
                z_values.append(z)
                absorbed+=1
                break
    
    return z_values,absorbed

n = 10000 # photons
mu_a = 5 # inverse cm
d = 0.5 # cm
delta = 0.01 # cm
delta_z = [i * delta for i in range(1, int(d / delta) + 1)] # array of z values

for i in range(5):
    
    z_values,absorb = monte_carlo_simulation(n, mu_a, d, delta_z)
    
    print(str(10000-absorb) + ' transmitted photons!')
    theo = 10000*np.exp(-1*mu_a*d)
    print('With an expected ' + str(np.round(theo)) + ' photons from Beer\'s Law!')
    print('\n')
    plt.hist(z_values,bins=50)
    plt.xlabel('z (cm)')
    plt.ylabel('Absorbed Photons')
    plt.title('Absorbed Photons as a Function of Penetration Depth (with Theoretical)')
    
    x1 = np.linspace(0.01,.5)
    y1 = 10000*np.exp(-1*mu_a*x1)-10000*np.exp(-1*mu_a*(x1+1/100))
    plt.plot(x1, y1, color="red")
    plt.grid()
    plt.show()