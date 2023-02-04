import random
import matplotlib.pyplot as plt

def monte_carlo_simulation(n, mu_a, d, delta_z):
    photons = n
    z_values = []
    absorbed_photons = []
    for z in delta_z:
        absorbed = 0
        for i in range(photons):
            if random.uniform(0, 1) < 1 - (1 - mu_a * z) ** photons:
                absorbed += 1
        photons -= absorbed
        z_values.append(z)
        absorbed_photons.append(absorbed)
    return z_values, absorbed_photons

if __name__ == '__main__':
    n = 10000
    mu_a = 0.05
    d = 0.05
    delta_z = [i * 0.001 for i in range(0, int(d / 0.001) + 1)]
    for i in range(5):
        z_values, absorbed_photons = monte_carlo_simulation(n, mu_a, d, delta_z)
        plt.plot(z_values, absorbed_photons)
    plt.xlabel('z (cm)')
    plt.ylabel('Absorbed Photons')
    plt.show()

    beer_law = [n * (1 - (1 - mu_a * z)) for z in delta_z]
    plt.plot(z_values, beer_law)
    plt.xlabel('z (cm)')
    plt.ylabel('Beer Law')
    plt.show()

    transmitted_photons = [n - absorbed for absorbed in absorbed_photons]
    plt.plot(z_values, transmitted_photons)
    plt.xlabel('z (cm)')
    plt.ylabel('Transmitted Photons')
    plt.show()

    beer_law_transmitted = [n * (1 - mu_a * z) for z in delta_z]
    plt.plot(z_values, beer_law_transmitted)
    plt.xlabel('z (cm)')
    plt.ylabel('Beer Law Transmitted Photons')
    plt.show()