# Chase Anderson
# Biomedical Optics HW2
# monte_HW2.py

import random
import numpy as np
import matplotlib.pyplot as plt                     

def new_deflection(gee):
    
    if(gee==0):
        t = np.arccos(2*random.uniform(0,1)-1)
    else:
        t = np.arccos((1 + gee**2 - ((1-gee**2)/(1-gee+2*gee*random.uniform(0,1)))**2)/(2*g))
    
    return t

def new_trajectory(t, p, mux, muy, muz):
    
    if(np.abs(muz) > 0.99999):
        mu_x_new = np.sin(t)*np.cos(p)
        mu_y_new = np.sin(t)*np.sin(p)
        mu_z_new = muz * np.cos(t)/np.abs(muz)
    else:
        mu_x_new = np.sin(t)/np.sqrt(1-muz*muz)*(mux*muz*np.cos(p)-muy*np.sin(p))+mux*np.cos(t)
        mu_y_new = np.sin(t)/np.sqrt(1-muz*muz)*(muy*muz*np.cos(p)+mux*np.sin(p))+muy*np.cos(t)
        mu_z_new = -np.sin(t)*np.cos(p)*np.sqrt(1-muz*muz)+muz*np.cos(t)
        
    return mu_x_new,mu_y_new,mu_z_new

def monte_carlo_simulation():

    a = 0
    r = 0
    t = 0
    
    n = 100000 # photons

    mu_a = 15 # inverse cm
    mu_s = 85 # inverse cm

    theta = 0
    phi = 0

    s = 0
    g = 0
    d = 0.03 # cm
    
    for j in range(n):
        
        loop = True

        x = 0
        y = 0
        z = 0

        mu_x = 0
        mu_y = 0
        mu_z = 1
         
        while(loop):
            
            s = -1*np.log(random.uniform(0,1))/(mu_a+mu_s)
            
            x = x + mu_x*s
            y = y + mu_y*s
            z = z + mu_z*s
            
            if(z >= d):
                t = t + 1
                loop = False
            elif(z <= 0):
                r = r + 1
                loop = False
            elif(random.uniform(0, 1) <= ((mu_a)*s)):
                a = a + 1
                loop = False
            else:
                theta = new_deflection(g)
                mu_x, mu_y, mu_z = new_trajectory(theta, phi, mu_x, mu_y, mu_z)
    
    return a, r, t

absorb = 0
reflect = 0
transmit = 0

for i in range(1):
    
    absorb = 0
    reflect = 0
    transmit = 0
    
    absorb, reflect, transmit = monte_carlo_simulation()
    
    print("absorb: " + str(absorb) + " | reflect: " + str(reflect) + " | transmit: " + str(transmit) + " | TOTAL: " + str(absorb+reflect+transmit));
    
    labels = ['ABSORBED','REFLECTED','TRANSMITTED']
    sizes = [absorb, reflect, transmit]
    
    plt.pie(sizes, labels=labels, autopct='%1.1f%%')
    plt.title('Distribution of Photon Behavior in Tissue')
    
    plt.grid()
    plt.show()