# Chase Anderson
# Biomedical Optics HW3
# monte_HW3.py

import random
import numpy as np
import matplotlib.pyplot as plt                     

def new_deflection(gee):
    
    if(gee==0):
        t = np.arccos(2*random.uniform(0,1)-1)
    else:
        t = np.arccos((1 + gee**2 - ((1-gee**2)/(1-gee+2*gee*random.uniform(0,1)))**2)/(2*gee))
    
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
    
    w = 1
    
    n = 100000 # photons
    
    n1 = 1
    n2 = 1.5

    mu_a = 10 # inverse cm
    mu_s = 90 # inverse cm

    theta = 0.0
    phi = 0.0

    s = 0.0
    g = 0.0
    d = 0.20 # cm
    m = 10 # roulette unit
    
    for j in range(n):
        
        loop = True

        w = 1

        x = 0
        y = 0
        z = 0

        mu_x = 0
        mu_y = 0
        mu_z = 1
        
        Rsp = ((n1-n2)**2)/((n1+n2)**2)
        
        r += Rsp
        w = (1-Rsp)
         
        while(loop):
            
            s = -1*np.log(random.uniform(0,1))/(mu_a+mu_s)
            
            x = x + mu_x*s
            y = y + mu_y*s
            z = z + mu_z*s
            
            if(z >= d):
                
                theta_i = np.arccos(np.abs(mu_z))
                theta_t = np.arcsin((n1/n2)*np.sin(theta_i))
                internal = (1/2)*(((np.sin(theta_i-theta_t)**2)/(np.sin(theta_i+theta_t)**2))+((np.tan(theta_i-theta_t)**2)/(np.tan(theta_i+theta_t)**2)))

                
                t = t + (1-internal)*w
                w = internal*w
                                
            elif(z <= 0):
                
                theta_i = np.pi-np.arccos(np.abs(mu_z))
                theta_t = np.arcsin((n1/n2)*np.sin(theta_i))
                internal = (1/2)*(((np.sin(theta_i-theta_t)**2)/(np.sin(theta_i+theta_t)**2))+((np.tan(theta_i-theta_t)**2)/(np.tan(theta_i+theta_t)**2)))
                            
                r = r + (1-internal)*w
                w = internal*w
                
            else:
                a = a + w*(mu_a/(mu_a+mu_s))
                w = w*(mu_s/(mu_a+mu_s))
                
            if(w<0.001):
                
                if(random.uniform(0,1)<(1/m)):
                    loop = False
                else:
                    w = m*w
            
            theta = new_deflection(g)
            phi = 2*3.1459*random.uniform(0,1)
            mu_x, mu_y, mu_z = new_trajectory(theta, phi, mu_x, mu_y, mu_z)
    
    return a, r, t

absorb = 0
reflect = 0
transmit = 0

for i in range(5):
    
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