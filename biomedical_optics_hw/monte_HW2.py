# Chase Anderson
# Biomedical Optics HW1
# monte.py

import random
import numpy as np
import matplotlib.pyplot as plt                     

def new_deflection():
    
    if(g==0):
        theta = np.arccos(2*random.uniform(0,1)-1)
    else:
        theta = np.arccos((1 + g**2 - ((1-g**2)/(1-g+2*g*random.uniform(0,1)))**2)/(2*g))
    
    return theta

def new_trajectory():
    
    if(np.abs(mu_z) > 0.99999):
        mu_x_new = np.sin(theta)*np.cos(phi)
        mu_y_new = np.sin(theta)*np.sin(phi)
        mu_z_new = mu_z * np.cos(theta)/np.abs(mu_z)
    else:
        mu_x_new = np.sin(theta)/np.sqrt(1-mu_z*mu_z)*(mu_x*mu_z*np.cos(phi)-mu_y*np.sin(phi))+mu_x*np.cos(theta)
        mu_y_new = np.sin(theta)/np.sqrt(1-mu_z*mu_z)*(mu_y*mu_z*np.cos(phi)+mu_x*np.sin(phi))+mu_y*np.cos(theta)
        mu_z_new = -np.sin(theta)*np.cos(phi)*np.sqrt(1-mu_z*mu_z)+mu_z*np.cos(theta)
        
    return mu_x_new,mu_y_new,mu_z_new

def monte_carlo_simulation():

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
                transmit = transmit + 1
                loop = False
            elif(z <= 0):
                reflect = reflect + 1
                loop = False
            elif(random.uniform(0, 1) <= ((mu_a)*(mu_z*s))):
                absorb = absorb + 1
                loop = False
            else:
                new_deflection()
                new_trajectory()

n = 100000 # photons

mu_s = 85 # inverse cm
mu_a = 15 # inverse cm

loop = True

x = 0
y = 0
z = 0

mu_x = 0
mu_y = 0
mu_z = 1

mu_x_new = 0
mu_y_new = 0
mu_z_new = 0

theta = 0
phi = 0

s = 0
g = 0
d = 0.03 # cm

absorb = 0
reflect = 0
transmit = 0

for i in range(5):
    
    absorb = 0
    reflect = 0
    transmit = 0
    
    monte_carlo_simulation()
    
    labels = ['ABSORBED','REFLECTED','TRANSMITTED']
    sizes = [absorb, reflect, transmit]
    
    plt.pie(sizes, labels=labels, autopct='%1.1f%%')
    plt.title('Distribution of Photon Behavior in Tissue')
    
    plt.grid()
    plt.show()