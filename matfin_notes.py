# -*- coding: utf-8 -*-
"""
Created on Wed Jun 27 15:47:45 2018

@author: William
"""

import numpy
from matplotlib import pyplot as plt
import sys
sys.path.append('C:/Users/William/Dropbox/KU/K1/MFII/Python/')
import brownian as br

# The Wiener process parameter.
delta = 2
# Total time.
T = 10.0
# Number of steps.
N = 500
# Time step size
dt = T/N
# Number of realizations to generate.
m = 8
# Create an empty array to store the realizations.
x = numpy.empty((m,N+1))
# Initial values of x.
x[:, 0] = 0

br.brownian(x[:,0], N, dt, delta, out=x[:,1:])

t = numpy.linspace(0.0, N*dt, N+1)

fig, ax = plt.subplots(figsize = (15,6))

ax.plot(t.T,x.T)
fig.tight_layout()
ax.set_ylabel('')
ax.set_xlabel('Time')
ax.set_xlim(xmin = t[0], xmax = t[-1])
fig.tight_layout()
fig.savefig('C:/Users/William/Dropbox/KU/K1/MFII/Python/bm_reg.png', bbox_inches='tight')
plt.close(fig)