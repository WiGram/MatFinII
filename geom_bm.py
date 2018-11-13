# -*- coding: utf-8 -*-
"""
Created on Wed Jun 27 20:38:33 2018

@author: William
"""

import numpy as np
import os
import pandas as pd
import math
import statsmodels.api as sm
from matplotlib import pyplot as plt
import scipy.stats as scs

np.random.seed(21432)

# Brownian motion

spot = 100.0
vol  = 0.2
mat  = 10
strike = 100
r    = 0.05
mu   = 0.07
q    = 0
nsteps = 100
sims = 8
np.zeros((1,2))

def bm_fct(spot, vol, mat, return_rate, dividend,nsteps, sims):
    dt = 1 / nsteps
    dt_sqrt = np.sqrt(dt)
    rand = np.random.standard_normal((mat * nsteps, sims))
    stock = np.zeros_like(rand)
    stock[0] = spot
    for t in range(1, mat * nsteps):
        stock[t] = stock[t-1] * np.exp((return_rate - dividend - 0.5 * vol**2 ) * dt + \
             vol * dt_sqrt *rand[t])
    return stock

time_to_mat = np.arange(0, mat, 1.0 / nsteps)
asset_price = bm_fct(spot, vol, mat, r, q, nsteps, sims)

sims = 100
mat  = 20
tmat = np.arange(0, mat, 1.0 / nsteps)

r1   = r
r2   = r
r3   = 2*r
r4   = 2*r
v1   = vol
v2   = vol + 0.1
v3   = vol
v4   = vol + 0.1

stock_1 = bm_fct(spot, v1, mat, r1, q, nsteps, sims)
stock_2 = bm_fct(spot, v2, mat, r2, q, nsteps, sims)
stock_3 = bm_fct(spot, v3, mat, r3, q, nsteps, sims)
stock_4 = bm_fct(spot, v4, mat, r4, q, nsteps, sims)


fig, ax = plt.subplots(figsize = (14,5))
ax.plot(time_to_mat, asset_price, label = 'Stock value')
ax.set_ylabel('Value (USD)')
ax.set_xlabel('Time')
ax.set_xlim(xmin = time_to_mat[0], xmax = time_to_mat[-1])
fig.tight_layout()
#fig.savefig('C:/Users/William/Dropbox/KU/K1/MFII/Python/geom_bm.png', bbox_inches='tight')
plt.close(fig)

fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(nrows = 2, ncols = 2, figsize = (14,8))
ax1.plot(tmat, stock_1, label = 'Stock 1')
ax1.set_title('Stock with r = 0.05, v = 0.2')
ax1.set_ylabel('Value (USD)')
ax1.set_xlabel('Time')
ax1.set_xlim(xmin = tmat[0], xmax = tmat[-1])
ax2.plot(tmat, stock_2, label = 'Stock 2')
ax2.set_title('Stock with r = 0.05, v = 0.3')
ax2.set_ylabel('Value (USD)')
ax2.set_xlabel('Time')
ax2.set_xlim(xmin = tmat[0], xmax = tmat[-1])
ax3.plot(tmat, stock_3, label = 'Stock 3')
ax3.set_title('Stock with r = 0.1, v = 0.2')
ax3.set_ylabel('Value (USD)')
ax3.set_xlabel('Time')
ax3.set_xlim(xmin = tmat[0], xmax = tmat[-1])
ax4.plot(tmat, stock_4, label = 'Stock 4')
ax4.set_title('Stock with r = 0.1, v = 0.3')
ax4.set_ylabel('Value (USD)')
ax4.set_xlabel('Time')
ax4.set_xlim(xmin = tmat[0], xmax = tmat[-1])
fig.tight_layout()
#fig.savefig('C:/Users/William/Dropbox/KU/K1/MFII/Python/geom_bm_four.png', bbox_inches='tight')
plt.close(fig)

# ================================================= #
# ===== pg. 38 - single stock path simulation ===== #
# ================================================= #

np.random.seed(250000)
gbm_dates = pd.DatetimeIndex(start = '30-09-2004',
                             end   = '30-09-2014',
                             freq  = 'B')

M = len(gbm_dates) # Amount of time steps
sims = 1
bank_days = 252.
dt = 1/bank_days # Size of time steps
df = np.exp(-r * dt) # discount factor

rand = np.random.standard_normal(size = (M,sims))
S = np.zeros_like(rand)
S[0] = spot
for t in range(1,M):
    S[t] = S[t-1] * np.exp((r - 0.5 * vol ** 2) * dt + vol * rand[t] * np.sqrt(dt))

gbm = pd.DataFrame(S[:, 0], index = gbm_dates, columns = ['index'])
gbm['returns'] = np.log(gbm['index'] / gbm['index'].shift(1))
gbm['rea_var'] = bank_days * np.cumsum(gbm['returns'] ** 2) / np.arange(len(gbm)) # Not demeaned
gbm['rea_vol'] = np.sqrt(gbm['rea_var'])
gbm = gbm.dropna()

def quotes_returns(data, name):
    ''' Plots quotes and returns. '''
    plt.figure(figsize = (14,8))
    plt.subplot(211)
    data['index'].plot()
    plt.ylabel('Daily quotes')
    plt.grid(False)
    plt.axis('tight')
    
    plt.subplot(212)
    data['returns'].plot()
    plt.ylabel('Daily log returns')
    plt.grid(False)
    plt.axis('tight')
    
    plt.savefig(os.path.join('C:/Users/William/Dropbox/KU/K1/MFII/Python/',name), bbox_inches='tight')
    
quotes_returns(gbm, 'index_price')

def dN(x, mu, sigma):
    ''' Probability density function of a normal random variable x'''
    z = (x - mu) / sigma
    pdf = np.exp(-0.5 * z ** 2) / np.sqrt(2 * math.pi * sigma ** 2)
    return pdf

def return_histogram(data, name):
    ''' Plots quotes and returns. '''
    plt.figure(figsize = (14,8))
    x = np.linspace(min(data['returns']), max(data['returns']), 100)
    plt.hist(np.array(data['returns']), bins = 50, normed = True)
    y = dN(x, np.mean(data['returns']), np.std(data['returns']))
    plt.plot(x, y, linewidth = 2)
    plt.xlabel('log returns')
    plt.ylabel('frequency/probability')
    plt.grid(True)
    plt.savefig(os.path.join('C:/Users/William/Dropbox/KU/K1/MFII/Python/',name), bbox_inches='tight')
    
return_histogram(gbm, 'index_returns_hist')

def return_qqplot(data, name):
    ''' Generates a Q-Q plot of the returns.'''
    fig, ax = plt.subplots(figsize = (14,8))
    sm.qqplot(data['returns'], line = 's', ax = ax)
    plt.grid(True)
    plt.xlabel('theoretical quantiles')
    plt.ylabel('sample quantiles')
    plt.savefig(os.path.join('C:/Users/William/Dropbox/KU/K1/MFII/Python/',name), bbox_inches='tight')

return_qqplot(gbm, 'returns_qq_plot')

# Volatility

def realised_volatility(data, name):
    ''' Plots the realised volatility.'''
    plt.figure(figsize = (14,8))
    data['rea_vol'].plot()
    plt.ylabel('realised volatility')
    plt.grid(True)
    plt.savefig(os.path.join('C:/Users/William/Dropbox/KU/K1/MFII/Python/',name), bbox_inches='tight')
realised_volatility(gbm, 'realised_vol')

def rolling_statistics(data, name):
    ''' Calculates and plots rolling statistics (mean, std, correlation).'''
    plt.figure(figsize = (14,8))
    
    plt.subplot(311)
    mr = pd.rolling_mean(data['returns'], 252) * 252
    mr.plot()
    plt.grid(True)
    plt.ylabel('returns (252d)')
    plt.axhline(mr.mean(), color = 'r', ls = 'dashed', lw = 1.5)
    
    plt.subplot(312)
    vo = pd.rolling_std(data['returns'], 252) * np.sqrt(252)
    vo.plot()
    plt.grid(True)
    plt.ylabel('volatility (252d)')
    plt.axhline(vo.mean(), color = 'r', ls = 'dashed', lw = 1.5)
    vx = plt.axis()
    
    plt.subplot(313)
    co = pd.rolling_corr(mr, vo, 252)
    co.plot()
    plt.grid(True)
    plt.ylabel('correlation (252d)')
    cx = plt.axis()
    
    plt.axis([vx[0], vx[1], cx[2], cx[3]]) # xmin, xmax, ymin, ymax
    plt.axhline(co.mean(), color = 'r', ls = 'dashed', lw = 1.5)
    
    plt.savefig(os.path.join('C:/Users/William/Dropbox/KU/K1/MFII/Python/',name), bbox_inches='tight')

rolling_statistics(gbm, 'rolling_stats')



def print_statistics(data):
    print('RETURN SAMPLE STATISTICS')
    print('----------------------------------------------------')
    print('Mean of Daily Log Returns  %9.6f' % np.mean(data['returns']))
    print('Std of Daily Log Returns   %9.6f' % np.std(data['returns']))
    print('Mean of Annua. Log Returns %9.6f' % (np.mean(data['returns'])*252))
    print('Std of Annua. Log Returns  %9.6f' % \
          (np.std(data['returns'])* np.sqrt(252)))
    print('----------------------------------------------------')
    print('Skew of Sample Log Returns %9.6f' % scs.skew(data['returns']))
    print('Skew Normal Test p-value   %9.6f' % scs.skewtest(data['returns'])[1])
    print('----------------------------------------------------')
    print('Kurt of Sample Log Returns %9.6f' % scs.kurtosis(data['returns']))
    print('Kurt Normal Test p-value   %9.6f' % \
          scs.kurtosistest(data['returns'])[1])
    print('----------------------------------------------------')
    print('Normal Test p-value        %9.6f' % \
          scs.normaltest(data['returns'])[1])
    print('----------------------------------------------------')
    print('Realised Volatility        %9.6f' % data['rea_vol'].iloc[-1])
    print('Realised Variance          %9.6f' % data['rea_var'].iloc[-1])
    
print_statistics(gbm)

90 * np.exp(-2)

def ret_fct(spot):
    log_ret = np.repeat(0., len(spot))
    for i in range(1, len(spot)):
        log_ret[i] = np.log(spot[i]/spot[i-1])
    return log_ret

asset_return = ret_fct(asset_price)
sample_mean = sum(asset_return)/len(asset_price)

test3 = np.abs(asset_return - sample_mean)

sample_vol  = np.sqrt(sum(asset_return**2)/len(asset_return))

test_vol = np.sqrt((asset_return[1] - sample_mean)**2)

plt.plot(test3)

count, bins, ignored = plt.hist(asset_return, 30, normed=True)
plt.plot(bins, 1 / (np.sqrt(2 * sample_vol**2 * np.pi)) * np.exp( - (bins - sample_mean)**2 / (2 * sample_vol**2)), linewidth=2, color='r')
plt.show()

scs.probplot(asset_return, dist="norm", plot=plt)
plt.title("Normal Q-Q plot")
plt.show()