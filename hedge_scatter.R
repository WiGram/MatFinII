#library('ggplot2') This will only be required, if latter data is inserted into a dataframe.

#source("hedge_scatter.R")

BlackScholesFormula  <- function (S, tmat, K ,r, d=0, sigma, opttype=1, greektype=1){

  d1 <- ( log(S/K) + ((r-d)+0.5*sigma^2) * tmat )/( sigma*sqrt(tmat) )
  d2 <- d1 - sigma * sqrt(tmat)
  
  if (opttype==1 && greektype==1) 
    result <- S * exp(-d*tmat)*pnorm(d1)-K * exp(-r*tmat) * pnorm(d2)
  
  if (opttype==2 && greektype==1) 
    result <- S * exp(-d*tmat)*pnorm(d1)-K * exp(-r*tmat) * pnorm(d2) - 
                S * exp(-q*tmat) + K * exp(-r*tmat)
  
  if (opttype==1 && greektype==2) 
    result <- exp(-d*tmat) * pnorm(d1)
  
  if (opttype==2 && greektype==2) 
    result <- exp(-d*tmat) * (pnorm(d1)-1)
  
  if (greektype==3) 
    result <- exp(-d*tmat) * dnorm(d1) / (S * sigma * sqrt(tmat) )
  
  if (greektype==4) 
    result <- exp(-d*tmat) * S * dnorm(d1) * sqrt(tmat)
  
  BlackScholesFormula <- result
}

BlackScholesImpVol  <- function (obsprice, S, tmat, K, r, d=0, opttype=1){

  difference <- function(sigBS, obsprice, S, tmat, K, r, d, opttype){
    BlackScholesFormula (S, tmat, K, r, d, sigBS, opttype, 1) - obsprice
  }
  
  uniroot(difference, c(10^-6,10), obsprice = obsprice, S = S, tmat = tmat, K = K,
          r = r, q = q, opttype = opttype)$root
}

# INITIALIZE
# ==========

S0          <- 100  # starting spot price
r           <- 0.05 # constant interest rate
mu          <- 0.1  # drift
sigma       <- 0.2  # volatility
sigma_hedge <- 0.2  # hedge volatility

capT   <- 1         # Specific time to maturity
strike <- 105       # Agreed strike price

Nhedge <- 12        # 12 = monthly, 250 ~ daily
Nrep   <- 1000      # Simulations

# HEDGE
# =====

St <- rep(S0, length = Nrep) # time series of spot prices
dt <- capT / Nhedge          # Average time interval

#initialoutlay<-BlackScholesFormula(S0,capT,strike, r,0,sigma_hedge,1,1)
initialoutlay <- BlackScholesFormula(S0, capT, strike, r, 0, sigma, 1, 1) # with (1,1) this returns the initial call price.

Vpf <- rep(initialoutlay,length=Nrep) # Value of portfolio

a  <- BlackScholesFormula(St, capT, strike, r, 0, sigma_hedge, 1, 2) # delta = amount placed in stock ( with (1,2) delta is returned )
b  <- Vpf - a * St                                                       # amount placed in bank - self-financing pf.

# Each value in each time series is now being updated 
for(i in 2:Nhedge){
  St  <- St * exp( (mu-0.5*sigma^2)*dt + sigma * sqrt(dt) * rnorm(Nrep))     # St was a (1x1000) vector of 100's. Each value is now adjusted
  Vpf <- a * St + b * exp(dt*r)                                              # The portfolio performs based on historical positions
  a   <- BlackScholesFormula(St,(capT-(i-1)*dt),strike, r,0,sigma_hedge,1,2) # New position in stock is taken based on new stock price
  b   <- (Vpf-a*St)                                                          # New position in bank is taken based on new value of pf and position in stock
}

St           <- St * exp((mu-0.5*sigma^2)*dt + sigma*sqrt(dt)*rnorm(Nrep))
Vpf          <- a*St + b*exp(dt*r) 
hedgeerror   <- (Vpf-pmax(St-strike,0))
optionpayoff <- pmax(St-strike,0)

# SUMMARY STATS & GRAPHS
# ======================

t <- seq(0,999)                     # Amateur plotting a time index
plot(x = t, y = St)                 # Stock price as a function of time

# The actual plot used in the lecture
plot(St, Vpf, col = 'blue', xlab = 'S(T)', ylab = 'Value of hedge portfolio',
     ylim = c(-5,105), xlim = c(50, 200), main = 'Discrete hedging of a call-option')
text(50,100, paste('# hedge points =', Nhedge), adj = 0) # (50,100) is the position of the text
points(50:200, pmax(50:200 - strike,0), type = 'l', lwd = 3)

# Ctrl + Shift + C for un/commenting the below section
# pdf("Scatter.pdf")
# 
# print(paste("Initial investment =",round(initialoutlay,4)))
# print(paste("Average discounted option payoff =",round(exp(-r*capT)*mean(optionpayoff),4)))
# print(paste("Average discounted portfolio value =",round(exp(-r*capT)*mean(Vpf),4)))
# 
# plot(St,Vpf,col="blue",xlab="S(T)",ylab="Value of hedge portfolio",ylim=c(-5,105),xlim=c(50,200),main="Discrete hedging of a call-option")
# text(50,100,paste("# hegde points =",Nhedge),adj=0)
# points(50:200,pmax(50:200 - strike,0),type='l',lwd=3)
# 
# dev.off()