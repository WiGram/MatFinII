library(ggplot2)

#Black Scholes Model
s     <- c(  100,  100) # stock price
k     <- c(  104,  100) # exercise price
t_0   <- c(    0,    0) # start date
T     <- c(    1, 0.25)   # expiration date
sigma <- c(  0.2,  0.2) # volatility
rfree <- c( 0.05, 0.05) # risk free rate


#Definition af Black-Scholes formlen
BSfunc <- function(S, K, sigm, r, t_0, T){
    d1    <- (log(S/K) + (r + 1/2*sigm^2)*(T-t_0))/(sigm*sqrt(T-t_0))
    d2    <- d1 - sigm*sqrt(T)
    C     <- S*pnorm(d1) - exp(-r*(T-t_0))*K*pnorm(d2)
    delta <- pnorm(d1)
    gamma <- 1/sqrt(2*pi)*exp(-1/2*d1^2)/(S*sigm*sqrt(T-t_0))
    return(c(C, d1, d2, delta, gamma))
}
#Anvendelse
x <- BSfunc(s, k, sigma, rfree, t_0, T)

M_1 <- matrix(x, byrow = T, nrow = 5)
        rownames(M_1) <- c("C", "d1", "d2", "delta", "gamma")
        colnames(M_1) <- paste('Option' , 1:length(M_1[1,]),sep=" ")
M_1

# Ex.2
# I want to create a delta neutral portfolio, containing option 1 and the underlying stock.
#Delta neutral means that the marginal change in the portfolio should be zero.
#Denote by n1 the amount of the option (negative - sell), and nS the amount of the stock.

X_C1 <- -1
X_S1 <- M_1["delta","Option 1"]

c(PF = X_C1*M_1["C","Option 1"] + X_S1*100, X_C1 = X_C1, X_S1 = X_S1)


# Ex.4
# I now want to create a delta - gamma neutral portfolio, containing option 1, 2 and the stock
# Note that you cannot make a delta - gamma neutral portfolio without another option.

# PF = - C1 + n2 C2 + nS S
# D_PF = -delta1 + n2 delta_2 + nS = 0
# G_PF = -gamma1 + n2 gamma2 = 0

# This means, that n2 = gamma1 / gamma2, nS = delta1 - n2 delta2

X_C1 <- -1
X_C2 <- M_1["gamma","Option 1"]/M_1["gamma","Option 2"]
X_S2 <- M_1["delta","Option 1"] - X_C2*M_1["delta","Option 2"]

c(PF = X_C1*M_1["C","Option 1"] + X_C2 * M_1["C","Option 2"] + X_S2*100, X_C1 = X_C1, X_C2 = X_C2, X_S2 = X_S2)

# Ex.5
# Consider the hedging error, when the stock initially is at S = 100 and falls to S = 95

x <- BSfunc(c(100,95, 100, 95),
            c(104, 104, 100, 100),
            c(sigma, sigma), 
            c(rfree, rfree),
            c(t_0, t_0),
            c(1,1,0.25,0.25)
)


M_2 <- matrix(x, byrow = T, nrow = 5)
        rownames(M_2) <- c("C", "d1", "d2", "delta", "gamma")
        colnames(M_2) <- c("Option 1, 100","Option 1, 95", "Option 2, 100", "Option 2, 95")
M_2

#Now take the same positions as in question 2 and 4 respectively:

delta_hedge <- c(X_C1 * M_2["C","Option 1, 100"] + X_S1 * 100, 
                 X_C1 * M_2["C","Option 1, 95"] + X_S1 * 95,
                 X_C1 * M_2["C","Option 1, 95"] + X_S1 * 95
                 - (X_C1 * M_2["C","Option 1, 100"] + X_S1 * 100)
                )
names(delta_hedge) <- c("PF, S = 100","PF, S = 95","Error")
delta_hedge

del_gam_hedge <- c(X_C1 * M_2["C","Option 1, 100"] + X_C2 * M_2["C","Option 2, 100"] + X_S2 * 100, 
                   X_C1 * M_2["C","Option 1, 95"] + X_C2 * M_2["C","Option 2, 95"] + X_S2 * 95,
                   X_C1 * M_2["C","Option 1, 95"] + X_C2 * M_2["C","Option 2, 95"] + X_S2 * 95
                   - (X_C1 * M_2["C","Option 1, 100"] + X_C2 * M_2["C","Option 2, 100"]+ X_S2 * 100)
                  )
names(del_gam_hedge) <- c("PF, S = 100","PF, S = 95","Error")
del_gam_hedge



#Finally: Example from lecture:

#================= Black-Scholes formlen ======================#
#Modellens parametre
s     <- c(  100,  100) # stock price
k     <- c(  105,  100) # exercise price
t_0   <- c(    0,    0) # start date
T     <- c(    1,    1)   # expiration date
sigma <- c(  0.1,  0.1) # volatility
rfree <- c( 0.05, 0.05) # risk free rate


#Definition af Black-Scholes formlen for en call-option
BSfunc <- function(S, K, sigm, r, t_0, T){
  d1    <- (log(S/K) + (r + 1/2*sigm^2)*(T-t_0))/(sigm*sqrt(T-t_0))
  d2    <- d1 - sigm*sqrt(T)
  C     <- S*pnorm(d1) - exp(-r*(T-t_0))*K*pnorm(d2)
  delta <- pnorm(d1)
  gamma <- 1/sqrt(2*pi)*exp(-1/2*d1^2)/(S*sigm*sqrt(T-t_0))
  return(c(C, d1, d2, delta, gamma))
}
#Anvendelse
x <- BSfunc(s, k, sigma, rfree, t_0, T)

M_1 <- matrix(x, byrow = T, nrow = 5)
rownames(M_1) <- c("C", "d1", "d2", "delta", "gamma")
colnames(M_1) <- paste('Option' , 1:length(M_1[1,]),sep=" ")
M_1

# Her vælges nu antal

x_1 <- M_1["gamma","Option 1"]/M_1["gamma","Option 2"]
x_1

x_2 <- M_1["delta","Option 1"] - x_1*M_1["delta","Option 2"]
x_2

Cost <- x_1*M_1["C","Option 2"] + x_2*s[2]
Cost

#Nu skal der udlånes x_b kroner, der koster 1 stykket:
x_b = -Cost

#Total portfolio now costs:
PF <- x_1*M_1["C","Option 2"] + x_2*s[2] + x_b
PF

#where our positions are as follows:
x <- data.frame("Position" = c("Call" = x_1, "Stocks" = x_2, "Bank" = x_b), 
                "Cashflow" = c(-x_1*M_1["C","Option 2"], -x_2*s[2], -x_b))
x
sum(x[,"Cashflow"])