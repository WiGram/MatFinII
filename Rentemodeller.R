## Vasicek kortrentemodel ##

## define model parameters
r0 <- 0.03
a <- 0.3
b <- 0.10
sigma <- 0.03

## simulate short rate paths
n <- 3    # MC simulation trials
T <- 20    # total time
m <- 200   # subintervals
dt <- T/m  # difference in time each subinterval

r <- matrix(0,m+1,n)  # matrix to hold short rate paths
r[1,] <- r0

for(j in 1:n){
  for(i in 2:(m+1)){
    dr <- a*(b-r[i-1,j])*dt + sigma*sqrt(dt)*rnorm(1,0,1)
    r[i,j] <- r[i-1,j] + dr
  }
} 

## plot paths
t <- seq(0, T, dt)
rT.expected <- b + (r0-b)*exp(-a*t)
rT.stdev <- sqrt( sigma^2/(2*a)*(1-exp(-2*a*t)))
matplot(t, r[,1:n], type="l", lty=1, main="Vasicek kortrentemodellen", 
        xlab = "Tid", ylab="Rente", ylim = c(-0.06,0.2)) 
abline(h=b, col="green", lty=2)
lines(t, rT.expected, lty=2) 
lines(t, rT.expected + 2*rT.stdev, lty=2) 
lines(t, rT.expected - 2*rT.stdev, lty=2)
points(0,r0)

## define model parameters for plotting yield curves
b <- 0.05
a <- 0.5
sigma <- 0.13
m <- 14

# Jeg får først brug for en række af funktioner, hvor t = 0, T = T
yield_vasicek <- function(r0, a, b, sigma, t){
  B <- (1 / a) * (1 - exp( -a * t ) )
  A <- (B - t) * ( b - sigma^2 / (2 * a^2) ) - (sigma^2 * B^2) / (4 * a)
  y <- B * r0 / t - A / t
  print(y)
}

r0 <- c(0.00, 0.03, 0.06)
n <- length(r0)
yield <- matrix(0, m, n) # matrix to hold yield curves
for(i in 1:n){
  for(t in 1:m){
    yield[t,i] <- yield_vasicek(r0[i], a, b, sigma, t)
  }
}

maturity <- seq(1, m, 1)
matplot(maturity, yield, type="l", xlab = "Time to maturity", ylab = "Yield", 
        main="Yield Curve Shapes", col="black", lty=1, ylim = c(0.00,0.06))
abline(h=b - sigma^2 / (2 * a^2), col="red", lty=2)

## Og så til forward renten ##
f_vasicek <- function(r0, a, b, sigma, t){
  B <- (1 / a) * (1 - exp( -a * t ) )
  (a * b - B * sigma^2/2) * B + r0 * exp(-a*t)
}

r0 <- c(0.01, 0.03, 0.06)
n <- length(r0)
Forward_rate <- matrix(0, m, n) # matrix to hold yield curves
for(i in 1:n){
  for(t in 1:m){
    Forward_rate[t,i] <- f_vasicek(r0[i], a, b, sigma, t)
  }
}

maturity <- seq(1, m, 1)
matplot(maturity, Forward_rate, type="l", col="black", 
        xlab = "Forward Rate", ylab = "Time to Maturity", main="Instantaneous Forward rates", lty=1,
        ylim = c(0.01,0.07))
abline(h=b, col="red", lty=2)


## CIR-modellen ##

## define model parameters
r0 <- 0.03
a <- 0.1
b <- 0.10
sigma <- 0.03

## simulate short rate paths
n <- 2    # MC simulation trials
T <- 20    # total time
m <- 200   # subintervals
dt <- T/m  # difference in time each subinterval



r_cir <- matrix(0,m + 1,n)
r_cir[1,] <- r0

for(j in 1:n){
  for(i in 2:(m+1)){
    dr_cir <- a*(b-r_cir[i-1,j])*dt + sigma*sqrt(r_cir[i-1,j])*sqrt(dt)*rnorm(1,0,1)
    r_cir[i,j] <- r_cir[i-1,j] + dr_cir
  }
}

## plot paths
t <- seq(0, T, dt)
rT_cir.expected <- r0*exp(-a*t) + b*(1-exp(-a*t))
rT_cir.stdev <- sqrt( r0*(sigma^2/a)*(exp(-a*t)-exp(-2*a*t)) + (b*sigma^2/(2*a)) * (1 - exp(-a*t)^2))
matplot(t, r_cir[,1:n], type="l", lty=1, main="CIR kortrentemodellen", xlab = "Time to Maturity", ylab="Rente", ylim = c(0.00,0.13)) 
abline(h=b, col="red", lty=2)
lines(t, rT_cir.expected, lty=2) 
lines(t, rT_cir.expected + 2*rT_cir.stdev, lty=2) 
lines(t, rT_cir.expected - 2*rT_cir.stdev, lty=2)
points(0,r0)
