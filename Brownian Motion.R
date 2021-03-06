#****************************************#
#*                                      *#
#*                                      *#
#*      Simulering af                   *#
#*      stokastiske processer           *#
#*                                      *#
#*                                      *#
#****************************************#


##
##  Forel�sningen tirsdag 31. oktober 2017
##
##  I koden simuleres forskellige DIFFUSIONS processer, dX = alpha(X,t)dt + sigma(X,t)dZ
##  is�r den Geometriske Brownske bev�gelse er interessant 
##  Pr�v at �ndre parametrene alpha og sigma 
##  og se hvilken betydning de har for processen
##


seed <- 3010
N <- 1000*25
dt <- 25/N
time <- seq(dt,25,by=dt)


# Brownsk bev�gelse 
set.seed(seed)
dW <- rnorm(N,sd=sqrt(dt))
W <- cumsum(dW)
plot(time,W,type='l')



alpha <- 0.1
sigma <- 0.6
# Aritmetisk brownsk bev�gelse
X <- alpha*time + sigma*W
plot(time,X,type='l')


#geometrisk brownsk bev�gelse
Z <- rep(1,N)
for(i in 2:N){
  dZ <- alpha*Z[i-1]*dt + sigma*Z[i-1]*dW[i-1]
  Z[i] <- Z[i-1]+dZ
}
plot(time,Z,type='l')


kappa <- 0.5
theta <- 1
sigma <- 0.1
#CIR
Z <- rep(1,N)
for(i in 2:N){
  dZ <- kappa*(theta-Z[i-1])*dt + sigma*sqrt(Z[i-1])*dW[i-1]
  Z[i] <- Z[i-1]+dZ
}
plot(time,W,type='l', col = "red")
lines(time,X,col="blue")
lines(time,Z,col="green")