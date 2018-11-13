#****************************************#
#*                                      *#
#*                                      *#
#*      Simulering af                   *#
#*      stokastiske processer           *#
#*                                      *#
#*                                      *#
#****************************************#


##
##  Forelæsningen tirsdag 31. oktober 2017
##
##  I koden simuleres forskellige DIFFUSIONS processer, dX = alpha(X,t)dt + sigma(X,t)dZ
##  især den Geometriske Brownske bevægelse er interessant 
##  Prøv at ændre parametrene alpha og sigma 
##  og se hvilken betydning de har for processen
##


seed <- 3010
N <- 1000*25
dt <- 25/N
time <- seq(dt,25,by=dt)


# Brownsk bevægelse 
set.seed(seed)
dW <- rnorm(N,sd=sqrt(dt))
W <- cumsum(dW)
plot(time,W,type='l')



alpha <- 0.1
sigma <- 0.6
# Aritmetisk brownsk bevægelse
X <- alpha*time + sigma*W
plot(time,X,type='l')


#geometrisk brownsk bevægelse
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