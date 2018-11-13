#================= Simulation of Brownian Motion ===============================#

nsim <- 10
t <- 0:1000
sigma2 <- 1

# Først et histogram over x's fordeling og et linjeplot, der følger én bevægelse:
x <- rnorm(n = length(t) - 1, sd = sigma2)
hist(x, breaks = 25, main = "")
x <- c(0, cumsum(x))
plot(t, x, type = "l", ylim = c(-50, 50))

t <- 0:1000
# Dernæst et plot, der følger 10 simuleringer
x <- matrix(rnorm(n = nsim * (length(t)-1),
                  sd = sigma2),
            nsim,
            length(t)-1)

x <- cbind(rep(0, nsim), t(apply(x, 1, cumsum)))
plot(t,x[1,], xlab = "Time", ylab = "", ylim = c(-100,100), type = "l")
apply(x[2:nsim,], 1, function(x,t) lines(t,x), t= t)

#======================== DELTA-call ==============================================#

k <- 40
sigma <- 0.3
r <- 0.08

sseq <- seq(20,60,.01)
d1 <- function(Slut){
  ( log(sseq / k) + (r + (sigma^2/2))*Slut) / (sigma * sqrt(Slut))
} 

phi<-function(d){
  pnorm(d, 0, 1)
}

plot(sseq, phi(d1(3)), col="darkorange", xlab="", ylab="",type="l", ylim = c(0,1), main="Delta on Call-option")
lines(sseq, phi(d1(1)), col ="darkgreen")
lines(sseq, phi(d1(1/4)), col = "darkblue")
legend("topleft",
       legend = c("Tre år","Et år", "3 mdr"),
       fill=c("darkorange","darkgreen","darkblue"))

#======================== DELTA-put ==============================================#

put <- function(Slut){
  phi(Slut) - 1
}

plot(sseq, put(d1(3)), col="darkorange", xlab="", ylab="",type="l", ylim = c(-1,0), main="Delta on Put-option")
lines(sseq, put(d1(1)), col ="darkgreen")
lines(sseq, put(d1(1/4)), col = "darkblue")
legend("topleft",
       legend = c("Tre år","Et år", "3 mdr"),
       fill=c("darkorange","darkgreen","darkblue"))

#======================== gamma ==============================================#

phi_maerke <- function(d){
  1 / sqrt (2 * pi ) * exp ( -1 / 2 *  d^2)
}

x_g <- function(Slut){
  phi_maerke(d1(Slut)) / ( sseq * sigma * sqrt (Slut))
} 

plot(sseq, x_g(3), col="darkorange", xlab="", ylab="",type="l", ylim = c(0,0.08), main="Gamma on Call-option")
lines(sseq, x_g(1), col ="darkgreen")
lines(sseq, x_g(1/4), col = "darkblue")
legend("topleft",
       legend = c("Tre år","Et år", "3 mdr"),
       fill=c("darkorange","darkgreen","darkblue"))

#====================== Theta ======================#

d2 <- function(Slut){
  d1(Slut) - sigma * sqrt(Slut)
}

x_theta <- function(Slut){
  - phi_maerke(d1(Slut)) * sseq * sigma / (2 * sqrt(Slut)) - r * k * exp(-r*Slut) * phi(d2(Slut))
} 

plot(sseq, x_theta(3), col="darkorange", xlab="", ylab="",type="l", ylim = c(-10,0), main="Theta on Call-option")
lines(sseq, x_theta(1), col ="darkgreen")
lines(sseq, x_theta(1/4), col = "darkblue")
legend("bottomleft",
       legend = c("Tre år","Et år", "3 mdr"),
       fill=c("darkorange","darkgreen","darkblue"))

#====================== Theta_put =================#

x_theta_put <- function(Slut){
  - phi_maerke(d1(Slut)) * sseq * sigma / (2 * sqrt(Slut)) + r * k * exp(-r*Slut) * phi(-d2(Slut))
} 

plot(sseq, x_theta_put(3), col="darkorange", xlab="", ylab="",type="l", ylim = c(-5,5), main="Theta on Put-option")
lines(sseq, x_theta_put(1), col ="darkgreen")
lines(sseq, x_theta_put(1/4), col = "darkblue")
legend("bottomleft",
       legend = c("Tre år","Et år", "3 mdr"),
       fill=c("darkorange","darkgreen","darkblue"))



#======================= Rho-call ======================#

x_rho <- function(Slut){
  k * Slut * exp(-r*Slut)*phi(d2(Slut))
}

plot(sseq, x_rho(3), col="darkorange", xlab="", ylab="",type="l", ylim = c(0,80), main="Rho on Call-option")
lines(sseq, x_rho(1), col ="darkgreen")
lines(sseq, x_rho(1/4), col = "darkblue")
legend("topleft",
       legend = c("Tre år","Et år", "3 mdr"),
       fill=c("darkorange","darkgreen","darkblue"))

#======================= Rho-put ======================#

x_rho_put <- function(Slut){
  - x_rho(Slut)
}

plot(sseq, x_rho_put(3), col="darkorange", xlab="", ylab="",type="l", ylim = c(-80,0), main="Rho on Put-option")
lines(sseq, x_rho_put(1), col ="darkgreen")
lines(sseq, x_rho_put(1/4), col = "darkblue")
legend("topleft",
       legend = c("Tre år","Et år", "3 mdr"),
       fill=c("darkorange","darkgreen","darkblue"))

#===================== Vega ======================#

x_vega <- function(Slut){
  sseq * phi_maerke(d1(Slut))*sqrt(Slut)
}

plot(sseq, x_vega(3), col="darkorange", xlab="", ylab="",type="l", ylim = c(0,25), main="Vega on Call-option")
lines(sseq, x_vega(1), col ="darkgreen")
lines(sseq, x_vega(1/4), col = "darkblue")
legend("topleft",
       legend = c("Tre år","Et år", "3 mdr"),
       fill=c("darkorange","darkgreen","darkblue"))
