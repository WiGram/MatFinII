
N=252;
r0 = 0.10     #Den rente, der startes i.
dt = 1/252    #
steps = 5*N   #

rvec_vas <-rvec_cir<- r0*rep(1,steps)

a = 0.1;      #Niveau
b = 0.05      #
sigma = 0.15

for(j in 1:(steps-1)){
  rvec_vas[j+1] <- rvec_vas[j] + a*(b-rvec_vas[j])*dt + sqrt(dt)*sigma*rnorm(1)
  rvec_cir[j+1] <- rvec_cir[j] + a*(b-rvec_cir[j])*dt + sqrt(dt)*sqrt(rvec_cir[j])*sigma*rnorm(1)
  
}

t <- seq(dt,5,by=dt)
par(mar=c(3,3,2,2))
plot(t,rvec_vas,  xlab="t", ylab="r(t)",type='l')
legend('topr',title='Renteudvikling i Vasicek',c(paste('a=',a),paste('b=',b), paste('sigma=',sigma)))


plot(t,rvec_cir,  xlab="t", ylab="r(t)")
legend('topl',title='Renteudvikling i CIR',c(paste('a=',a),paste('b=',b), paste('sigma=',sigma)))

# Hvor Vasicek-modellen er 
# dr_t = a(b-r_t) dt + \sigma dW_t^P