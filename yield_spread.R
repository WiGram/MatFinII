# A simple programme to graph the yield spread
# Throughout I assume t_0 = 0.

library(ggplot2)
library(grid)

#======================= Introducing formulas ==========================#

# First the BS-formula for the call-option:
BScall <- function(V,D,Tm,sig,r){
  d1 <- (log(V/D) + ( r + sig^2/2 ) *Tm ) / ( sig * sqrt(Tm))
  d2 <- d1 - sig * sqrt(Tm)
  price <- V * pnorm(d1) - D * exp(-r*Tm) * pnorm(d2)
  price
}

# Put-price using put-call parity
BSput <- function(V,D,Tm,sig,r){
  BScall(V,D,Tm,sig,r) - V + D * exp(-r* Tm)
}

#======================= Specifying values ============================#

Tm <- seq(1, 10, 1)
B <- matrix(1, length(Tm), 4)
y <- matrix(1, length(Tm), 4)
S <- matrix(1, length(Tm), 4)

V <- c(90,120,150,200)
D <- 100
sig <- 0.2
r <- 0.05

#------------ Iterate to specify at each time to maturity -------------#
for (j in 1:4){
  for (i in 1:10){
    S[i,j] <- BScall(V[j],D,Tm[i],sig,r)
    B[i,j] <- D * exp(-r*Tm[i]) - BSput(V[j],D,Tm[i],sig,r)
    y[i,j] <- 1/Tm[i] * log(D / B[i,j])
  }
}

yield_spread <- y - r

#------------ Simple test of the values --------------------#
#png('stock_value.png',width = 720, height = 480)
ggplot(NULL, aes(x = Tm, axis)) +
  geom_line(aes(y = S[,3], colour = 'V = 150')) +
  geom_line(aes(y = S[,4], colour = 'V = 200')) +
  xlab('Time to maturity') + ylab('Stock value') +
  ggtitle("Risk structure on Stocks") +
  theme_classic() +
  theme(axis.text.x  = element_text(colour="grey20",size=20,angle=45,hjust=.5,vjust=.5,face="plain"),
        axis.text.y  = element_text(colour="grey20",size=20,angle=0, hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=20,angle=0, hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
        legend.text  = element_text(colour='grey20',size=20,angle=0, hjust=.5,vjust=.5,face='plain'),
        legend.title = element_text(colour='grey20',size=20,angle=0, hjust=.5,vjust=.5,face='plain'),
        plot.title   = element_text(colour='grey20',size=20,angle=0, hjust=.5,vjust=.5,face='plain'))
#dev.off()

#png('bond_value.png',width = 720, height = 480)
ggplot(NULL, aes(x = Tm)) +
  geom_line(aes(y = B[,3], colour = 'V = 150')) +
  geom_line(aes(y = B[,4], colour = 'V = 200')) +
  xlab('Time to maturity') + ylab('Bond value') +
  ggtitle("Risk structure on Bonds") +
  theme_classic() +
  theme(axis.text.x  = element_text(colour="grey20",size=20,angle=45,hjust=.5,vjust=.5,face="plain"),
        axis.text.y  = element_text(colour="grey20",size=20,angle=0, hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=20,angle=0, hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
        legend.text  = element_text(colour='grey20',size=20,angle=0, hjust=.5,vjust=.5,face='plain'),
        legend.title = element_text(colour='grey20',size=20,angle=0, hjust=.5,vjust=.5,face='plain'),
        plot.title   = element_text(colour='grey20',size=20,angle=0, hjust=.5,vjust=.5,face='plain'))
#dev.off()

#png('yield_spread.png',width = 720, height = 480)
ggplot(NULL, aes(x = Tm), title = 'Test') +
  geom_line(aes(y = yield_spread[,3], colour = 'V = 150')) +
  geom_line(aes(y = yield_spread[,4], colour = 'V = 200')) +
  xlab('Time to maturity') + ylab('Yield spread') +
  ggtitle("Risk structure of interest rates") +
  theme_classic() +
  theme(axis.text.x  = element_text(colour="grey20",size=20,angle=45,hjust=.5,vjust=.5,face="plain"),
        axis.text.y  = element_text(colour="grey20",size=20,angle=0, hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=20,angle=0, hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
        legend.text  = element_text(colour='grey20',size=20,angle=0, hjust=.5,vjust=.5,face='plain'),
        legend.title = element_text(colour='grey20',size=20,angle=0, hjust=.5,vjust=.5,face='plain'),
        plot.title   = element_text(colour='grey20',size=20,angle=0, hjust=.5,vjust=.5,face='plain'))
#dev.off()

#png('yield_spread_dec.png',width = 720, height = 480)
ggplot(NULL, aes(x = Tm), title = 'Test') +
  geom_line(aes(y = yield_spread[,1], colour = 'V = 90')) +
  geom_line(aes(y = yield_spread[,2], colour = 'V = 120')) +
  xlab('Time to maturity') + ylab('Yield spread') +
  ggtitle("Risk structure of interest rates") +
  theme_classic() +
  theme(axis.text.x  = element_text(colour="grey20",size=20,angle=45,hjust=.5,vjust=.5,face="plain"),
        axis.text.y  = element_text(colour="grey20",size=20,angle=0, hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="grey20",size=20,angle=0, hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(colour="grey20",size=20,angle=90,hjust=.5,vjust=.5,face="plain"),
        legend.text  = element_text(colour='grey20',size=20,angle=0, hjust=.5,vjust=.5,face='plain'),
        legend.title = element_text(colour='grey20',size=20,angle=0, hjust=.5,vjust=.5,face='plain'),
        plot.title   = element_text(colour='grey20',size=20,angle=0, hjust=.5,vjust=.5,face='plain'))
#dev.off()

