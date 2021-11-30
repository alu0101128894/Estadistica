setwd("M:/RTRABAJO/INF")     # establece el directorio de trabajo
load("HIPER200.RData")
attach(HIPER200)
#============================================================
# (a) ESTIMACI�N PUNTUAL: ESTIMADORES DE M�XIMA VEROSIMILITUD
#============================================================
# Caso de una distribuci�n Normal
set.seed(1496)
x<-rnorm(100,5,2)
print(x)

#no va a funcionar, �sabes el motivo?
hist(x)
curve(dnorm(x,5,2), add=T, col='red', lty=1, lwd=3)

#s� va a funcionar
hist(x, freq=FALSE,  col='light blue', ylab='f. de densidad - f(x)',main='Distribuci??n Normal',cex.main=1.0)
curve(dnorm(x,5,2), add=T, col='red', lty=1, lwd=3)

library(MASS)
fitdistr(x,"normal")
mean(x)
sd(x)

# Caso de una distribuci�n exponencial (lambda=[rate])
set.seed(1496)
x<-rexp(100,rate=0.5)
print(x)
hist(x, freq=FALSE,  col='light blue', ylab='f. de densidad - f(x)',main='Distribuci�n Exponencial',cex.main=1.0, ylim=c(0,0.5))
curve(dexp(x,0.5), add=T, col='red', lty=1, lwd=3)

fitdistr(x,"exponential")
mean(x)
sd(x)

# Caso de una distribuci�n Gamma(shape=p=4,lambda=2=scale=[1/rate])
set.seed(1496)
x<-rgamma(100,shape=4,scale=2)  # rgamma(100,shape=4,rate=0.5)
print(x)
hist(x, freq=FALSE,  col='light blue', ylab='f. de densidad - f(x)',main='Distribuci??n Gamma',cex.main=1.0)
curve(dgamma(x,4,0.5), add=T, col='red', lty=1, lwd=3)

fitdistr(x,"gamma")
mean(x)
sd(x)

# Caso de una distribuci�n Geom�trica
set.seed(1496)
x<-rgeom(1000,0.3)
print(x)

fitdistr(x,"geometric")
mean(x)
sd(x)

# Caso de una distribuci�n Binomial Negativa
set.seed(1496)
x<-rnbinom(100,5,0.3)
print(x)

fitdistr(x,"negative binomial")
mean(x)
sd(x)


# Caso de una distribuci�n Binomial
set.seed(1496)
x<-rbinom(100,12,0.3)
print(x)

library(bbmle)
mtmp <- function(size,prob) 
  {-sum(dbinom(x,size,prob,log=TRUE))}
(m0 <- mle2(mtmp,start=list(prob=0.8),data=list(size=12)))
summary(m0)

mean(x) #recordar que la media de una binomial es np
sd(x)

# Caso de una distribuci�n de Poisson
set.seed(1496)
x<-rpois(100,1.5)
print(x)

fitdistr(x,"poisson")
mean(x)
sd(x)

#===========================
#(b) INTERVALOS DE CONFIANZA 
#===========================
# CASO DE UNA MUESTRA

# Para una variable normal N(mu,sigma)
#(a) intervalo de confianza sobre una media con varianza conocida
library(DescTools)

#(a1) intervalo de confianza para la media del PESO con desviaci�n t�pica (sd) conocida
MeanCI(peso,sd=10.0,conf.level=0.95)  

#(a2) intervalo de confianza para la media del PESO con desviaci�n t�pica (sd) desconocida
MeanCI(peso,conf.level=0.95)   
      
#(a3) intervalo de confianza para la varianza con media desconocida
VarCI(peso, conf.level=0.95)  

#(a4) intervalo de confianza para la varianza con media conocida (caso POCO USUAL)
mu0<-66   #asignacion del valor de la media
x11<-as.matrix(peso)
c11<-as.numeric(t(x11)%*%x11+length(HIPER200$peso)*mu0^2-2*mu0*apply(x11,2,sum))
l1<-c11/qchisq(0.975, df=length(HIPER200$peso))
l2<-c11/qchisq(0.025, df=length(HIPER200$peso))
ic4<-cbind(l1,l2)
ic4
c11/200

# intervalo de confianza para una proporci�n
BinomCI(x=40, n=100, conf.level=0.95, method="wald")  #x es el n�mero de �xitos; n el de pruebas de Bernoulli

# intervalo de confianza para lambda de una Poisson
PoissonCI(x=14, n=100, conf.level=0.95, method="wald")  

#CASO DE DOS MUESTRAS

#Para una variable normal, en dos subpoblaciones N(mu1,sigma1) y N(mu2,sigma2)

# (b1)IC para la diferencia de medias con varianzas desconocidas y distintas
MeanDiffCI(peso~genero, conf.level=0.95)

#IC para la diferencia de medias con varianzas desconocidas y distintas (otra manera)
peso1<-subset(HIPER200$peso, genero=="masculino")
peso2<-subset(HIPER200$peso, genero=="femenino")
MeanDiffCI(peso1,peso2, conf.level=0.95) 

# (c) INTERPRETACI�N DE UN INTERVALO DE cONFIANZA
library(TeachingDemos)
set.seed(19485)
ci.examp(mean.sim = 12, sd = 3, n = 1000, reps = 30, conf.level = 0.95)

#En una gran ciudad, para una muestra de 500 familias, se comprob� que 280 hab�an contratado cierto canal privado de televi�n.
#Determina un intervalo de confianza del 95%, para estimar la proporci�n real de familias suscritas a dicho canal
#Soluci�n: estimaci�n de la proporci�n: 56%; intervalo de confianza al 95%: (0.5164906, 0.6035094)


#=====================
# FINAL DE LA PR�CTICA
#=====================
