

setwd("M:/RTRABAJO/INF")     # establece el directorio de trabajo
load("HIPER200.RData")
attach(HIPER200)
library(HH) #Librer�a de soporte gr�fico

#==================================
#TEST DE HIPOTESIS CON UNA MUESTRA
#==================================

# Para una variable normal N(mu,sigma)
#(a) test de hipotesis sobre una media con varianza conocida
# Vamos a echar un vistazo a los descriptores de la variable TAsis0
library(DescTools)
Desc(TAsist0, plotitz1=TRUE)

library(BSDA)
# Construimos el test de medias, bajo hip�tesis nula de mu=150.4, �la muestra responde a esa distribuci�n?
z1 <- z.test(TAsist0,alternative="two.sided", mu=150.4, sigma.x=25)  #contraste bilateral
z1

# El estad�stico normalizado es -2.1298, �que probabilidad en las dos colas acumula?

# Por tanto, �aceptamos la hip�tesis nula?�a qu� nivel de verosimilitud?

# Ahora, valoramos el gr�fico (importante: en �l, se suman las probabilidades de las dos colas)
print(NTplot(mean0=150.4, sd=25,xbar=146.635,n=200,xlim=c(140, 160), alpha.right=0.025, alpha.left=0.025), tablesOnPlot=FALSE)

# Ahora un test por la izquierda
z2<-z.test(TAsist0,alternative="less",  mu=150.4, sigma.x=25) #contraste unilateral izquierda
z2
print(NTplot(mean0=150.4, sd=25,xbar=146.635,  n=200, xlim=c(140, 160), alpha.right=0.0,alpha.left=0.05), tablesOnPlot=FALSE)

z.test(TAsist0,alternative="greater",  mu=150.4, sigma.x=25) #contraste unilateral derecha

#(b) test de hipotesis sobre una media con varianza desconocida
# Recu�rdese que en los bilaterales, se suman las probabilidades de ambas colas

t1<-t.test(TAsist0,alternative="two.sided", mu=150.4)  #contraste bilateral
t1
print(NTplot(t1), tablesOnPlot=FALSE)

t2<-t.test(TAsist0,alternative="less", mu=150.4)  #contraste unilateral izquierda
t2
t3<-t.test(TAsist0,alternative="greater", mu=150.4) #contraste unilateral derecha
t3

#(c) Test para una proporcion Bi(1,p)
prop.test(60, 100, p=0.68, alternative="two.sided",conf.level = 0.95, correct = FALSE)   #contraste bilateral

#=================================================
#TEST DE HIPOTESIS CON DOS MUESTRAS INDEPENDIENTES
#=================================================

# Para realizar estos contrastes no es necesario tener dos muestras. Tambi�n
# se pueden crear grupos seg�n los valores de una variable (factor)

ed<-ifelse(edad<=40,1,2)  #recodificaci�n de la edad en DOS clases disjuntas
ed<-factor(ed, labels=c("<=40",">40"))

# Otra opcion es recodificar un factor para que tenga dos modalidades
a_f<-HIPER200$act_fisi             #se copia la variable en a_f
levels(a_f)<-c("escasa", "moderada","moderada")   #se recodifica via los niveles del factor dando el mismo nombre

#Por ultimo, se puede seleccionar casos desde una tercera variable
HIPER200_25 <- subset(HIPER200, subset=edad > 25, select=c(TAsist0,genero))

# Para una variable normal en dos poblaciones: N(mu1,sigma1) y N(mu2,sigma2)

#(d) test de hipotesis sobre la igualdad de medias cuando las varianzas son conocidas
TAsist0_m <-subset(HIPER200$TAsist0, subset=genero=="masculino")
TAsist0_f <-subset(HIPER200$TAsist0, subset=genero=="femenino")
z.test(TAsist0_m,TAsist0_f,alternative="two.sided",mu=0, sigma.x=20, sigma.y=28)

Desc(TAsist0_m, plotit=FALSE)
Desc(TAsist0_f, plotit=FALSE)

#(e) test de hipotesis sobre la igualdad de medias cuando las varianzas son desconocidas pero iguales
t4<-t.test(peso~genero,alternative="two.sided",var.equal=TRUE)  #contraste bilateral
print(NTplot(t4), tablesOnPlot=FALSE)
t.test(peso~genero,alternative="less",var.equal=TRUE)  #contraste unilateral izquierda
t.test(peso~genero,alternative="greater",var.equal=TRUE)  #contraste unilateral derecha

#(f) test de hipotesis sobre la igualdad de medias cuando las varianzas son desconocidas pero distintas
t5<-t.test(peso~genero,alternative="two.sided",var.equal=FALSE)  #contraste bilateral
t.test(peso~genero,alternative="less",var.equal=FALSE)  #contraste unilateral izquierda
t.test(peso~genero,alternative="greater",var.equal=FALSE)  #contraste unilateral derecha

#(g) test de hipotesis sobre la igualdad de varianzas cuando las medias son desconocidas
var.test(peso~genero,alternative="two.sided")

#(h) test de hipotesis sobre la igualdad de medias cuando las muestras son DEPENDIENTES
t6<-t.test(TAsist1,TAsist0,alternative="two.sided",paired=TRUE)
print(NTplot(t6), tablesOnPlot=FALSE)

#(i) test de hipotesis sobre la igualdad de dos proporciones Bi(1,p1) y Bi(1,p2)
prop.test(x = c(16, 34), n = c(100, 100), correct = FALSE)   # dos proporciones
table(conc_hta,genero)

# Ejercicio
# Una empresa inform�tica afirma en su publicidad que, al menos, el 96% de los ordenadores que fabrica est�n libres
# de fallos durante el primer a�o de uso. Sin embargo, para una muestra de 150 ordenadores comprados a dicha empresa,
# se detectan fallos, durante el primer a�o de uso, en 15 de ellos.
# �Se puede rechazar, con alfa=0.1 la afirmaci�n de la empresa?�Cual es el p-valor?
# El p-valor es 0.0001768 




#=====================
# FINAL DE LA PR�CTICA
#=====================
