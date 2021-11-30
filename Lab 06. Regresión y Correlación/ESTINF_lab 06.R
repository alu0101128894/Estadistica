#Establecimiento del directorio de trabajo por defecto
setwd("M:/RTRABAJO/INF")
load("HIPER200.RData")
attach(HIPER200)

# REPRESENTACIÓN GRÁFICA DE VARIABLES CUANTITATIVAS
plot(peso,talla)  #diagrama de dispersión
plot(peso,talla, pch=20, col="red",main="diagrama de dispersión")  

plot(TAdias0,TAsist0, pch=20, col="red", xlab="TA Diastólica inicial (mmHg)", ylab="TA Sistólica inicial (mmHg)",main="diagrama de dispersión") 
plot(edad,TAsist0, pch=20, col="red", xlab="Edad", ylab="TA Sistólica inicial (mmHg)",main="diagrama de dispersión") 

# GRÁFICOS MEJORADOS , PERO OPCIONALES
library(lattice)
xyplot(TAsist0~TAdias0, type=c("p", "r"))

library(ggplot2)
ggplot(HIPER200, aes(x=TAdias0, y=TAsist0, shape=genero, colour=genero)) +   geom_point(size=3,shape=20) +   scale_shape_manual(values=c(1,2)) + scale_colour_brewer(palette="Set1")+ theme_bw()

ggplot(HIPER200, aes(x=TAdias0, y=TAsist0, shape=genero)) +   geom_point() + stat_smooth(method=lm, level=0.95) + theme_bw()

ggplot(HIPER200, aes(x=peso, y=talla, shape=genero, colour=genero)) +   geom_point(size=3,shape=20) +   scale_shape_manual(values=c(1,2)) + scale_colour_brewer(palette="Set1")+ theme_bw()

ggplot(HIPER200, aes(x=peso, y=talla, shape=genero)) +   geom_point() + stat_smooth(method=lm, level=0.95) + theme_bw()

ggplot(HIPER200, aes(x=TAdias0, y=TAsist0,  colour=edad)) +   geom_point() + theme_bw()

# Regresión lineal: recta, parámetros, gráfico de dispersión con recta
reg<-lm(TAsist0~TAdias0)  # establecimiento del modelo
print(reg)
summary(reg)
plot( TAdias0, TAsist0, pch=20, col="red",main="diagrama de dispersión")
abline(reg)
cov(TAsist0, TAdias0)   # covarianza
cor(TAsist0, TAdias0)   # correlacion de Pearson

reg<-lm(TAdias1~TAdias0)  # establecimiento del modelo
print(reg)
summary(reg)
plot( TAdias0, TAdias1, pch=20, col="red",main="diagrama de dispersión")
abline(reg)
cov(TAdias1, TAdias0)   # covarianza
cor(TAdias1, TAdias0)   # correlacion de Pearson


reg<-lm(TAsist0~TAdias0,data=HIPER200,subset=genero=="masculino")  # establecimiento del modelo para una subpoblación
reg<-lm(TAsist0~TAdias0,data=HIPER200,subset=edad>50)              # establecimiento del modelo para una subpoblación

#======================
# datos del ejercicio 1, para la aplicación de diversos modelos
#======================
x<-c(2,3,4,4,5,5,6,7,7,9,9,10,11,11,12)
y<-c(11,12,10,13,11,9,10,7,12,8,7,3,6,5,5) 
plot(x,y, pch=20, col="red", xlim=c(0,16),ylim=c(0,12), xlab="Nº de núcleos", ylab="Tiempo de respuesta",main="diagrama de dispersión") 
cov(x,y)  # covarianza entre X e Y
cor(x,y)  # correlación entre X e Y


# REGRESIÓN LINEAL - VARIOS MODELOS

# 1-modelo con recta
reg1<-lm(y~x)
print(reg1$coef)
summary(reg1)
abline(reg1)
xdat1<-cbind(x,y,reg1$fitted,reg1$residuals) # construimos una matriz con los datos, predichos y residuales
colnames(xdat1) <- c("núcleos","tiempo","pred1","res1")
print(xdat1)

#pronosticos de y para ciertos valores de x=13,14 y 15
predict (reg1 ,data.frame(x=c(13,14,15)),interval ="prediction")

x1<-c(13,14,15) ; y1<-c(3.78,2.98,2.17)
points(x1,y1,col="blue",pch=20)

# 2-modelo parabólico
reg2<-lm(y~x+I(x^2))
summary(reg2)
print(reg2$coef)
xdat2<-cbind(x,y,reg2$fitted,reg2$residuals)
colnames(xdat2) <- c("núcleos","tiempo","pred2","res2")
print(xdat2)
curve(reg2$coef[1]+reg2$coef[2]*x+reg2$coef[3]*x^2, 1,15,add=TRUE, col="blue")

#pronosticos de y en ciertos valores de x=13, 14 y 15
predict (reg2 ,data.frame(x=c(13,14,15)),interval ="prediction")

# 3-modelo polinómico de grado 3
reg3<-lm(y~x+I(x^2)+I(x^3))
print(reg3$coef)
summary(reg3)
xdat3<-cbind(x,y,reg3$fitted,reg3$residuals)
colnames(xdat3) <- c("núcleos","tiempo","pred3","res3")
print(xdat3)
curve(reg3$coef[1]+reg3$coef[2]*x+reg3$coef[3]*x^2+reg3$coef[4]*x^3, 1,15,add=TRUE, col="green")

#pronosticos de y en ciertos valores de x=13, 14 y 15
predict (reg3 ,data.frame(x=c(13,14,15)),interval ="prediction")

# 4-modelo hiperbólico  y=a+b*(1/x)
reg4<-lm(y~I(1/x)) 
summary(reg4)
curve(reg4$coef[1]+reg4$coef[2]*(1/x),add=TRUE, col="orange")
#FALTA TABLA DE DATOS CON PREDICHOS Y RESIDUALES PARA ESTE MODELO
#FALTA GRÁFICA DE DATOS CON LA FUNCIÓN AJUSTADA
#FALTAN LAS PREDICCIONES DEL MODELO EN CIERTOS VALORES DE X

legend(13,12, c("lin","cuad","cub","hip"),col=c("black","blue","green","orange"),lty=c(1,1,1,1))

# 5-modelo logarítmico
reg5<-lm(y~log(x))
print(reg5$coef)
summary(reg5)
#FALTA TABLA DE DATOS CON PREDICHOS Y RESIDUALES PARA ESTE MODELO
#FALTAN LAS PREDICCIONES DEL MODELO EN CIERTOS VALORES DE X

plot(log(x),y, pch=20, col="red", xlab="log(Nºde núcleos)", ylab="Tiempo de respuesta",main="diagrama de dispersión") 
abline(reg5)


#=====================
# FINAL DE LA PRÁCTICA
#=====================
