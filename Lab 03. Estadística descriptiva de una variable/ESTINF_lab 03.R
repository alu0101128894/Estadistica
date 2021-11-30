# ESTADISTICA DESCRIPTIVA DE UNA VARIABLE

setwd("C:/Users/Kephril/Desktop/Estadistica/Lab_03_Estad�stica_descriptiva_de_una_variable")
load("HIPER200.RData")
str(HIPER200)
attach(HIPER200)

# Todos los estadisticos descriptivos de una variable y la tabla de frecuencias
# de manera conjunta con el paquete(libreria) DESCTOOLS
library(DescTools)

# Tabla de frecuencias para variables cualitativas nominales y ordenadas
#options(scipen = 3)    #suprime la notación exponencial en una tabla de frecuencias
Freq(act_fisi)
Freq(estudios)
Freq(cafe)
Freq(genero)

Mode(genero)
Mode(cafe)

# Tabla de frecuencias para variables continuas
Freq(edad)
print(edad)
c1<-c(15,20,30,40,50,60,70,90)
Freq(edad, breaks=c1,right=FALSE)
Mode(edad)

# Estadistica descriptiva de una variable cuantitativa
#Desc(edad, plotit=FALSE)
Desc(edad, plotit=TRUE)

quantile(edad,0.35)

quantile(act_fisi,0.35, type=1)  #cuantil para una variable cualitativa ordinal

# Estadística descriptiva de todo el el conjunto de datos (data frame)
Desc(HIPER200, plotit=FALSE)  

#===========================================================================================
# Estadistica descriptiva de una variable númerica, dentro del programa basico STATS
summary(estudios)
summary(edad)
min(edad)
max(edad)
range(edad)
sum(edad)
cumsum(edad)
mean(edad)
median(edad)
length(edad)
sd(edad)
sd1<-sqrt((length(edad)-1)/length(edad))*sd(edad) 
print(sd1)
quantile(edad,0.35)
IQR(edad)
table(edad)
unique(edad)
sum(edad)+(peso)+(talla) +(TAsist0) + (TAdias0) + (TAsist1) + (TAdias1)

summary(HIPER200)     # Estadística descriptiva de todo el data frame con la libreria STATS
#===========================================================================================

# Momentos de una variable
library(moments)
moment(edad)
all.moments(TAsist1, order.max = 4, central = FALSE, absolute = FALSE, na.rm = FALSE)
all.moments(TAsist1, order.max = 4, central = TRUE, absolute = FALSE, na.rm = FALSE)

# GRÁFICOS UNIVARIANTES  1D

# Diagrama de barras con R basico (graphics)
table(estudios)
barplot(table(estudios), main="Diagrama de barras",sub="Nivel de estudios", space=2, beside=TRUE)

plot(table(estudios),type="l") #poligono de frecuencias
table(act_fisi)
barplot(table(act_fisi), width=1.5, space=1, border="blue",col=5, ylim=c(0,100), 
        main="",xlab="Actividad fisica", ylab="frecuencias", beside=FALSE, horiz=FALSE)
title(main = list("Estudio de Hipertension", cex = 1.5, col = "blue", font = 3))

# Diagrama de sectores con R basico (graphics)
#table(profesio)
pie(table(profesio), main="Grafico de sectores\nProfesion")

#help("hist")

# Histograma con R basico (graphics)
hist(peso)
hist(peso, breaks=3) 
hist(peso, freq=TRUE, breaks=7, col="darkgray", xlab="Peso", ylab="frec",
     main="Histograma")

# ggplot2 un paquete(libreria) MUY COMPLETO de graficos
library(ggplot2) 

# Diagrama de barras
qplot(act_fisi, data = HIPER200, geom = "bar", ylab="frecuencias")
ggplot(HIPER200, aes(x=profesio)) + geom_bar(fill="white", colour="black") + theme_bw()

# Histograma
qplot(peso, data = HIPER200, geom = "histogram", ylab="frecuencias")
ggplot(HIPER200, aes(x=peso)) + geom_histogram(binwidth=10, fill="white", colour="black") + theme_bw() 

# Poligono de frecuencias
ggplot(HIPER200, aes(x=peso)) +  geom_freqpoly(binwidth=10)+ theme_bw() 

# Histograma mas poligono de frecuencias
ggplot(HIPER200, aes(x=edad)) +
  geom_histogram(binwidth=10, fill="white", colour="black") +
  geom_freqpoly(binwidth=10)+ theme_bw()

# Curva acumulativa o de distribucion
plot.ecdf(x=edad, verticals=FALSE , do.points=FALSE ,
          main="Curva de distribución de la Edad", lwd=2, ylab="valores de F",xlab="Edad", 
          panel.first=grid(col="gray",lty="dotted"))

# Grafico de cajas de Box
boxplot(TAsist0)
boxplot(TAsist0, range=1.5,ylab="mmHg", main="Estudio de Hipertension", sub="TA Sistalica inicial")

ggplot(HIPER200, aes(x=1, y=peso)) +  geom_boxplot() + theme_bw() 

# grafico de tallo y hojas
stem(edad)


#=====================
# FINAL DE LA PRÁCTICA
#=====================

