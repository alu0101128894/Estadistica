# Ejemplos de asignación numérica

a<-2          
3->b                # constante
c=5                 # constante
c = 7               # constante

# Ejemplos de asignación no numérica

a<-"2"                 # constante
b<-"casado"            # constante
is.numeric(b)          

# Ejemplos de asignación a un vector

a<-c(1,5,3,2,9,-1)             # vector 
b<-c("d","g","m","z","13")     # vector
d <- c(3:15)                   # vector 

# Ejemplos de asignación para trabajar con matrices

d <- matrix( 1:10, nrow=2)                # matriz
d <- matrix( 1:10, ncol=2 )               # matriz
d <- matrix( 1:10, nrow=5,ncol=2 )        # matriz
d <- matrix( 1:1000, ncol=5 )             # matriz
d<-matrix(c(18,15,12,27,5, 4,9,7,-7,-5,-4,-2), nrow=3)
d<-matrix(c(18,15,12,27,5, 4,9,7,-7,-5,-4,-2), ncol=3)
d<-matrix(c(18,15,12,27,5, 4,9,7,-7,-5,-4,-2), nrow=4,ncol=3)
d<-matrix(c(18,15,12,27,5, 4,9,7,-7,-5,-4,-2), nrow=3,ncol=4)

# Ejemplos de acceso a los elementos de una matriz

d[3,2]     # elemento que ocupa el lugar (3,2) en la matriz d
d[2,]      # elementos que forman la fila 2 en la matriz d 
d[,3]      # elementos que forman la columnas 3 en la matriz d


dim(d)                                         # dimensiones de una matriz
remove(d)                                      # elimina un objeto
ls()                                           # lista los objetos del workspace
remove(a,c)                                    # elimina varios objetos
remove(list=ls())                              # elimina todos los objetos

a <- seq( from=5, to=23, by=.5 )               # crear un vector mediante sequence
a <- seq( from=5, to=23, length=100 )
a1<-rep(3,10)                                  # crear un vector mediante rep
a1<-rep(1:10,3) 
b <- rnorm(100)                        # generación de valores de una distribución normal
b <- runif(100)                        # generación de valores de una distribución uniforme
hist(b)                                # un histograma simple
c <- cbind(a,b)                        # combina columnas en una matriz
d <- rbind(a,b)                        # combina filaS en una matriz
dt <- t(d)                             # transpuesta de una matriz
e <- cbind(c,dt)                       # combina columnas en una matriz
e

colnames(e) <- c("nom1","nom2","nom3","nom4")  # poner nombre a columnas de una matriz
rownames(e) <- paste("obs",1:100, sep="")      # poner nombre a filas de una matriz
e
# nombre a filas y columnas de manera conjunta
dimnames(e) <- list( paste("obs",1:100, sep=""),c("nom1","nom2","nom3","nom4"))

f<-c(letters,letters,letters)                     # vector de caracteres
#f[1:78] <- f                                       
f[79:100]<-NA
h <- data.frame(e,f)                              # data frame
dim(h)                                            # las dimensiones del data frame
str(h)                                           # la estructura del data fame
remove(list=ls())  

a <- 1:20                            # vector
b <- 31:50                           # vector 
      
# Operaciones con vectores

sqrt(a)
a+b                                 
a-b
a*b
a/b
b^a

# Operaciones con matrices

A<-matrix(1:12,nrow=3)      # formamos la matriz A
A
log(A)
A[2,3]
A[2,]
A[,3]
C<-A[c(1,3),2:3]
C
B<-matrix(-8:1,nc=3, byrow=T)       # formamos la matriz B   
B
A+B                                 # no es posible realizar la operación
A+t(B)                              # realiza la suma elemento a elemento
A*t(B)                              # realiza el producto elemento a elemento   
C1<-A%*%B                           # producto de matrices  
C1
D<-solve(C1)                        # matriz inversa
remove(list=ls()) 
A<-matrix(c(30,18,42,66,45,96,78,42,114),nrow=3, byrow=TRUE)
A
det(A)                            # determinante
eigen(A)                          # diagonaliza la matriz
D<-solve(A)                       # matriz inversa
D
A%*%D
remove(list=ls())  

# ejemplo de bucle (solo para que lo miren)
h <- seq(from=1, to=12)
s <- c()
for(i in 3:15)
   { s[i] <- h[i] * 10 }
s
remove(list=ls())  

# ejemplo simple de gráfica
curve(3*x+10,-10,10)
curve(x^2-3*x+10,-10,10, add=TRUE)
curve(x^3,-10,10, add=TRUE)
curve(x^4,-10,10, add=TRUE)

#=====================
# FINAL DE LA PRÁCTICA
#=====================