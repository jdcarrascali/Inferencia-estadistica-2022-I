#Funciones
Indicadora <-function(a,b,x){
  if (a<=x && x<b){
    c=1 
  }
  else{
    c=0
  }
  return(c)
}
MLE <-function(X){
  #Halló X^1 y X^n
  x_1 <- min(X)
  x_n <- max(X)
  #Fabrico un vector con -X^1 y X^n 
  v <- c(-x_1,x_n)
  #Halló el MLE
  MLE <- max(v)
  return(MLE)
}
fteorica <-function(x,n){
  (1/(7^n))*n*x^{n-1}*Indicadora(0,7,x)
}

#Defino el tamaño de la muestra
n=1000
#Creo un vector vacio que me guardara el MLE de cada muestra
MLEM <- c()
#Creo un for que me ayudará a generar las 1000 muestras
for(i in 1:1000){
  #Genero una muestra de tamaño n
  x <- runif(n)
  #Realizaciones
  y <- qunif(x,min=-7,max=7)
  #Calculó el MLE de la muestra
  a <- MLE(y)
  #Guardo el MLE de la muestra
  MLEM <- c(MLEM, a)
  #Continuo con la siguiente muestra
}
#Dibujo un histograma con los 1000 MLE
hist(MLEM,prob=TRUE,xlim=c(-2,10),xlab="MLE",ylab="Densidad",main="Histograma")
#x para la función
xpts<-seq(-2,10,by=0.001)
#Creo un vector para guardar los valores de la función teoríca
v <- c()
#Genero los valores de la función teoríca
for(i in xpts){
  x <- fteorica(i,n)
  v <- c(v,x)
}
#Imprimo la función teoríca
lines(xpts,v,col="red",lwd=2)