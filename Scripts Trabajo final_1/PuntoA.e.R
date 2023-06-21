#Funciones
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
R_n <-function(a,b,n){
  x=n*(a-b)
  return(x)
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
#x para la función R_n
xpts<-seq(-10,50,by=0.001)
#Creo un vector para guardar los valores de R_n
v <- c()
#Genero los valores de la función R_n
for(i in MLEM){
  x <- R_n(7,i,n)
  v <- c(v,x)
}
#Dibujo un histograma con los 1000 R_n
hist(v,prob=TRUE,xlim=c(-10,50),xlab="MLE",ylab="R_n",main="Histograma")