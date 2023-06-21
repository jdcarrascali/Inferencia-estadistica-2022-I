N=1000 #Definir tamaño de muestra
set.seed(2)
data<-rnorm(n=N,mean=(1/7),sd=sqrt(7))
mean(data)
var(data)
var(data)/mean(data)
miu_hat<- 1/7
miu_hat
var_hat<- 7

x <- seq(-8, 8, 0.1)
#Distribución empiríca de los datos
y<-ecdf(data)
plot(y,main="Distribución empírica vs teórica")
curve(pnorm(x, mean = 1/7, sd = sqrt(7)), from = -10,to=10,col=2,lwd=4,add=TRUE)
#Bootstraps
bootstrap<- replicate(n=1000,sample(data,replace=TRUE))
dim(bootstrap)
#Funciones media muestral y primera distribución
#Media muestral
x_i=NULL
x_barra<-function(n){  
  for (i in 1:N){
    x_i<-append(x_i,bootstrap[i,n])
  }
  return(mean(x_i))
}
#Vector de medias e histograma de medias
y_i=NULL
for (i in 1:1000){
  y_i=append(y_i,x_barra(i))
}


#Distribución estandarización
x_i=NULL
x_est<-function(n){  
  for (i in 1:N){
    x_i<-append(x_i,bootstrap[i,n])
  }
  return(sqrt(N)*(mean(x_i)-mean(data))/sqrt(var(data)))
}


#Vector de estandarizaciones e histograma
y_i=NULL
for (i in 1:1000){
  y_i=append(y_i,x_est(i))
}
hist(y_i,breaks=50,freq=F,main = "Histograma de medias")
  curve(dnorm(x, mean = 0, sd = 1), from = -3,to=3,col=2,lwd=4,add=TRUE)
  
#Varianza muestral
x_i=NULL
s_i<-function(n){  
    for (i in 1:N){
      x_i<-append(x_i,bootstrap[i,n])
    }
    return(var(x_i))}
#Vector de varianzas e histograma
y_i=NULL
for (i in 1:1000){
  y_i=append(y_i,s_i(i))
}
#Distribución chi
x_i=NULL
c_i<-function(n){ 
  for (i in 1:N){
    x_i<-append(x_i,bootstrap[i,n])
  }
  return((N-1)*var(x_i)/var(data))
}
#Vector de chis e histograma
y_i=NULL
for (i in 1:1000){
  y_i=append(y_i,c_i(i))
}

hist(y_i,breaks=50,freq=F,main="Histograma de varianzas")
curve(dchisq(x,df=N-1), from = 0,to=25,col=2,lwd=4,add=TRUE) # ajustar limites

#Cociente de variación
x_i=NULL
err_i<-function(n){ 
  for (i in 1:N){
    x_i<-append(x_i,s_i(i)/x_barra(i))}
return(x_i)
}
#Vector de cocientes e histograma
y_i=NULL
for (i in 1:1000){
  y_i=append(y_i,err_i(i))
}
hist(y_i,breaks=50,freq=F,xlim = c(0, 60),main="Histograma de cocientes")#Cambiar limites
curve(dnorm(x,mean=9,sd=2), from = 0,to=100,col=2,lwd=4,add=TRUE)# Curva N=10
10-sqrt(var(data))/mean(data)# Sesgo n=10
curve(dchisq(x,df=22), from = 0,to=100,col=2,lwd=4,add=TRUE)#Curva N=1000
22-sqrt(var(data))/mean(data)#Sesgo n=1000

abline(v=22,col=4,lwd=3)# Reemplazar v=9 si n=10
abline(v=sqrt(var(data))/mean(data),col=6,lwd=3)
sqrt(var(data))/mean(data)-10
