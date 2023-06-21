####### Punto d + Bonus############
library(modeest)

m <- 1000
Theta <- 7

## Estimadores

theta_1 <- function(x){
  mean(x)
}

theta_2 <- function(x){
  median(x)
}

theta_3 <- function(d){
  Theta=d
  flogis=exp(-(-x-Theta))/(1+exp(-(x-Theta)))^2
  L=-sum(log(flogis))
}

thetaMLE <- function(x){
  solLogis=optim(1,theta_3)
  theta_MLE=solLogis$par[1]
}
  
theta_4 <- function(x){
  mlv(x)
}

bias <- function(x){
  mean(x)-Theta
}

MSE <- function(x){
  bias(x)^2+var(x)
}

Theta_1 <- numeric(m)
Theta_2 <- numeric(m)
Theta_3 <- numeric(m)
Theta_4 <- numeric(m)

##m = 1000 muestras de tamaño n = 10

n <- 10
set.seed(7)
for (m in 1:m) {
  x <- rlogis(n,7,1)
  Theta_1[m] <- theta_1(x)
  Theta_2[m] <- theta_2(x)
  Theta_3[m] <- thetaMLE(x)
  Theta_4[m] <- theta_4(x)
}

###Histograma estimador 1 (Media Muestral)

hist(Theta_1,prob=TRUE,xlim=c(min(Theta_1),max(Theta_1)),main="Estimador Media muestral")
abline(v=c(7,mean(Theta_1)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 2 (Mediana Muestral)

hist(Theta_2,prob=TRUE,xlim=c(min(Theta_2),max(Theta_2)),main="Estimador Mediana muestral")
abline(v=c(7,mean(Theta_2)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 3 (ML)

hist(Theta_3,prob=TRUE,xlim=c(min(Theta_3),max(Theta_3)),main="Estimador ML")
abline(v=c(7,mean(Theta_3)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 4 (Moda)

hist(Theta_4,prob=TRUE,xlim=c(min(Theta_4),max(Theta_4)),main="Estimador Moda")
abline(v=c(7,mean(Theta_4)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

media1 <- mean(Theta_1)
media2 <- mean(Theta_2)
media3 <- mean(Theta_3)
media4 <- mean(Theta_4)

varianza1 <- var(Theta_1)
varianza2 <- var(Theta_2)
varianza3 <- var(Theta_3)
varianza4 <- var(Theta_4)

sesgo1 <- bias(Theta_1)
sesgo2 <- bias(Theta_2)
sesgo3 <- bias(Theta_3)
sesgo4 <- bias(Theta_4)

ECM1 <- MSE(Theta_1)
ECM2 <- MSE(Theta_2)
ECM3 <- MSE(Theta_3)
ECM4 <- MSE(Theta_4)

######## Punto e ################

##m = 1000 muestras de tamaño n = 50

n <- 50
set.seed(7)
for (m in 1:m) {
  x <- rlogis(n,7,1)
  Theta_1[m] <- theta_1(x)
  Theta_2[m] <- theta_2(x)
  Theta_3[m] <- thetaMLE(x)
  Theta_4[m] <- theta_4(x)
}

###Histograma estimador 1 (Media Muestral)

hist(Theta_1,prob=TRUE,xlim=c(min(Theta_1),max(Theta_1)),main="Estimador Media muestral")
abline(v=c(7,mean(Theta_1)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 2 (Mediana Muestral)

hist(Theta_2,prob=TRUE,xlim=c(min(Theta_2),max(Theta_2)),main="Estimador Mediana muestral")
abline(v=c(7,mean(Theta_2)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 3 (ML)

hist(Theta_3,prob=TRUE,xlim=c(min(Theta_3),max(Theta_3)),main="Estimador ML")
abline(v=c(7,mean(Theta_3)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 4 (Moda)

hist(Theta_4,prob=TRUE,xlim=c(min(Theta_4),max(Theta_4)),main="Estimador Moda")
abline(v=c(7,mean(Theta_4)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

media1 <- mean(Theta_1)
media2 <- mean(Theta_2)
media3 <- mean(Theta_3)
media4 <- mean(Theta_4)

varianza1 <- var(Theta_1)
varianza2 <- var(Theta_2)
varianza3 <- var(Theta_3)
varianza4 <- var(Theta_4)

sesgo1 <- bias(Theta_1)
sesgo2 <- bias(Theta_2)
sesgo3 <- bias(Theta_3)
sesgo4 <- bias(Theta_4)

ECM1 <- MSE(Theta_1)
ECM2 <- MSE(Theta_2)
ECM3 <- MSE(Theta_3)
ECM4 <- MSE(Theta_4)

##m = 1000 muestras de tamaño n = 100

n <- 100
set.seed(7)
for (m in 1:m) {
  x <- rlogis(n,7,1)
  Theta_1[m] <- theta_1(x)
  Theta_2[m] <- theta_2(x)
  Theta_3[m] <- thetaMLE(x)
  Theta_4[m] <- theta_4(x)
}

###Histograma estimador 1 (Media Muestral)

hist(Theta_1,prob=TRUE,xlim=c(min(Theta_1),max(Theta_1)),main="Estimador Media muestral")
abline(v=c(7,mean(Theta_1)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 2 (Mediana Muestral)

hist(Theta_2,prob=TRUE,xlim=c(min(Theta_2),max(Theta_2)),main="Estimador Mediana muestral")
abline(v=c(7,mean(Theta_2)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 3 (ML)

hist(Theta_3,prob=TRUE,xlim=c(min(Theta_3),max(Theta_3)),main="Estimador ML")
abline(v=c(7,mean(Theta_3)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 4 (Moda)

hist(Theta_4,prob=TRUE,xlim=c(min(Theta_4),max(Theta_4)),main="Estimador Moda")
abline(v=c(7,mean(Theta_4)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

media1 <- mean(Theta_1)
media2 <- mean(Theta_2)
media3 <- mean(Theta_3)
media4 <- mean(Theta_4)

varianza1 <- var(Theta_1)
varianza2 <- var(Theta_2)
varianza3 <- var(Theta_3)
varianza4 <- var(Theta_4)

sesgo1 <- bias(Theta_1)
sesgo2 <- bias(Theta_2)
sesgo3 <- bias(Theta_3)
sesgo4 <- bias(Theta_4)

ECM1 <- MSE(Theta_1)
ECM2 <- MSE(Theta_2)
ECM3 <- MSE(Theta_3)
ECM4 <- MSE(Theta_4)

##m = 1000 muestras de tamaño n = 200

n <- 200
set.seed(7)
for (m in 1:m) {
  x <- rlogis(n,7,1)
  Theta_1[m] <- theta_1(x)
  Theta_2[m] <- theta_2(x)
  Theta_3[m] <- thetaMLE(x)
  Theta_4[m] <- theta_4(x)
}

###Histograma estimador 1 (Media Muestral)

hist(Theta_1,prob=TRUE,xlim=c(min(Theta_1),max(Theta_1)),main="Estimador Media muestral")
abline(v=c(7,mean(Theta_1)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 2 (Mediana Muestral)

hist(Theta_2,prob=TRUE,xlim=c(min(Theta_2),max(Theta_2)),main="Estimador Mediana muestral")
abline(v=c(7,mean(Theta_2)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 3 (ML)

hist(Theta_3,prob=TRUE,xlim=c(min(Theta_3),max(Theta_3)),main="Estimador ML")
abline(v=c(7,mean(Theta_3)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 4 (Moda)

hist(Theta_4,prob=TRUE,xlim=c(min(Theta_4),max(Theta_4)),main="Estimador Moda")
abline(v=c(7,mean(Theta_4)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

media1 <- mean(Theta_1)
media2 <- mean(Theta_2)
media3 <- mean(Theta_3)
media4 <- mean(Theta_4)

varianza1 <- var(Theta_1)
varianza2 <- var(Theta_2)
varianza3 <- var(Theta_3)
varianza4 <- var(Theta_4)

sesgo1 <- bias(Theta_1)
sesgo2 <- bias(Theta_2)
sesgo3 <- bias(Theta_3)
sesgo4 <- bias(Theta_4)

ECM1 <- MSE(Theta_1)
ECM2 <- MSE(Theta_2)
ECM3 <- MSE(Theta_3)
ECM4 <- MSE(Theta_4)

##m = 1000 muestras de tamaño n = 500

n <- 500
set.seed(7)
for (m in 1:m) {
  x <- rlogis(n,7,1)
  Theta_1[m] <- theta_1(x)
  Theta_2[m] <- theta_2(x)
  Theta_3[m] <- thetaMLE(x)
  Theta_4[m] <- theta_4(x)
}

###Histograma estimador 1 (Media Muestral)

hist(Theta_1,prob=TRUE,xlim=c(min(Theta_1),max(Theta_1)),main="Estimador Media muestral")
abline(v=c(7,mean(Theta_1)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 2 (Mediana Muestral)

hist(Theta_2,prob=TRUE,xlim=c(min(Theta_2),max(Theta_2)),main="Estimador Mediana muestral")
abline(v=c(7,mean(Theta_2)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 3 (ML)

hist(Theta_3,prob=TRUE,xlim=c(min(Theta_3),max(Theta_3)),main="Estimador ML")
abline(v=c(7,mean(Theta_3)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 4 (Moda)

hist(Theta_4,prob=TRUE,xlim=c(min(Theta_4),max(Theta_4)),main="Estimador Moda")
abline(v=c(7,mean(Theta_4)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

media1 <- mean(Theta_1)
media2 <- mean(Theta_2)
media3 <- mean(Theta_3)
media4 <- mean(Theta_4)

varianza1 <- var(Theta_1)
varianza2 <- var(Theta_2)
varianza3 <- var(Theta_3)
varianza4 <- var(Theta_4)

sesgo1 <- bias(Theta_1)
sesgo2 <- bias(Theta_2)
sesgo3 <- bias(Theta_3)
sesgo4 <- bias(Theta_4)

ECM1 <- MSE(Theta_1)
ECM2 <- MSE(Theta_2)
ECM3 <- MSE(Theta_3)
ECM4 <- MSE(Theta_4)

##m = 1000 muestras de tamaño n = 1000

n <- 1000
set.seed(7)
for (m in 1:m) {
  x <- rlogis(n,7,1)
  Theta_1[m] <- theta_1(x)
  Theta_2[m] <- theta_2(x)
  Theta_3[m] <- thetaMLE(x)
  Theta_4[m] <- theta_4(x)
}

###Histograma estimador 1 (Media Muestral)

hist(Theta_1,prob=TRUE,xlim=c(min(Theta_1),max(Theta_1)),main="Estimador Media muestral")
abline(v=c(7,mean(Theta_1)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 2 (Mediana Muestral)

hist(Theta_2,prob=TRUE,xlim=c(min(Theta_2),max(Theta_2)),main="Estimador Mediana muestral")
abline(v=c(7,mean(Theta_2)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 3 (ML)

hist(Theta_3,prob=TRUE,xlim=c(min(Theta_3),max(Theta_3)),main="Estimador ML")
abline(v=c(7,mean(Theta_3)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

###Histograma estimador 4 (Moda)

hist(Theta_4,prob=TRUE,xlim=c(min(Theta_4),max(Theta_4)),main="Estimador Moda")
abline(v=c(7,mean(Theta_4)),col=c("red","blue"),lwd=2)
legend(locator(1),c("Verdadero Valor","Promedio m estimaciones"),col=c("red","blue"),lty=c(1,1),lwd=c(2,2))

media1 <- mean(Theta_1)
media2 <- mean(Theta_2)
media3 <- mean(Theta_3)
media4 <- mean(Theta_4)

varianza1 <- var(Theta_1)
varianza2 <- var(Theta_2)
varianza3 <- var(Theta_3)
varianza4 <- var(Theta_4)

sesgo1 <- bias(Theta_1)
sesgo2 <- bias(Theta_2)
sesgo3 <- bias(Theta_3)
sesgo4 <- bias(Theta_4)

ECM1 <- MSE(Theta_1)
ECM2 <- MSE(Theta_2)
ECM3 <- MSE(Theta_3)
ECM4 <- MSE(Theta_4)

######### Punto G ##############

##Vn #############

n <- 1000
set.seed(7)
for (m in 1:m) {
  v <- rnorm(n,0,sqrt(6))
}

hist(v,prob=TRUE,xlim=c(min(v),max(v)),ylim=c(0,0.17),xlab="Datos",ylab="Densidad",main="Histograma de Vn")
vpts <- seq(min(v),max(v),by=0.001)
lines(vpts,dnorm(vpts,mean=0,sd=sqrt(6)),col="red",lwd=2)

##Wn ################

n <- 1000
set.seed(7)
for (m in 1:m) {
  w <- rnorm(n,0,2)
}

hist(w,prob=TRUE,xlim=c(min(w),max(w)),xlab="Datos",ylab="Densidad",main = "Histograma de Wn")
wpts <- seq(min(w),max(w),by=0.001)
lines(wpts,dnorm(wpts,mean=0,sd=2),col="red",lwd=2)

##Un ################

n <- 1000
set.seed(7)
for (m in 1:m) {
  u <- rnorm(n,0,1)
}

hist(u,prob=TRUE,xlim=c(min(u),max(u)),xlab="Datos",ylab="Densidad",main="Histograma de Un")
upts <- seq(min(u),max(u),by=0.001)
lines(upts,dnorm(upts,mean=0,sd=1),col="red",lwd=2)