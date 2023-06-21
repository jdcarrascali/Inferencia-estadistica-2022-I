m <-1000
Theta <-7
###Estimadores

theta_1 <- function(x){
  mean(x)
}

theta_2 <- function(x){
  sqrt(1/(2*n)*sum(x^2))
}

theta_3 <- function(x){
  (1/(6*n)*sum(x^3))^(1/3)
}

theta_4 <- function(x){
  median(x)/log(2)
}

bias <- function(x){
  mean(x)-Theta
}

MSE <- function(x){
  bias(x)^2+var(x)
}

Theta1 <- numeric(m)
Theta2 <- numeric(m)
Theta3 <- numeric(m)
Theta4 <- numeric(m)

###Estimadores aplicados en las m muestras de tamaño 10

n <- 10
set.seed(7)
for (m in 1:m) {
  x <- rexp(n, rate=1/7)
  Theta1[m] <- theta_1(x)
  Theta2[m] <- theta_2(x)
  Theta3[m] <- theta_3(x)
  Theta4[m] <- theta_4(x)
}

###Histograma estimador 1

hist(Theta1,prob=TRUE,xlim=c(min(Theta1),max(Theta1)),main="Estimador 1")
abline(v=c(7,mean(Theta1)),col=c("red","blue"),lwd=2)

###Histograma estimador 2

hist(Theta2,prob=TRUE,xlim=c(min(Theta2),max(Theta2)),main="Estimador 2")
abline(v=c(7,mean(Theta2)),col=c("red","blue"),lwd=2)

###Histograma estimador 3

hist(Theta3,prob=TRUE,xlim=c(min(Theta3),max(Theta3)),main="Estimador 3")
abline(v=c(7,mean(Theta3)),col=c("red","blue"),lwd=2)

###Histograma estimador 4

hist(Theta4,prob=TRUE,xlim=c(min(Theta4),max(Theta4)),main="Estimador 4")
abline(v=c(7,mean(Theta4)),col=c("red","blue"),lwd=2)

MeanT1 <- mean(Theta1)
MeanT2 <- mean(Theta2)
MeanT3 <- mean(Theta3)
MeanT4 <- mean(Theta4)

VarT1 <- var(Theta1)
VarT2 <- var(Theta2)
VarT3 <- var(Theta3)
VarT4 <- var(Theta4)

BiasT1 <- bias(Theta1)
BiasT2 <- bias(Theta2)
BiasT3 <- bias(Theta3)
BiasT4 <- bias(Theta4)

MSET1 <- MSE(Theta1)
MSET2 <- MSE(Theta2)
MSET3 <- MSE(Theta3)
MSET4 <- MSE(Theta4)

###Estimadores aplicados en las m muestras de tamaño 50

n <- 50
set.seed(7)
for (m in 1:m) {
  x <- rexp(n, rate=1/7)
  Theta1[m] <- theta_1(x)
  Theta2[m] <- theta_2(x)
  Theta3[m] <- theta_3(x)
  Theta4[m] <- theta_4(x)
}

###Histograma estimador 1

hist(Theta1,prob=TRUE,xlim=c(min(Theta1),max(Theta1)),main="Estimador 1")
abline(v=c(7,mean(Theta1)),col=c("red","blue"),lwd=2)

###Histograma estimador 2

hist(Theta2,prob=TRUE,xlim=c(min(Theta2),max(Theta2)),main="Estimador 2")
abline(v=c(7,mean(Theta2)),col=c("red","blue"),lwd=2)

###Histograma estimador 3

hist(Theta3,prob=TRUE,xlim=c(min(Theta3),max(Theta3)),main="Estimador 3")
abline(v=c(7,mean(Theta3)),col=c("red","blue"),lwd=2)

###Histograma estimador 4

hist(Theta4,prob=TRUE,xlim=c(min(Theta4),max(Theta4)),main="Estimador 4")
abline(v=c(7,mean(Theta4)),col=c("red","blue"),lwd=2)

MeanT1 <- mean(Theta1)
MeanT2 <- mean(Theta2)
MeanT3 <- mean(Theta3)
MeanT4 <- mean(Theta4)

VarT1 <- var(Theta1)
VarT2 <- var(Theta2)
VarT3 <- var(Theta3)
VarT4 <- var(Theta4)

BiasT1 <- bias(Theta1)
BiasT2 <- bias(Theta2)
BiasT3 <- bias(Theta3)
BiasT4 <- bias(Theta4)

MSET1 <- MSE(Theta1)
MSET2 <- MSE(Theta2)
MSET3 <- MSE(Theta3)
MSET4 <- MSE(Theta4)

###Estimadores aplicados en las m muestras de tamaño 100

n <- 100
set.seed(7)
for (m in 1:m) {
  x <- rexp(n, rate=1/7)
  Theta1[m] <- theta_1(x)
  Theta2[m] <- theta_2(x)
  Theta3[m] <- theta_3(x)
  Theta4[m] <- theta_4(x)
}

###Histograma estimador 1

hist(Theta1,prob=TRUE,xlim=c(min(Theta1),max(Theta1)),main="Estimador 1")
abline(v=c(7,mean(Theta1)),col=c("red","blue"),lwd=2)

###Histograma estimador 2

hist(Theta2,prob=TRUE,xlim=c(min(Theta2),max(Theta2)),main="Estimador 2")
abline(v=c(7,mean(Theta2)),col=c("red","blue"),lwd=2)

###Histograma estimador 3

hist(Theta3,prob=TRUE,xlim=c(min(Theta3),max(Theta3)),main="Estimador 3")
abline(v=c(7,mean(Theta3)),col=c("red","blue"),lwd=2)

###Histograma estimador 4

hist(Theta4,prob=TRUE,xlim=c(min(Theta4),max(Theta4)),main="Estimador 4")
abline(v=c(7,mean(Theta4)),col=c("red","blue"),lwd=2)

MeanT1 <- mean(Theta1)
MeanT2 <- mean(Theta2)
MeanT3 <- mean(Theta3)
MeanT4 <- mean(Theta4)

VarT1 <- var(Theta1)
VarT2 <- var(Theta2)
VarT3 <- var(Theta3)
VarT4 <- var(Theta4)

BiasT1 <- bias(Theta1)
BiasT2 <- bias(Theta2)
BiasT3 <- bias(Theta3)
BiasT4 <- bias(Theta4)

MSET1 <- MSE(Theta1)
MSET2 <- MSE(Theta2)
MSET3 <- MSE(Theta3)
MSET4 <- MSE(Theta4)

###Estimadores aplicados en las m muestras de tamaño 200

n <- 200
set.seed(7)
for (m in 1:m) {
  x <- rexp(n, rate=1/7)
  Theta1[m] <- theta_1(x)
  Theta2[m] <- theta_2(x)
  Theta3[m] <- theta_3(x)
  Theta4[m] <- theta_4(x)
}

###Histograma estimador 1

hist(Theta1,prob=TRUE,xlim=c(min(Theta1),max(Theta1)),main="Estimador 1")
abline(v=c(7,mean(Theta1)),col=c("red","blue"),lwd=2)

###Histograma estimador 2

hist(Theta2,prob=TRUE,xlim=c(min(Theta2),max(Theta2)),main="Estimador 2")
abline(v=c(7,mean(Theta2)),col=c("red","blue"),lwd=2)

###Histograma estimador 3

hist(Theta3,prob=TRUE,xlim=c(min(Theta3),max(Theta3)),main="Estimador 3")
abline(v=c(7,mean(Theta3)),col=c("red","blue"),lwd=2)

###Histograma estimador 4

hist(Theta4,prob=TRUE,xlim=c(min(Theta4),max(Theta4)),main="Estimador 4")
abline(v=c(7,mean(Theta4)),col=c("red","blue"),lwd=2)

MeanT1 <- mean(Theta1)
MeanT2 <- mean(Theta2)
MeanT3 <- mean(Theta3)
MeanT4 <- mean(Theta4)

VarT1 <- var(Theta1)
VarT2 <- var(Theta2)
VarT3 <- var(Theta3)
VarT4 <- var(Theta4)

BiasT1 <- bias(Theta1)
BiasT2 <- bias(Theta2)
BiasT3 <- bias(Theta3)
BiasT4 <- bias(Theta4)

MSET1 <- MSE(Theta1)
MSET2 <- MSE(Theta2)
MSET3 <- MSE(Theta3)
MSET4 <- MSE(Theta4)

###Estimadores aplicados en las m muestras de tamaño 500

n <- 500
set.seed(7)
for (m in 1:m) {
  x <- rexp(n, rate=1/7)
  Theta1[m] <- theta_1(x)
  Theta2[m] <- theta_2(x)
  Theta3[m] <- theta_3(x)
  Theta4[m] <- theta_4(x)
}

###Histograma estimador 1

hist(Theta1,prob=TRUE,xlim=c(min(Theta1),max(Theta1)),main="Estimador 1")
abline(v=c(7,mean(Theta1)),col=c("red","blue"),lwd=2)

###Histograma estimador 2

hist(Theta2,prob=TRUE,xlim=c(min(Theta2),max(Theta2)),main="Estimador 2")
abline(v=c(7,mean(Theta2)),col=c("red","blue"),lwd=2)

###Histograma estimador 3

hist(Theta3,prob=TRUE,xlim=c(min(Theta3),max(Theta3)),main="Estimador 3")
abline(v=c(7,mean(Theta3)),col=c("red","blue"),lwd=2)

###Histograma estimador 4

hist(Theta4,prob=TRUE,xlim=c(min(Theta4),max(Theta4)),main="Estimador 4")
abline(v=c(7,mean(Theta4)),col=c("red","blue"),lwd=2)

MeanT1 <- mean(Theta1)
MeanT2 <- mean(Theta2)
MeanT3 <- mean(Theta3)
MeanT4 <- mean(Theta4)

VarT1 <- var(Theta1)
VarT2 <- var(Theta2)
VarT3 <- var(Theta3)
VarT4 <- var(Theta4)

BiasT1 <- bias(Theta1)
BiasT2 <- bias(Theta2)
BiasT3 <- bias(Theta3)
BiasT4 <- bias(Theta4)

MSET1 <- MSE(Theta1)
MSET2 <- MSE(Theta2)
MSET3 <- MSE(Theta3)
MSET4 <- MSE(Theta4)

###Estimadores aplicados en las m muestras de tamaño 1000

n <- 1000
set.seed(7)
for (m in 1:m) {
  x <- rexp(n, rate=1/7)
  Theta1[m] <- theta_1(x)
  Theta2[m] <- theta_2(x)
  Theta3[m] <- theta_3(x)
  Theta4[m] <- theta_4(x)
}

###Histograma estimador 1

hist(Theta1,prob=TRUE,xlim=c(min(Theta1),max(Theta1)),main="Estimador 1")
abline(v=c(7,mean(Theta1)),col=c("red","blue"),lwd=2)

###Histograma estimador 2

hist(Theta2,prob=TRUE,xlim=c(min(Theta2),max(Theta2)),main="Estimador 2")
abline(v=c(7,mean(Theta2)),col=c("red","blue"),lwd=2)

###Histograma estimador 3

hist(Theta3,prob=TRUE,xlim=c(min(Theta3),max(Theta3)),main="Estimador 3")
abline(v=c(7,mean(Theta3)),col=c("red","blue"),lwd=2)

###Histograma estimador 4

hist(Theta4,prob=TRUE,xlim=c(min(Theta4),max(Theta4)),main="Estimador 4")
abline(v=c(7,mean(Theta4)),col=c("red","blue"),lwd=2)

MeanT1 <- mean(Theta1)
MeanT2 <- mean(Theta2)
MeanT3 <- mean(Theta3)
MeanT4 <- mean(Theta4)

VarT1 <- var(Theta1)
VarT2 <- var(Theta2)
VarT3 <- var(Theta3)
VarT4 <- var(Theta4)

BiasT1 <- bias(Theta1)
BiasT2 <- bias(Theta2)
BiasT3 <- bias(Theta3)
BiasT4 <- bias(Theta4)

MSET1 <- MSE(Theta1)
MSET2 <- MSE(Theta2)
MSET3 <- MSE(Theta3)
MSET4 <- MSE(Theta4)