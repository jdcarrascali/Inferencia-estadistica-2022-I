data=read.xlsx("C:/Users/kaido/Downloads/dataset_trabajo1.xlsx",startRow=1,colNames=TRUE)
#Ver tabla
View(data)
#Fijar semilla
set.seed(233)
#Variable aleatoria de interes
x=data$Gestation
#Submuestra Punto C 
submuestra_c<-data %>%filter(Gestation <40)
x=submuestra_c$Headcirc
#Submuestra Punto D
submuestra_d<-data %>%filter(smoker ==1)
submuestra_d
x=submuestra_d$Gestation
x
#install.packages("dplyr")
# installed.packages("survival")
# installed.packages("fitdistrplus")
#install.packages("tidyverse")
library(openxlsx)
library(tidyverse)
library(MASS)
library(survival)
library(readr)
library(fitdistrplus)
library(datasets)
library(dplyr)

hist(x,main = "Histograma",xlab="Madre fumadora y > 40 sem.")
#Mutate



## Paso (1) Plot Cullen y Frey
descdist( data = x , discrete = FALSE)
descdist(data = x, discrete = FALSE, boot=1000)

## Step (2) Fit

fitdist(x,"beta")
max(x)
logis_=fitdist(x,"logis")
unif_=fitdist(x,"unif")
normal_ = fitdist(x, "norm")
weibull_ = fitdist(x, "weibull")
gamma_ = fitdist(x, "gamma")

plot(logis_)
plot(unif_)
plot(normal_)
plot(weibull_)
plot(gamma_)
prueba<-gofstat(logis_)
prueba$kstest
prueba$chisqpvalue
print(logis_)
print(unif_)
print(normal_)
print(weibull_)
print(gamma_)
summary(normal_)
summary(weibull_)
summary(gamma_)
#Punto B 2

#Peso promedio bebés
x=data$Birthweight

#Verificamos si los datos provienen de una dist. normal
normal_ = fitdist(x, "norm")
plot(normal_)  #Evidencia gráfica
prueba<-gofstat(normal_)
prueba$kstest
prueba$chisqpvalue

#Cálculo intervalo de confianza 
x_barra=mean(x)
sd=sqrt(var(x))
x_barra-qt(0.995,df=41)*sd/sqrt(42) #Límite inferior
x_barra+qt(0.995,df=41)*sd/sqrt(42) #Límite superior

#Varianza educación del padre
x=data$fedyrs
#Verificamos si los datos provienen de una dist. normal
normal_ = fitdist(x, "norm")
plot(normal_)  #Evidencia gráfica
prueba<-gofstat(normal_)
prueba$kstest
prueba$chisqpvalue
#Ajuste alternativo
descdist( data = x , discrete = FALSE)
descdist(data = x, discrete = FALSE, boot=1000)
#Bondad de ajuste
unif_=fitdist(x,"unif")
plot(unif_)
prueba<-gofstat(unif_)
prueba$kstest
prueba$chisqpvalue

#Coeficiente de variación edad madre
x=data$mage
#Verificamos si los datos provienen de una dist. normal
normal_ = fitdist(x, "norm")
plot(normal_)  #Evidencia gráfica
prueba<-gofstat(normal_)
prueba$kstest
prueba$chisqpvaluepbin

#Proporción padre fumador
fnocig<-as.numeric(data$fnocig>0)
data<-cbind(data,fnocig)
x=fnocig
descdist( data = x , discrete = FALSE)
descdist(data = x, discrete = FALSE, boot=1000)

