library(tidyverse)
library(MASS)
library(survival)
library(fitdistrplus)
library(datasets)
library(magrittr)
library(openxlsx)

#Punto 4
data=read.xlsx("C:/Users/kaido/Downloads/dataset_trabajo1.xlsx",startRow=1,colNames=TRUE)
#Ver tabla
View(data)
#Fijar semilla
data
dif_p_m <- as.numeric( data$fage-data$mage)
data <- cbind ( data , dif_p_m )
x = dif_p_m
#Valor crítico
c=(mean(x)-0)/(sd(x)/sqrt(42))
c
#Nivel de significancia 95%
qnorm(0.05,mean=0,sd=1)
