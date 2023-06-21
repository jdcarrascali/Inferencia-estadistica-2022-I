############# Bonus Parte B ###################

##### Instalación de Paquetes #######

install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("GGally")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("PerformanceAnalytics")

###### Carga de Paquetes ######

library(readxl)
library(dplyr)
library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)

###### Importar la Base de Datos #######

library(readxl)
datos <- read_excel("C:/Users/ander/OneDrive - Universidad Nacional de Colombia/Documentos/(2022-01) Cuarto Semestre/Inferencia Estadística/Proyecto/Proyecto parte B/dataset_trabajo1.xlsx")
View(datos)
attach(datos)
datos

##### Punto 7 #########

correlacion <- round(cor(datos$mnocig,datos$fnocig),2)
correlacion
plot(x = datos$mnocig, y = datos$fnocig, col = "blue",main = "Diagrama - Correlación Cigarrillos Fumados Madre vs Padre",xlab="Cigarrillos Fumados Madre",ylab="Cigarrillos Fumados Padre")
abline(lm(datos$mnocig~datos$fnocig),col = "orange")

######## Punto 8 ########

correlacion1 <- cor(datos$smoker,datos$lowbwt, method = 'spearman')
correlacion1
plot(x = datos$smoker, y = datos$lowbwt, col = "blue",main = "Diagrama - Correlación Madre Fumadora vs Bebé de bajo peso",xlab="Madre Fumadora",ylab="Bebé Bajo Peso")
abline(lm(datos$smoker~datos$lowbwt),col = "orange")

####### Punto 9 #########
anova <- aov(datos$race~datos$Gestation)
summary(anova)

