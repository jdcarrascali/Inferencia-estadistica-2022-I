############ Punto 3 #################

######## Importar Base de Datos #########

library(readxl)
data <- read_excel("C:/Users/ander/OneDrive - Universidad Nacional de Colombia/Documentos/(2022-01) Cuarto Semestre/Inferencia Estadística/Proyecto/Proyecto parte B/dataset_trabajo1.xlsx")
View(data)

######### Punto 3 #############

t.test(x=data$Birthweight, alternative = 'two.sided', mu = 3.4, conf.level = 0.95)

######## Punto 4 ##########

z_critico <- (22/42 - 0.40) / sqrt(0.40 * (1 - 0.40) / 200)
z_critico

####### Punto 5 ###########

x <- data[data$smoker==0, 2]
x
m_1 <- mean(x$Length)

y <- data[data$smoker==1, 2]
y
m_2 <- mean(y$Length) 

t.test(x, y, alternative='greater', mu=0, var.equal=TRUE, conf.level=0.95)