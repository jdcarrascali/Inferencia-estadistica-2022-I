#Fijar semilla
set.seed(233)
#Leer dataset
data=read.xlsx("C:/Users/kaido/Downloads/dataset_trabajo1.xlsx",startRow=1,colNames=TRUE)
#Definir variabl e histograma
x=data$mage
hist(x)
#Estimación teorica variable de interes
v_o=sd(x)/mean(x)
v_o
#Estimacion para variable padre fumador
fnocig<-as.numeric(data$fnocig>0)
data<-cbind(data,fnocig)
x=fnocig
v_0=length(x[x==1])/42

#Generacion muestras bootstrap
bootstrap<- replicate(n=1000,sample(x,replace=TRUE))
#Cada columna representa una submuestra
dim(bootstrap)

#Boostrap_1
#Función generadora de estimadores
x_i=NULL
var_1<-function(n){  
  for (i in 1:42){
    x_i<-append(x_i,bootstrap[i,n])
  }
  return(length(x_i[x_i==1])/42)#Cambiar estadistica para cada caso
}
#Función que almacena estos estimadores
y_i=NULL
for (i in 1:1000){
  y_i=append(y_i,var_1(i))
}
hist(y_i)
#Limite inferior
quantile(y_i,.0275)
#Limite superior
quantile(y_i,.975)
#Boostrap_2
#Función generadora de estimadores
x_i=NULL
var_2<-function(n){  
  for (i in 1:42){
    x_i<-append(x_i,bootstrap[i,n])
  }
  return(length(x_i[x_i==1])/42-v_0)#Cambiar estadistica para cada caso
}
#Función que almacena estos estimadores
y_i=NULL
for (i in 1:1000){
  y_i=append(y_i,var_2(i))
}
hist(y_i)
#Limite inferior
v_o-quantile(y_i,.975)
#Limite superior
v_o-quantile(y_i,.0275)
