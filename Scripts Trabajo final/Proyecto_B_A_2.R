###Valores a tener en cuenta

alpha <- 0.05
a<-seq(from = 0.05,to = 0.95,by = (0.05))
b<-c(5,10,50,100,200,500,1000)
z_0.975 <- qnorm(0.975)

##Funciones que se usarán en el algoritmo

p_n <- function(x){
  mean(x)
}
se2 <- function(x){
  sqrt((p_n(x)*(1-p_n(x)))/n)
}
error2 <- function(x){
  z_0.975*(se2(x))
}

##Límite inferior y límite superior de la posibilidad 1

l1 <- function(x){
  ((2*n*p_n(x)+(z_0.975)^2)-(z_0.975)*sqrt(4*n*p_n(x)+(z_0.975)^2-4*n*(p_n(x))^2))/(2*(n+(z_0.975)^2))
}
u1 <- function(x){
  ((2*n*p_n(x)+(z_0.975)^2)+(z_0.975)*sqrt(4*n*p_n(x)+(z_0.975)^2-4*n*(p_n(x))^2))/(2*(n+(z_0.975)^2))
}

##Límite inferior y límite superior de la posibilidad 2

l2 <- function(x){
  p_n(x)-error2(x)
}
u2 <- function(x){
  p_n(x)+error2(x)
}

##Límite inferior y límite superior de la posibilidad 3

l3 <- function(x){
  (sin(asin(sqrt(p_n(x)))-(z_0.975)/(2*sqrt(n))))^2
}
u3 <- function(x){
  (sin(asin(sqrt(p_n(x)))+(z_0.975)/(2*sqrt(n))))^2
}

##Longitud del intervalo de las 3 posibilidades

long1 <- function (x){
  u1(x)-l1(x)
}
long2 <- function (x){
  u2(x)-l2(x)
}
long3 <- function (x){
  u3(x)-l3(x)
}

##Aquí guardaremos información pertinente para los plots de otros puntos, los cuales no pudimos incluir en el algoritmo

for (n in b) {
  Cobertura1<-paste("Cob","P1",n,sep="_") 
  assign(Cobertura1,numeric())
  Cobertura2<-paste("Cob","P2",n,sep="_")
  assign(Cobertura2,numeric())
  Cobertura3<-paste("Cob","P3",n,sep="_")
  assign(Cobertura3,numeric())
  
  Longitud1<-paste("Long","P1",n,sep="_")
  assign(Longitud1,numeric())
  Longitud2<-paste("Long","P2",n,sep="_")
  assign(Longitud2,numeric())
  Longitud3<-paste("Long","P3",n,sep="_")
  assign(Longitud3,numeric())
}

##ALGORITMO DEL A2

set.seed(129)
for(p in a){
  for(n in b){
    
    ##Aquí guardaremos las observaciones del algoritmo
    
    P1<-paste("P1",n,p,sep="_")  
    assign(P1,numeric())
    
    P2<-paste("P2",n,p,sep="_")
    assign(P2,numeric())
    
    P3<-paste("P3",n,p,sep="_")
    assign(P3,numeric())
    
    
    for(i in 1:1000){
      
      x<-rbinom(n,1,p)
      
      ##Valores que se asignaran dependiendo de si se cumple o no la condición
      
      x_1 <- c(eval(parse(text=P1)),0)
      x_2 <- c(eval(parse(text = P1)),long1(x))
      y_1 <- c(eval(parse(text=P2)),0)
      y_2 <- c(eval(parse(text = P2)),long2(x))
      z_1 <- c(eval(parse(text=P3)),0)
      z_2 <- c(eval(parse(text=P3)),long3(x))
      
      if(l1(x)>=p | u1(x)<=p){
        assign(P1,x_1)
      }else{
        assign(P1,x_2)
      }
      if(l2(x)>=p | u2(x)<=p){
        assign(P2,y_1)
      }else{
        assign(P2,y_2)
      }
      if(l3(x)>=p | u3(x)<=p){
        assign(P3,z_1)
      }else{
        assign(P3,z_2)
      }
    }
    
    ##Valores que se asignaran a algunos objetos de tipo numérico, algunos necesarios para los plots
    
    c_1 <-(sum(eval(parse(text=P1))!=0)/1000)
    c_2 <-(sum(eval(parse(text=P2))!=0)/1000)
    c_3 <-(sum(eval(parse(text=P3))!=0)/1000)
    c_4 <-c(eval(parse(text=paste("Cob","P1",n,sep="_"))),c_1)
    c_5 <-c(eval(parse(text=paste("Cob","P2",n,sep="_"))),c_2)
    c_6 <-c(eval(parse(text=paste("Cob","P3",n,sep="_"))),c_3)
    
    l_1 <-sum(eval(parse(text=P1)))/sum(eval(parse(text=P1))!=0)
    l_2 <-sum(eval(parse(text=P2)))/sum(eval(parse(text=P2))!=0)
    l_3 <-sum(eval(parse(text=P3)))/sum(eval(parse(text=P3))!=0)
    l_4 <-c(eval(parse(text=paste("Long","P1",n,sep="_"))),l_1)
    l_5 <-c(eval(parse(text=paste("Long","P2",n,sep="_"))),l_2)
    l_6 <-c(eval(parse(text=paste("Long","P3",n,sep="_"))),l_3)
    
    ##Donde se guarda cada observación de la cobertura promedio, para cada combinación de n y p
    
    CP_I1<-paste("Cob","Prom",P1,sep="_")
    assign(CP_I1,c_1)
    CP_I2<-paste("Cob","Prom",P2,sep="_")
    assign(CP_I2,c_2)
    CP_I3<-paste("Cob","Prom",P3,sep="_")
    assign(CP_I3,c_3)
    
    ##Donde se guarda cada observación de la cobertura promedio, para cada combinación de n y p

    LL_I1<-paste("Long","Prom",P1,sep="_")
    assign(LL_I1,l_1)
    LL_I2<-paste("Long","Prom",P2,sep="_")
    assign(LL_I2,l_2)
    LL_I3<-paste("Long","Prom",P3,sep="_")
    assign(LL_I3,l_3)
    
    ##Información que usaremos exclusivamente para los plots
    
    assign(paste("Cob","P1",n,sep="_"),c_4)
    assign(paste("Cob","P2",n,sep="_"),c_5)
    assign(paste("Cob","P3",n,sep="_"),c_6)

    
    assign(paste("Long","P1",n,sep="_"),l_4)
    assign(paste("Long","P2",n,sep="_"),l_5)
    assign(paste("Long","P3",n,sep="_"),l_6)
  }
}

##PLOTS DEL A3

##Cobertura promedio para cada tamaño de muestra con la posibilidad 1 en función de cada parámetro p

plot(a,Cob_P1_5,col=1,ylim=c(0.8,1))
lines(a,Cob_P1_5,col=1)
lines(a,Cob_P1_10,col=2)
lines(a,Cob_P1_50,col=3)
lines(a,Cob_P1_100,col=4)
lines(a,Cob_P1_200,col=5)
lines(a,Cob_P1_500,col=6)
lines(a,Cob_P1_1000,col=7)
legend(locator(1),c("n=5","n=10","n=50","n=100","n=200","n=500","n=1000"),col=c("1","2","3","4","5","6","7"),cex = 0.8,lwd=2)


##Cobertura promedio para cada tamaño de muestra con la posibilidad 2 n función de cada parámetro p

plot(a,Cob_P2_5,col=1)
lines(a,Cob_P2_5,col=1)
lines(a,Cob_P2_10,col=2)
lines(a,Cob_P2_50,col=3)
lines(a,Cob_P2_100,col=4)
lines(a,Cob_P2_200,col=5)
lines(a,Cob_P2_500,col=6)
lines(a,Cob_P2_1000,col=7)
legend(locator(1),c("n=5","n=10","n=50","n=100","n=200","n=500","n=1000"),col=c("1","2","3","4","5","6","7"),cex = 0.8,lwd=2)

##Longitud promedio para cada tamaño de muestra con la posibilidad 1 en función de cada parámetro p

plot(a,Long_P1_5,col=1,ylim=c(0,0.8))
lines(a,Long_P1_5,col=1)
lines(a,Long_P1_10,col=2)
lines(a,Long_P1_50,col=3)
lines(a,Long_P1_100,col=4)
lines(a,Long_P1_200,col=5)
lines(a,Long_P1_500,col=6)
lines(a,Long_P1_1000,col=7)
legend(locator(1),c("n=5","n=10","n=50","n=100","n=200","n=500","n=1000"),col=c("1","2","3","4","5","6","7"),cex = 0.7,lwd=2)

##Longitud promedio para cada tamaño de muestra con la posibilidad 2 en función de cada parámetro p

plot(a,Long_P2_5,col=1,ylim=c(0,1))
lines(a,Long_P2_5,col=1)
lines(a,Long_P2_10,col=2)
lines(a,Long_P2_50,col=3)
lines(a,Long_P2_100,col=4)
lines(a,Long_P2_200,col=5)
lines(a,Long_P2_500,col=6)
lines(a,Long_P2_1000,col=7)
legend(locator(1),c("n=5","n=10","n=50","n=100","n=200","n=500","n=1000"),col=c("1","2","3","4","5","6","7"),cex = 0.6,lwd=2)

##PLOTS DEL A4


##Cobertura promedio de las 3 posibilidades para diferentes tamaños de muestra

#Tamaño de muestra n=5

plot(a,Cob_P1_5,col=1,ylim=c(0.8,1))
lines(a,Cob_P1_5,col=1)
lines(a,Cob_P2_5,col=2)
lines(a,Cob_P3_5,col=3)
legend(locator(1),c("P1","P2","P3"),col=c("1","2","3"),cex = 1,lwd=2)

#Tamaño de muestra n=50

plot(a,Cob_P1_50,col=1,ylim=c(0.8,1))
lines(a,Cob_P1_50,col=1)
lines(a,Cob_P2_50,col=2)
lines(a,Cob_P3_50,col=3)
legend(locator(1),c("P1","P2","P3"),col=c("1","2","3"),cex = 1,lwd=2)

#Tamaño de muestra n=200

plot(a,Cob_P1_200,col=1,ylim=c(0.8,1))
lines(a,Cob_P1_200,col=1)
lines(a,Cob_P2_200,col=2)
lines(a,Cob_P3_200,col=3)
legend(locator(1),c("P1","P2","P3"),col=c("1","2","3"),cex = 1,lwd=2)

#Tamaño de muestra n=1000

plot(a,Cob_P1_1000,col=1,ylim=c(0.8,1))
lines(a,Cob_P1_1000,col=1)
lines(a,Cob_P2_1000,col=2)
lines(a,Cob_P3_1000,col=3)
legend(locator(1),c("P1","P2","P3"),col=c("1","2","3"),cex = 1,lwd=2)


##Longitud promedio de las 3 posibilidades para diferentes tamaños de muestra

#Tamaño de muestra n=5

plot(a,Long_P1_5,col=1,ylim=c(0,0.8))
lines(a,Long_P1_5,col=1)
lines(a,Long_P2_5,col=2)
lines(a,Long_P3_5,col=3)
legend(locator(1),c("P1","P2","P3"),col=c("1","2","3"),cex = 1,lwd=2)

#Tamaño de muestra n=50

plot(a,Long_P1_50,col=1,ylim=c(0,0.4))
lines(a,Long_P1_50,col=1)
lines(a,Long_P2_50,col=2)
lines(a,Long_P3_50,col=3)
legend(locator(1),c("P1","P2","P3"),col=c("1","2","3"),cex = 1,lwd=2)

#Tamaño de muestra n=200

plot(a,Long_P1_200,col=1,ylim=c(0,0.2))
lines(a,Long_P1_200,col=1)
lines(a,Long_P2_200,col=2)
lines(a,Long_P3_200,col=3)
legend(locator(1),c("P1","P2","P3"),col=c("1","2","3"),cex = 1,lwd=2)

#Tamaño de muestra n=1000

plot(a,Long_P1_1000,col=1,ylim=c(0,0.1))
lines(a,Long_P1_1000,col=1)
lines(a,Long_P2_1000,col=2)
lines(a,Long_P3_1000,col=3)
legend(locator(1),c("P1","P2","P3"),col=c("1","2","3"),cex = 1,lwd=2)

