#Se busca a partir del dataset Titanic realizar las siguientes gráficas:
#    a)  Histograma, los hombres y mujeres involucrados.
#    b)  Gráfica de barras de personas por cada clase.


#Cargamos las librerias a utilizar
library(tidyverse)
library(moments)
library(titanic)
library(agricolae)
library(reshape2)
library(gridExtra)

#Carga del dataset titanic
data=titanic_train
str(data)
data<-na.omit(data)
names(data)
num<-data[,c(3,6,7,8,10)]
chr<-data[,c(2,5,12)]

#Nombramos las columnas
colnames(num)<-c("Clase_Pasajero","Edad","Pariente_Masculino",
                 "Pariente_Femenino","Tarifa_Pasaje")
names(num)
colnames(chr)<-c("Sobreviviente","Sexo","Puerto_Embarcadero")
names(chr)

#Renombramos los datos male y female al español
chr[chr$Sexo=="female","Sexo"]<-"Mujer"
chr[chr$Sexo=="male","Sexo"]<-"Hombre"

#Funcion para crear el histograma
resumen1<-function(x){
  round(cbind(frecuencia =table(x),relativo=prop.table(table(x))),3)}

df1<-data.frame(resumen1(chr$Sexo))
df1

# Graficando el histogrma
ggplot(chr,aes(Sexo)) +geom_bar( fill="#fa8072" )+
  labs(title="Histograma", y="Frecuencia", x="Sexo")+
  theme_classic(base_size=15)

#Funcion para crear la grafica de barras
resumen2<-function(y){
  q<-quantile(y, prob=c(0.25,0.5,0.75))
  nombre<-c("min","cuart 1","media","cuart 2","cuart 3","max","sd","asimetria","kurtorsi")
  valor<-round(c(min(y),q[1],mean(y),q[2],q[3],max(y),sd(y),skewness(y),kurtosis(y)),3)
  data.frame(nombre,valor)}

resumen2(num$Clase_Pasajero)

resumen1(num$Clase_Pasajero)

# Graficando
ggplot(num,aes(Clase_Pasajero)) +geom_bar( fill="#fa8072" )+
  labs(title="Grafica de barras", y="Frecuencia", x="Clase_Pasajero") + theme_classic(base_size=15)
