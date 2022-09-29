library(tidyverse)

datos=read.csv("D:/Carlos/Personales/Estadistica/basepensionados.csv")

datos=as_tibble(datos)

mean(datos$edad)
sd(datos$edad)

n=50

m1=sample(datos$edad,n)
m2=datos[sample(nrow(datos),n),]$edad
m3=sample_n(datos,n)$edad
m4=sample_n(datos,n)$edad
m5=sample_n(datos,n)$edad

muestras=cbind(m1,m2,m3,m4,m5)
nombres=c('m1','m2','m3','m4','m5')

for (muestra in nombres){
  media=mean(muestras[,muestra])
  desviacion=sd(muestras[,muestra])
  print(paste('Muestra: ',muestra,' media: ',media,'desviación: ',desviacion))
}


