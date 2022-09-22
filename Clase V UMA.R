library(tidyverse)

baseExamen=read.csv(file.choose(),encoding = 'UTF-8')

x=0.3
dnorm(x)

x=seq(-4,4,by=0.1)
y=dnorm(x)

ggplot()+geom_line(aes(x=x, y=y))

y2=pnorm(x)
ggplot()+geom_line(aes(x=x, y=y2))

q=seq(0.01,0.99,by=0.01)
qx=qnorm(q)

tabla=tibble(q,qx)

simulacion=rnorm(1000000)

ggplot()+geom_histogram(aes(x=simulacion, y=stat(density)),fill='lightblue', colour='gray',alpha=0.5)+
  geom_line(aes(x=x, y=y))

datos=read.csv("D:/Clases Impartidas/Estadistica/basepensionados.csv")

cuantil=function(x,proba){
  ordenados=sort(x)
  n=length(x)
  return(ordenados[n*proba])
}

cuantil(datos$edad,0.05)

quantile(datos$edad,0.05)


temblores=read.csv(file.choose())

temblores$Fecha=as.Date(temblores$Fecha)

temblores$mes=months(temblores$Fecha)

conteo=as.data.frame(table(temblores$mes))

ggplot(conteo)+geom_col(aes(Var1, Freq),color='lightblue',fill='lightblue')+
  ggtitle("Sismos por mes")+
  geom_hline(yintercept = mean(conteo$Freq), color='darkgreen')+
  geom_hline(yintercept = mean(conteo$Freq)+2*sd(conteo$Freq), color='red')+
  geom_hline(yintercept = mean(conteo$Freq)-2*sd(conteo$Freq), color='red')

filtrado=temblores[temblores$Magnitud>=5.0,]

conteo=as.data.frame(table(filtrado$mes))

ggplot(conteo)+geom_col(aes(Var1, Freq),color='lightblue',fill='lightblue')+
  ggtitle("Sismos por mes")+
  geom_hline(yintercept = mean(conteo$Freq), color='darkgreen')+
  geom_hline(yintercept = mean(conteo$Freq)+2*sd(conteo$Freq), color='red')+
  geom_hline(yintercept = mean(conteo$Freq)-2*sd(conteo$Freq), color='red')

graficaFrecuencias=function(temblores,magnitud){
  filtrado=temblores[temblores$Magnitud>=magnitud,]
  
  conteo=as.data.frame(table(filtrado$mes))
  
  ggplot(conteo)+geom_col(aes(Var1, Freq),color='lightblue',fill='lightblue')+
    ggtitle(paste("Sismos por mes con magnitud mayor o igual a ",magnitud))+
    geom_hline(yintercept = mean(conteo$Freq), color='darkgreen')+
    geom_hline(yintercept = mean(conteo$Freq)+2*sd(conteo$Freq), color='red')+
    geom_hline(yintercept = mean(conteo$Freq)-2*sd(conteo$Freq), color='red')
}

graficaFrecuencias(temblores,6.0)

magnitudes=seq(0,9,by=1)

for (i in magnitudes){
  print(graficaFrecuencias(temblores,i))
}