library(tidyverse)
####ejemplo Normal
simulacion=rnorm(100,2,1)

m=mean(simulacion)
s=sd(simulacion)

x=seq(-1,5,by=0.1)
y=dnorm(x,m,s)

ggplot()+geom_histogram(aes(x=simulacion, y=stat(density)),fill='lightblue', colour='gray',alpha=0.5)+
  geom_line(aes(x=x, y=y))

####ejemplo Binomial theta1=n theta2=p

simulacionBin=rbinom(35,5,0.3)

ggplot()+geom_histogram(aes(x=simulacionBin, y=stat(density)),fill='lightblue', colour='gray',alpha=0.5)
  
m1=mean(simulacionBin)
m2=sum(simulacionBin^2)/length(simulacionBin)

theta2=1-(m2-m1^2)/m1
theta1=m1/theta2
theta1=round(theta1,digits=0)

  
x=seq(0,20, by=1)
y=dbinom(x,theta1,theta2)

ggplot()+geom_histogram(aes(x=simulacionBin, y=stat(density)),fill='lightblue', colour='gray',alpha=0.5)+
  geom_line(aes(x=x, y=y))

####ejemplo Poisson

simulacionPoi=rpois(100000,0.6)

ggplot()+geom_histogram(aes(x=simulacionPoi, y=stat(density)),fill='lightblue', colour='gray',alpha=0.5)

lambda=mean(simulacionPoi)

x=seq(0,10, by=1)
y=dpois(x,lambda)

ggplot()+geom_histogram(aes(x=simulacionPoi, y=stat(density)),fill='lightblue', colour='gray',alpha=0.5)+
  geom_line(aes(x=x, y=y))

