library(tidyverse)

simulacion=rnorm(100000)

medias=NULL
medias.2=NULL
varianzas=NULL
varianzas.2=NULL

for (i in 1:10) {
  muestra=sample(simulacion, 100)
  n=length(muestra)
  media=mean(muestra)
  media2=sum(muestra)/(n-5)
  varianza=var(muestra)
  varianza.2=sum((muestra-media)^2)/n
  medias=c(medias,media)
  medias.2=c(medias.2,media2)
  varianzas=c(varianzas,varianza)
  varianzas.2=c(varianzas.2,varianza.2)
}
mean(medias)
mean(medias.2)
mean(varianzas)
mean(varianzas.2)

ggplot()+geom_histogram(aes(x=medias),fill='lightblue', colour='gray',alpha=0.5)+
  geom_histogram(aes(x=medias.2),fill='lightsalmon', colour='gray', alpha=0.5)

ggplot()+geom_histogram(aes(x=varianzas),fill='lightblue', colour='gray',alpha=0.5)+
  geom_histogram(aes(x=varianzas.2),fill='lightsalmon', colour='gray', alpha=0.5)