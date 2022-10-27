library(tidyverse)

simulacion=rnorm(100000)

medias=NULL
medias.2=NULL
varianzas=NULL
varianzas.2=NULL

for (i in 1:1000) {
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
mm1=mean(medias)
mm2=mean(medias.2)

mv1=mean(varianzas)
mv2=mean(varianzas.2)

vm1=var(medias)
vm2=var(medias.2)

vv1=var(varianzas)
vv2=var(varianzas.2)

cadena_medias_medias=paste("la media de las medias es:", mm1,'la media de la otra media es:', mm2)
cadena_varianzas_medias=paste("la varianza de las medias es:", vm1,'la varianza de la otra media es:', vm2)

cadena_medias_varianzas=paste("la media de las varianzas es:", mv1,'la media de la otra varianza es:', mv2)
cadena_varianzas_varianzas=paste("la varianza de las varianzas es:", vv1,'la varianza de la otra varianza es:', vv2)

print(cadena_medias_medias)
print(cadena_varianzas_medias)
print(cadena_medias_varianzas)
print(cadena_medias_varianzas)

ggplot()+geom_histogram(aes(x=medias),fill='lightblue', colour='gray',alpha=0.5)+
  geom_histogram(aes(x=medias.2),fill='lightsalmon', colour='gray', alpha=0.5)

ggplot()+geom_histogram(aes(x=varianzas),fill='lightblue', colour='gray',alpha=0.5)+
  geom_histogram(aes(x=varianzas.2),fill='lightsalmon', colour='gray', alpha=0.5)