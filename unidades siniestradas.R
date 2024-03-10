library(dplyr)
library(zoo)
library(ggplot2)
# install.packages(c("zoo", "readxl", "dplyr"))
base <- read.csv("C:\\Users\\Manuel\\OneDrive - Universidad Autonoma de Guadalajara\\MODELACION ACTUARIAL\\datos_examen-1.csv")


#a. Calcular la media muestral del número de unidades siniestradas por siniestro.
magnitudes <- as.numeric(base$costo)
mean(magnitudes)
#en promedio son 65 el numero de unidades siniestradas por siniestro 

#b. Calcular la mediana de esta misma variable.
median(magnitudes)
#Hay 50% de probabilidad que una unidad siniestrada por siniestra sea de 1

#c. Calcular la probabilidad empírica de que se observen más de 3 unidades afectadas  en un siniestro
z <- base %>%
  count(Poliza)
ECDF2 <- ecdf(z$n)
mean(magnitudes>3)
1-ecdf(magnitudes)(3)

#En promedio hay un 14.09% de probabilidad que haya mas de 3 unidades siniestradas por siniestro


z <- base %>%
  count(Poliza)
ECDF2 <- ecdf(z$n)
ggplot() +
  geom_step(aes(x = z$n, y = ECDF2(z$n)))

#e. Calcular la probabilidad de observar entre 200 y 500 siniestros por póliza.
ECDF2(500)-ECDF2(200)
#10% es la probabilidad que el numero de siniestros por poliza este entre 200 y 500

#f. Calcular la probabilidad de observar más de 1000 siniestros por póliza, dado que se  rebasaron los 500 siniestros por póliza.
mean(z$n[z$n>500]>1000)
#El evento de tener mas de 1000 siniestros por poliza, dado que haya habido mas de 500 siniestros por poliza se presenta en la mitad de los casos

#g. Calcular el cuantil 0.95 de los siniestros por póliza.
quantile(ECDF2, 0.95, type = 1)

#El 95% de las polizas tienen igual o menos de 654.5 siniestros
#h. Calcular el rango intercuartil del número de siniestros por póliza.
quantile(ECDF2, .75)-quantile(ECDF2, .25)
#La mitad de los datos estan abarcados en un intervalo de 88

#i. Calcular el cuantil 0.995 del monto indemnizado por unidad siniestrada
#IndemnizacionFondoPesosMex
#El 99.5% de las polizas tendran 33600 o menos de monto indemnizado por unidad
q <- base$IndemnizacionFondoPesosMex
u <- base$costo
a = q/u
quantile(a, 0.995)

#j. Calcular el promedio del monto indemnizado por unidad siniestrada, sabiendo que  el monto observado será superior al cuantil 0.995
#P(A/B)=P(ainterseccionb)/P(B)
mean(a[a>33600])
#En promedio el monto indemnizado en el caso de que este en el 99.5% de las polizas es de 42,361.53
