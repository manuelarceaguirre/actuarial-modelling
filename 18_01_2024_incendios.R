library(readxl)
library(dplyr)
library(zoo)
library(ggplot2)
# install.packages(c("zoo", "readxl", "dplyr"))
base_incendios <- read_excel("D:/UAG/Modelación actuarial/2024/Material/bases_ejemplos/siniestralidad_incendios.xlsx")

# ¿Qué cuantil es la media?
mean(base_incendios$monto<=mean(base_incendios$monto,
                                na.rm = T), 
     na.rm = T)

# N = número de siniestros al mes

base_incendios <- base_incendios %>% 
  mutate(mes_siniestro = as.yearmon(
    fecha_siniestro))

siniestros_mes <- base_incendios %>% 
  count(mes_siniestro) %>%
  # group_by(mes_siniestro) %>% 
  # summarise(n = n())
  arrange(mes_siniestro)

meses_datos <- seq.Date(from = as.Date("2014-01-01"),
                        to = as.Date("2021-12-31"),
                        by = "month")

meses_datos <- data.frame(
  mes_siniestro = as.yearmon(meses_datos))

siniestros_mes_completo <- meses_datos %>% 
  left_join(siniestros_mes,
            by = "mes_siniestro") %>% 
  mutate(n = ifelse(is.na(n), 0, n))

N <- siniestros_mes_completo$n
hist(N)
summary(N)

# Obteniendo la función de distribución empírica:

ecdf_N <- ecdf(N)

plot(N, ecdf_N(N))
ggplot()+
  geom_step(aes(N, ecdf_N(N)))

# Probabilidad de que en un mes haya más de 10 siniestros:

mean(N>10)


# ¿Cuál es el cuantil 0.65 de la distribución?

# Casos en la ecdf donde se supera el cuantil:
ecdf_N(N)[ecdf_N(N)>=0.65]

# Meses en el vector donde se supera el cuantil:
N[ecdf_N(N)>=0.65]

# Necesitamos el valor mínimo 
#donde se alcanza este nivel de probabilidad:

min(N[ecdf_N(N)>=0.65])

# Comprobando:
mean(N<=3)
mean(N<=2)

quantile(N, 0.65, type = 1)
  
# Mediana:
quantile(N, 0.5, type = 1)
median(N)

# Rango intercuartil:
quantile(N, .75)-quantile(N, .25)
IQR(N)

ggplot()+
  geom_histogram(aes(N), bins = 10,
                 color = "black",
                 fill = "orange",
                 alpha = 0.5)+
  geom_vline(aes(xintercept = quantile(N, .25),
                 color = "Q1"),
             linetype = "dashed")+
  geom_vline(aes(xintercept = quantile(N, .5),
                 color = "Mediana"),
             linetype = "dashed")+
  geom_vline(aes(xintercept = quantile(N, .75),
                 color = "Q3"),
             linetype = "dashed")

# Densidad Kernel:
ggplot()+
  geom_density(aes(N),
                 color = "black",
                 fill = "white",
                 alpha = 0.5)+
  geom_vline(aes(xintercept = quantile(N, .25),
                 color = "Q1"),
             linetype = "dashed")+
  geom_vline(aes(xintercept = quantile(N, .5),
                 color = "Mediana"),
             linetype = "dashed")+
  geom_vline(aes(xintercept = quantile(N, .75),
                 color = "Q3"),
             linetype = "dashed")

# Ejemplo: Si quisiera tener el 95% de certeza de que
# estaré preparado, ¿para cuántos siniestros debería de
# estar listo en un mes?

quantile(N, .95)

# Cuartiles son cuantiles: el Q1 es el cuantil 0.25, el Q2 o mediana es el cuantil 0.5, etc.

# Si solo uno de cada tres meses fuera necesario estar preparado,
# ¿cuántos siniestros debería de esperar?

quantile(N, 1/3)

# Podemos calcular ciertos estadísticos de la variable:

# Media muestral
mean(N)
# Varianza muestral
var(N)
# Desviación estándar muestral
sd(N)




