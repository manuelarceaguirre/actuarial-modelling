sismos_oaxaca <- read.csv("D:/UAG/Modelación actuarial/2024/Material/bases_ejemplos/sismos_oaxaca.csv",
                          skip = 4,
                          nrows = 108000)

magnitudes <- as.numeric(sismos_oaxaca$Magnitud)
magnitudes <- magnitudes[!is.na(magnitudes)]

# Casos en los que la magnitud del sismo fue menor o igual que 4:
sum(magnitudes<=4) 
# Tamaño de la muestra:
length(magnitudes)

# Probabilidad de que la magnitud de un sismo haya sido menor o igual que 4
sum(magnitudes<=4)/length(magnitudes)

# Otra forma:
mean(magnitudes<=4) 

# ECDF:

ECDF <- rep(NA, length(magnitudes))

for(i in 1:length(ECDF)){
  
  ECDF[i] <- mean(magnitudes<=magnitudes[i])
  
}

View(data.frame(Magnitud = magnitudes,
                eCDF = ECDF))

library(ggplot2)

ggplot()+
  geom_step(aes(magnitudes, ECDF))+
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "red")
  
  
# Otra forma de obtener la función de distribución empírica:

ECDF2 <- ecdf(magnitudes) 
ECDF2(3.5)
ECDF2(4)
ECDF_FACIL <- ECDF2(magnitudes)

all.equal(ECDF, ECDF_FACIL)

# ¿Cuál es la probabilidad de observar un sismo de 
# magnitud mayor que 5?
1-ECDF2(5)
mean(magnitudes>5)
ggplot()+
  geom_step(aes(magnitudes, ECDF))+
  geom_vline(xintercept = 3.5, linetype = "dashed", color = "red")+
  geom_hline(yintercept = mean(magnitudes<=3.5), linetype = "dashed", color = "red")+
  scale_x_continuous(n.breaks = 20)+
  scale_y_continuous(n.breaks = 20)



# ¿Cuál es la probabilidad de que un 
# sismo en Oaxaca sea de magnitud entre 3.5 y 4.5?
mean(magnitudes<=4.5)-mean(magnitudes<=3.5)
ECDF2(4.5)-ECDF2(3.5)
mean(magnitudes<=4.5 & magnitudes > 3.5)

# ¿Cuál es la probabilidad de que en un intervalo de 5 años 
# haya por lo menos un sismo de magnitud superior a 7?
sismos_oaxaca$Fecha <- as.Date(sismos_oaxaca$Fecha, format = "%d/%m/%Y")

G1_1 <- subset(sismos_oaxaca, Fecha >= as.Date("01/01/1990", format="%d/%m/%Y") & Fecha <= as.Date("31/12/1994", format="%d/%m/%Y"))
Intervalo1 <- as.numeric(G1_1$Magnitud)
Intervalo1 <- Intervalo1[!is.na(Intervalo1)]

G2_1 <- subset(sismos_oaxaca, Fecha >= as.Date("01/01/1995", format="%d/%m/%Y") & Fecha <= as.Date("31/12/1999", format="%d/%m/%Y"))
Intervalo2 <- as.numeric(G2_1$Magnitud)
Intervalo2 <- Intervalo2[!is.na(Intervalo2)]

G3_1 <- subset(sismos_oaxaca, Fecha >= as.Date("01/01/2000", format="%d/%m/%Y") & Fecha <= as.Date("31/12/2004", format="%d/%m/%Y"))
Intervalo3 <- as.numeric(G3_1$Magnitud)
Intervalo3 <- Intervalo3[!is.na(Intervalo3)]

G4_1 <- subset(sismos_oaxaca, Fecha >= as.Date("01/01/2005", format="%d/%m/%Y") & Fecha <= as.Date("31/12/2009", format="%d/%m/%Y"))
Intervalo4 <- as.numeric(G4_1$Magnitud)
Intervalo4 <- Intervalo4[!is.na(Intervalo4)]

G5_1 <- subset(sismos_oaxaca, Fecha >= as.Date("01/01/2010", format="%d/%m/%Y") & Fecha <= as.Date("31/12/2014", format="%d/%m/%Y"))
Intervalo5 <- as.numeric(G5_1$Magnitud)
Intervalo5 <- Intervalo5[!is.na(Intervalo5)]

G6_1 <- subset(sismos_oaxaca, Fecha >= as.Date("01/01/2015", format="%d/%m/%Y") & Fecha <= as.Date("31/12/2019", format="%d/%m/%Y"))
Intervalo6 <- as.numeric(G6_1$Magnitud)
Intervalo6 <- Intervalo6[!is.na(Intervalo6)]

G7_1 <- subset(sismos_oaxaca, Fecha >= as.Date("01/01/2020", format="%d/%m/%Y") & Fecha <= as.Date("31/12/2024", format="%d/%m/%Y"))
Intervalo7 <- as.numeric(G7_1$Magnitud)
Intervalo7 <- Intervalo7[!is.na(Intervalo7)]

n <- rep(NA, length(7))

n[1]=sum(Intervalo1>=7)
n[2]=sum(Intervalo2>=7)
n[3]=sum(Intervalo3>=7)
n[4]=sum(Intervalo4>=7)
n[5]=sum(Intervalo5>=7)
n[6]=sum(Intervalo6>=7)
n[7]=sum(Intervalo7>=7)

n <- ifelse(n > 1, 1, n)

mean(n)

# La probabilidad de que en un intervalo de cinco años (sin repetir) haya por lo menos un sismo de magnitud igual o superior a 7 es de 0.43

library(dplyr)
library(lubridate)
library(zoo)

sismos_oaxaca$Año <- year(dmy(sismos_oaxaca$Fecha))


 
sismos_oaxaca <- mutate(sismos_oaxaca, Magnitud = as.numeric(Magnitud))

# ctrl + shift + M para generar %>% (pipe)

# Calculamos el máximo para cada año:
maximos_año <- sismos_oaxaca %>%
  mutate(Magnitud = as.numeric(Magnitud)) %>% # Convertimos en numérico la magnitud
  group_by(Año) %>%  # Agrupamos por año
  summarise(Maximo = max(Magnitud, na.rm = TRUE)) # Calculamos el máximo para cada grupo
  
# Iteramos para cada año y calculamos la magnitud máxima de su grupo de cinco años
maximos_5 <- c()
for(i in 1:(nrow(maximos_año)-4)){
  
  maximos_5 <- c(maximos_5, 
                 max(maximos_año[i:(i+4),"Maximo"]))
  
}

length(maximos_5)
nrow(maximos_año)

# Calculamos la probabilidad requerida:
mean(maximos_5>7)

# ¿Cuál es la probabilidad de que en 2 años no haya sismos
# de magnitud superior a 6?

#Es equivalente a calcular P(2 años magnitud máxima <= 6)

maximos_2 <- c()
for(i in 1:(nrow(maximos_año)-1)){
  
  maximos_2 <- c(maximos_2, 
                 max(maximos_año[i:(i+1),"Maximo"]))
  
}

length(maximos_2)
nrow(maximos_año)

# Calculamos la probabilidad requerida:
mean(maximos_2<=6)

