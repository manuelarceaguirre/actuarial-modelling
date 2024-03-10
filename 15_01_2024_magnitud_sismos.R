sismos_oaxaca <- read.csv("D:/UAG/Modelación actuarial/2024/Material/sismos_oaxaca.csv",
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

# ¿Cuál es la probabilidad de que en 2 años no haya sismos
# de magnitud superior a 6?






