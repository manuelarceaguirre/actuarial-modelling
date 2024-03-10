library(readxl)
library(dplyr)
library(zoo)
# install.packages(c("zoo", "readxl", "dplyr"))
base_incendios <- read_excel("D:/UAG/Modelación actuarial/2024/Material/bases_ejemplos/siniestralidad_incendios.xlsx")

# ¿Qué cuantil es la media?
mean(base_incendios$monto<=mean(base_incendios$monto, na.rm = T), 
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








  
