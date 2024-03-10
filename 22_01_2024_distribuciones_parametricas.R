# Funciones: ----
## Función de densidad o probabilidad: d
dpois(6, lambda = 3.5) # Probabilidad de que una Poisson con lambda igual a 3.5 tome el valor 6
(3.5^6)*exp(-3.5)/factorial(6)

dnorm(x = 120, mean = 100, sd = 5)
(1/(sqrt(2*pi*25)))*exp((-(120-100)^2)/(2*25))

## Función de distribución o CDF: p
plnorm(1e3, meanlog = 5, sdlog = 2) # Probabilidad de que una Lognormal tome un valor menor o igual que mil, con esos parámetros
plnorm(1e3, meanlog = 5, sdlog = 2)-plnorm(500, meanlog = 5, sdlog = 2)
plnorm(1e3, meanlog = 5, sdlog = 2, lower.tail = FALSE) #Probabilidad de que una lognormal tome un valor mayor que mil
1-plnorm(1e3, meanlog = 5, sdlog = 2)

## Cuantiles: q
qbinom(.9, size = 50, prob = 0.7) # Cuantil 0.9 de una binomial con esos parámetros

qgamma(.5, shape = 2, rate = 1.5) # Mediana de esta gamma

## Generador de valores pseudo-aleatorios: r

rexp(n = 1e2, rate = .5)

hist(rexp(n = 1e4, rate = .5))

rnorm(10)
hist(rnorm(1e5))

runif(20)
hist(runif(1e5))

e1071::kurtosis(rnorm(1e6))
e1071::kurtosis(rexp(1e6))

# Binomial: ----
# N ~ Binomial(n, p), si N es el número de éxitos en n intentos,
# con probabilidad de éxito p

## Soporte: 0, 1, 2, ..., n
## Parámetros: n, p
# n (intentos) y p (prob. de éxito)
# Modelar el número de éxitos en n intentos
# Por ejemplo: Número de pólizas siniestradas en un año de un total de N pólizas
# Número de personas de 65 años de un grupo de 100 que fallecen de un año a otro
## Momentos
## Media = n*p
# Si q = 1-p
## Varianza = n*p*q
## Sesgo = (q-p)/sqrt(n*p*q)
## Exceso de curtosis = (1-6*p*q)/(n*p*q)
## Gráfica
p <- 0.5
q <- 1-p
n <- 50
N <- rbinom(1e6, size = n, prob = p)

ggplot()+
  geom_density(aes(N))

ggplot()+
  geom_function(fun = dbinom, 
                args = list(size = n, prob = p))+
  xlim(0, 50)

# Si tuviera distintos parámetros:

ggplot()+
  geom_function(fun = dbinom, 
                args = list(size = n, prob = 0.5),
                aes(col = "p = 0.5"))+
  geom_function(fun = dbinom, 
                args = list(size = n, prob = 0.7),
                aes(col = "p = 0.7"))+
  geom_function(fun = dbinom, 
                args = list(size = n, prob = 0.3),
                aes(col = "p = 0.3"))+
  geom_function(fun = dbinom, 
                args = list(size = n, prob = 0.999),
                aes(col = "p = 0.999"))+
  xlim(0, 50)+
  labs(color = "Parámetro")

# Sesgo y curtosis de estas distribuciones:
## p = 0.5
library(e1071)
p <- 0.5
(q-p)/sqrt(n*p*q) # Sesgo = 0 -> Distribución simétrica

(1-6*p*q)/(n*p*q) # Curtosis < 0 -> colas más ligeras que la dist. normal

p <- 0.1
(q-p)/sqrt(n*p*q) # Sesgo > 0 -> Distribución cargada a la derecha

(1-6*p*q)/(n*p*q) # Curtosis > 0 -> colas más pesadas que la dist. normal


p <- 0.9
(q-p)/sqrt(n*p*q) # Sesgo > 0 -> Distribución cargada a la derecha

(1-6*p*q)/(n*p*q) # Curtosis > 0 -> colas más pesadas que la dist. normal


p <- 0.9999
(q-p)/sqrt(n*p*q) # Sesgo > 0 -> Distribución cargada a la derecha

(1-6*p*q)/(n*p*q) # Curtosis > 0 -> colas más pesadas que la dist. normal

## Cálculos
# Poisson: ----
# Se usa para modelar variables aleatorias
# que toman valores enteros y 0 y cuando no 
# se conoce el límite entero que podría tomar

# N ~ pois(lambda), lambda > 0

ggplot()+
  geom_function(aes(col = "lambda = 0.5"),
                fun = dpois, 
                args = list(lambda = 0.5))+
  geom_function(aes(col = "lambda = 2"),
                fun = dpois, 
                args = list(lambda = 2))+
  xlim(0, 8)


ggplot()+
  geom_function(aes(col = "lambda = 1000"),
                fun = dpois, 
                args = list(lambda = 1000))+
  xlim(500, 1500)

# Sesgo y curtosis:
lambda <- 0.5
1/sqrt(lambda) # Sesgo
1/lambda # Curtosis en exceso

# Con lambda = 1000
1/sqrt(1000) # Sesgo
1/1000 # curtosis

# Cuál es la probabilidad de que N <= 10 para una poisson con lambda = 12
ppois(10, lambda = 12)

# Cuál es la probabilidad de que N esté entre 1200 y 1250 para una poisson con lambda = 1300
ppois(1250, lambda = 1300)-ppois(1200, lambda = 1300)


# Normal: ----
# X ~ N(mu, sigma)
# Exponencial: ----
# X ~ exp(1/lambda)
# Lognormal: ----
# X ~ lognorm(mu, sigma), si exp(X) ~ N(mu, sigma)
# Binomial negativa: ----
# N ~ NB(r, p)
# Pareto: ----
# X ~ Pareto(alpha, theta o x_m)
dpareto()
library(actuar)

