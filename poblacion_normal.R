
library(readxl)
BD <- read_excel("BD_2S ENLA muestral 2023.xlsx")
View(BD)

library(magrittr)
library(dplyr)

data_ayacucho_nuevo <- BD %>% 
  filter(departamento == "AYACUCHO", area == "Rural") %>% 
  select(c("sexo", "M500_EM_2S_2023_MA")) %>% 
  na.omit()

shapiro.test(data_ayacucho_nuevo$M500_EM_2S_2023_MA)

nota_m <- data_rural$M500_EM_2S_2023_MA # extraer las notas en forma de vector 

# Q-Q Plot
qqnorm(nota_m)
qqline(nota_m, col = "red")

# Histograma
hist(nota_m, main = "Histograma de Nota Matemáticas", xlab = "Puntajes")

##  quitar valores extremos 
z_scores_m <- scale(nota_m)
extremos_m <- which(abs(z_scores_m)>3)
data_extremos_m <- nota_m[extremos_m]
print(data_extremos_m)

data_rural_sin <- data_rural[-extremos_m, ]
shapiro.test(data_rural_sin$M500_EM_2S_2023_MA)  # cumple normalidad 
qqnorm(data_rural_sin$M500_EM_2S_2023_MA)
qqline(data_rural_sin$M500_EM_2S_2023_MA, col = "red")
hist(data_rural_sin$M500_EM_2S_2023_MA, main = "Histograma de Nota Matemáticas", xlab = "Puntajes")


### Población #####

data_rural_sin
summary(data_rural_sin$M500_EM_2S_2023_MA)

#### Comprobando normalidad de los grupos 

data_hombre <- data_rural_sin %>% filter(sexo == "Hombre") %>% select(2)
shapiro.test(data_hombre$M500_EM_2S_2023_MA)   # cumple normalidad 

N_h <- nrow(data_hombre)

data_mujer <- data_rural_sin %>% filter(sexo == "Mujer") %>%  select(2)
shapiro.test(data_mujer$M500_EM_2S_2023_MA)    # cumple normalidad 

N_m <- nrow(data_mujer)

# Estimar el tamaño de muestra para una población infinita

n <- 160 # tamaño total de la muestra

n_h <- round(n*N_h/N, 0) # tamaño de muestra de estrato masculino
n_m <- round(n*N_m/N, 0) # tamaño de muestra de estrao femenino

########### Muestra ##############

library(TeachingSampling)
set.seed(123)
sam <- S.STSI(data_rural_sin$sexo, c(N_h, N_m), c(n_h, n_m))
muestra <- data_rural_sin[sam,]
View(muestra)

muestra_hombre <- muestra %>% filter(sexo == "Hombre") %>% select(2)
shapiro.test(muestra_hombre$M500_EM_2S_2023_MA)

muestra_mujer <- muestra %>% filter(sexo == "Mujer") %>% select(2)
shapiro.test(muestra_mujer$M500_EM_2S_2023_MA)









