
library(readxl)
BD <- read_excel("BD_2S ENLA muestral 2023.xlsx")
View(BD)

library(magrittr)
library(dplyr)

data_ayacucho <- BD %>% filter(departamento == "AYACUCHO") %>% select(c(
  "area",                     # Zona donde viven (rural o urbana)
  "M500_EM_2S_2023_CT",       # Puntaje de comprensión lectora
  "M500_EM_2S_2023_MA",       # Puntaje de matemáticas
  "M500_EM_2S_2023_CS"        # Puntaje de ciencias sociales
))

any(is.na(data_ayacucho))
data_ayacucho_limpio <- na.omit(data_ayacucho)


############################
############### Rural ######
############################


data_rural <- data_ayacucho_limpio %>% filter(area=="Rural") %>% select(-area)


############### Nota matemáticas ############################

nota_m <- data_rural$M500_EM_2S_2023_MA # se tiene que extraer como vector numerico
shapiro.test(nota_m)

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

nota_m_sin_extremos <- nota_m[-extremos_m]

shapiro.test(nota_m_sin_extremos)  # cumple normalidad 
qqnorm(nota_m_sin_extremos)
qqline(nota_m_sin_extremos, col = "red")
hist(nota_m, main = "Histograma de Nota Matemáticas", xlab = "Puntajes")





##################
#### Urbana ###### 
##################


data_urbano <- data_ayacucho_limpio %>% filter(area == "Urbana") %>% select(-area)

############### Nota matemáticas ############################

nota_m_urb <- data_urbano$M500_EM_2S_2023_MA # se tiene que extraer como vector numerico
shapiro.test(nota_m_urb)

any(is.na(nota_m_urb))
# Q-Q Plot
qqnorm(nota_m_urb)
qqline(nota_m_urb, col = "red")

# Histograma
hist(nota_m_urb, main = "Histograma de Nota Matemáticas", xlab = "Puntajes")

##  quitar valores extremos 
z_scores_m_urb <- scale(nota_m_urb)
extremos_m_urb <- which(abs(z_scores_m_urb)>3)
data_extremos_m_urb <- nota_m[extremos_m_urb]
print(data_extremos_m_urb)

nota_m_sin_extremos_urb <- nota_m_urb[-extremos_m_urb]

shapiro.test(nota_m_sin_extremos_urb)  # cumple normalidad 
qqnorm(nota_m_sin_extremos_urb)
qqline(nota_m_sin_extremos_urb, col = "red")
hist(nota_m_sin_extremos_urb, main = "Histograma de Nota Matemáticas", xlab = "Puntajes")




