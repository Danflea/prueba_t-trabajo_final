
library(readxl)
bd<-read_excel("BD_2S ENLA muestral 2023.xlsx")
bd <- read_excel("C:/Users/frusc/Desktop/Análisis Estadistico - Danilo/Trabajos_encargados/RMD-Trabajo_anaalisis-estadistico/BD_2S ENLA muestral 2023.xlsx")
View(BD_2S_ENLA_muestral_2023)

library(magrittr)
library(dplyr)

##### Poblacion Ayacucho

pob_ayacucho <- bd %>% filter(departamento == "AYACUCHO") %>% select(c(5, 7, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26))

any(is.na(pob_ayacucho))

pob_ayacucho_limpio <- na.omit(pob_ayacucho)

any(is.na(pob_ayacucho_limpio))


# Población Urbana
pob_urbana <- pob_ayacucho_limpio %>% filter(area == "Urbana")
any(is.na(pob_urbana))

# Población rural
pob_rural <- pob_ayacucho_limpio %>% filter(area == "Rural")
any(is.na(pob_rural))


###### 


