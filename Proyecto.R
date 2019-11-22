
#Lectura de datos PIB 'variacion anual del PIB de España y previsiones de crecimiento hechas por el FMI'
PIB <- read.csv('dat/PIB/variacion_anual_del_pib_d.csv',sep = ";",header = T,encoding = 'UTF-8')
FMI <- PIB[-c(37:39),] 

#Las previsiones sobre el PIB español de la OCDE
pib <-  read.csv('dat/PIB/las_previsiones_sobre_el_PIB.csv',sep = ";",header = T,encoding = 'UTF-8')
OCDE <- pib[-c(37:40),] 

#Lectura de datos de poblacion de Espana 
poblacion <-  read.csv('dat/Poblacion/evolucion_de_la_poblacion.csv',sep = ";",header = T,encoding = 'UTF-8')
p.poblacion <- read.csv('dat/Poblacion/prevision_poblacion.csv',sep = ";",header = T,encoding = 'UTF-8')
emigracion <-  read.csv('dat/Poblacion/evolucion_de_la_emigracio.csv',sep = ";",header = T,encoding = 'UTF-8')
git pull

