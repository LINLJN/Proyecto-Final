
#Lectura de datos PIB 'variacion anual del PIB de España y previsiones de crecimiento hechas por el FMI'
PIB <- read.csv('dat/PIB/variacion_anual_del_pib_d.csv',sep = ";",header = T,encoding = 'UTF-8')
PIB$Variación.anual.del.PIB <- as.numeric(gsub(',','.',PIB$Variación.anual.del.PIB))

#Las previsiones sobre el PIB español de la OCDE

pib <-  read.csv('dat/PIB/las_previsiones_sobre_el_PIB.csv',sep = ";",header = T,encoding = 'UTF-8')
pib$Variación.interanual.del.PIB <- as.numeric(gsub(',','.',pib$Variación.interanual.del.PIB))

#Lectura de datos de poblacion de Espana Generico

poblacion <-  read.csv('dat/Poblacion/evolucion_de_la_poblacion.csv',sep = ";",header = T,encoding = 'UTF-8')
poblacion$Evolución.de.la.población.residente.en.España <-as.numeric(gsub(',','.',poblacion$Evolución.de.la.población.residente.en.España))

#--------------------------------------
p.poblacion <- read.csv('dat/Poblacion/prevision_poblacion.csv',sep = ";",header = T,encoding = 'UTF-8')

sub <- function(x){
  x<-as.numeric(gsub(",",".",x))
}

p.poblacion[,3:5] <- lapply(p.poblacion[,3:5],sub)

#--------------------------------------
emigracion <-  read.csv('dat/Poblacion/evolucion_de_la_emigracio.csv',sep = ";",header = T,encoding = 'UTF-8')

  mil <- function(x){
   x<-as.numeric(gsub("\\.","",x))
  }
emigracion[,3:4] <- lapply(emigracion[,3:4],mil)


#Lectura de poblacion por comunidad autonoma 
autonoma <-  read.csv('dat/Poblacion/2853sc.csv',sep = ";",skip = 5)

  mil <- function(x){
    x<-as.numeric(gsub("\\.","",x))
  }
  
autonoma[,2:70] <- lapply(autonoma[,2:70],mil)

p.total <- autonoma[,c(1:24)]
p.total <- p.total[-c(1,20:24),]


library(reshape2)
p.total <- melt(p.total,"X")
p.total$variable <- gsub('X','',p.total$variable)

library(ggplot2)
library(gganimate)
theme_set(theme_bw())

p <- ggplot(
  p.total, 
  aes(x = variable, y=value, size = pop, colour = X)
) +
  geom_point(show.legend = FALSE, alpha = 0.7) +
  scale_color_viridis_d() +
  scale_size(range = c(1, 5)) +
  labs(x = "Year", y = "n.habitantes")

p


p.hombres <-  autonoma[,c(25:47)]

p.mujeres <-  autonoma[,c(48:70)]