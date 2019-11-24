
#Lectura de datos PIB 'variacion anual del PIB de Espa?a y previsiones de crecimiento hechas por el FMI'
PIB <- read.csv('dat/PIB/variacion_anual_del_pib_d.csv',sep = ";",header = T,encoding = 'UTF-8')
PIB$Variaci?n.anual.del.PIB <- as.numeric(gsub(',','.',PIB$Variaci?n.anual.del.PIB))



#Las previsiones sobre el PIB espa?ol de la OCDE

pib <-  read.csv('dat/PIB/las_previsiones_sobre_el_PIB.csv',sep = ";",header = T,encoding = 'UTF-8')
pib$Variaci?n.interanual.del.PIB <- as.numeric(gsub(',','.',pib$Variaci?n.interanual.del.PIB))



#Lectura de datos de poblacion de Espana Generico

poblacion <-  read.csv('dat/Poblacion/evolucion_de_la_poblacion.csv',sep = ";",header = T,encoding = 'UTF-8')
poblacion$Evoluci?n.de.la.poblaci?n.residente.en.Espa?a <-as.numeric(gsub(',','.',poblacion$Evoluci?n.de.la.poblaci?n.residente.en.Espa?a))



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
names (p.total)
names(p.total) = c('Provincia','year','poblacion')

library(tidyverse)
library(lubridate)
p.total %>%
  select(Provincia,poblacion,year) %>%
  group_by(year) %>%
  arrange(year,-poblacion) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 19) ->
ranked_by_year1  

ranked_by_year1$year<- as.integer(ranked_by_year1$year)
ranked_by_year1$poblacion<- as.integer(ranked_by_year1$poblacion)

ggplot(data = ranked_by_year1) +
  aes(group = poblacion, fill = Provincia) +
  aes(xmin = 0 ,
      xmax = poblacion / 1000000) +
  aes(ymin = rank - .45,
      ymax = rank + .45) +
  scale_y_reverse() +
  scale_x_continuous(
    limits = c(-350, 1400),
    breaks = c(0, 400, 800, 1200),
    labels = c(0, 400, 800, 1200)) +
  labs(fill = "") +
  geom_rect(alpha = .7) +
  labs(x = 'Population (millions)') +
  aes(label = Provincia, y = rank) +
  geom_text(col = "gray13",
            hjust = "right",
            x = -50) +
  labs(y = "") +
  scale_fill_viridis_d(option = "magma",
                       direction = -1) +
  geom_text(x = 1000 , y = -10,
            family = "Times",
            aes(label = as.character(year)),
            size = 30, col = "grey18") +
  my_theme ->
  my_plot
  
library(gganimate)
options(gganimte.nframes = 20)
my_plot + gganimate::transition_time(year)




p.hombres <-  autonoma[,c(25:47)]

p.mujeres <-  autonoma[,c(48:70)]