
#Lectura de datos PIB 'variacion anual del PIB de Espa?a y previsiones de crecimiento hechas por el FMI'
PIB <- read.csv('dat/PIB/variacion_anual_del_pib_d.csv',sep = ";",header = T,encoding = 'UTF-8')
PIB$Variación.anual.del.PIB <- as.numeric(gsub(',','.',PIB$Variación.anual.del.PIB))



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
library(reshape2)
library(tidyverse)
library(lubridate)
library(gganimate)

autonoma <-  read.csv('dat/Poblacion/2853sc.csv',sep = ";",skip = 5)

mil <- function(x){
  x<-as.numeric(gsub("\\.","",x))
}

autonoma[,2:70] <- lapply(autonoma[,2:70],mil)

#Filtramos datos
p.total <- autonoma[,c(1:24)]
p.total <- p.total[-c(1,20:24),]
#Pasamos a modo largo la tabla
p.total <- melt(p.total,"X")
p.total$variable <- gsub('X','',p.total$variable)
names (p.total)
names(p.total) = c('Provincia','year','poblacion')
p.total <- na.omit(p.total)
#asignamos rangos para la grafica
p.total %>%
  select(Provincia,poblacion,year) %>%
  group_by(year) %>%
  arrange(year,-poblacion) %>%
  mutate(rank = 1:n()) %>%
  filter(rank <= 19) ->
  ranked_by_year1  
#Convertimos datos a integer
ranked_by_year1$year<- as.integer(ranked_by_year1$year)
ranked_by_year1$poblacion<- as.integer(ranked_by_year1$poblacion)
#creamos la grafica animada
my_theme <- ggplot(data = ranked_by_year1) +
  aes(group = Provincia, fill=Provincia) + 
  aes(xmin = 0 ,
      xmax = poblacion/2000) +
  aes(ymin = rank - 0.45,
      ymax = rank + 0.45) +
  scale_y_reverse(
    breaks = c(0),
    labels = c(0)) +
  scale_x_continuous(
    limits = c(-1500, 4500),
    breaks = c(0),
    labels = c(0)) +
  labs(fill = "") +
  geom_rect(alpha = 0.7) +
  labs(title = "Tasa de crecimiento demográfico (España/Comunidades)",x = 'Población') +
  aes(label = Provincia, y = rank) +
  geom_text(col = "gray13",
            hjust = "right",
            x = -30) +
  geom_text(aes(y = rank , label = as.character(poblacion)),
            vjust = 0.4, 
            hjust = 'right' ,
            x = 4800) +
  labs(y = "Provincia") +
  theme(axis.text=element_text(size=20,face="bold"),
        axis.title=element_text(size=14,face="bold")) +
  scale_fill_viridis_d(option = "viridis",
                       direction = -1) +
  geom_text(x = 3000 , y = -15,
            family = "Times",
            aes(label = as.character(year)),
            size = 15, col = "grey18") 
my_plot <- my_theme
#ponemos condiciones a la grafica y la pintamos 
options(gganimate.dev_args = list(width = 800, height = 600))
my_plot + gganimate::transition_states(year)

#------------------------------------------------
p.hombres <-  autonoma[,c(25:47)]

p.mujeres <-  autonoma[,c(48:70)]