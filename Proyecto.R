
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


p.hombres <-  autonoma[,c(25:47)]

p.mujeres <-  autonoma[,c(48:70)]