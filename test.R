autonoma <-  read.csv('dat/Poblacion/2853sc.csv',sep = ";",skip = 5)

mil <- function(x){
  x<-as.numeric(gsub("\\.","",x))
}

autonoma[,2:70] <- lapply(autonoma[,2:70],mil)

p.total <- autonoma[,c(1:24)]
p.total <- p.total[-c(1,20:24),]

library(ggplot2)
library(reshape2)
p.total <- melt(p.total,"X")
p.total$variable <- gsub('X','',p.total$variable)
names (p.total)
names(p.total) = c('Provincia','year','poblacion')

library(gganimate)
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



#install.packages("hrbrthemes")
#library(hrbrthemes)


p <- ranked_by_year1 %>%
  ggplot(aes(x = -rank,y = poblacion, group = poblacion)) +
  geom_tile(aes(y = poblacion / 2, height = poblacion, fill = Provincia), width = 0.9) +
  geom_text(aes(label = Provincia), hjust = "right", colour = "black", fontface = "bold", nudge_y = -100000) +
  geom_text(aes(label = scales::comma(poblacion)), hjust = "left", nudge_y = 100000, colour = "grey30") +
  coord_flip(clip="off") +
  scale_fill_viridis_d(name = 'Provincia',option = "magma",direction = -1) +
  #scale_fill_viridis_d(option = "magma",
                       #direction = -1)+
  scale_x_discrete("") +
  scale_y_continuous("",labels=scales::comma) +
  hrbrthemes::theme_ipsum(plot_title_size = 32, subtitle_size = 24, caption_size = 20, base_size = 20) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = c(1, 0.3),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.y=element_blank()) +
  # gganimate code to transition by year:
  transition_time(year) +
  ease_aes('cubic-in-out') +
  labs(title='Largest Cities in the United States',
       subtitle='Population in {round(frame_time,0)}',
       caption='Source: United States Census
michaeltoth.me / @michael_toth')

animate(p, nframes = 40 , fps = 60, end_pause = 30, width = 800, height = 800)





#install.packages("transformr")
#library(transformr)
#library(gganimate)




