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



install.packages("hrbrthemes")
library(hrbrthemes)


p <- ranked_by_year1 %>%
  ggplot(aes(x = -rank,y = poblacion, group = poblacion)) +
  geom_tile(aes(y = poblacion / 2, height = poblacion, fill = Provincia), width = 0.9) +
  geom_text(aes(label = Provincia), hjust = "right", colour = "black", fontface = "bold", nudge_y = -100000) +
  geom_text(aes(label = scales::comma(poblacion)), hjust = "left", nudge_y = 100000, colour = "grey30") +
  coord_flip(clip="off") +
  #scale_fill_manual(name = 'Provincia', values = c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3")) +
  scale_fill_viridis_d(option = "magma",
                       direction = -1)+
  scale_x_discrete("") +
  scale_y_continuous("",labels=scales::comma) +
  hrbrthemes::theme_ipsum(plot_title_size = 32, subtitle_size = 24, caption_size = 20, base_size = 20) +
  theme(panel.grid.major.y=element_blank(),
        panel.grid.minor.x=element_blank(),
        legend.position = c(0.4, 0.2),
        plot.margin = margin(1,1,1,2,"cm"),
        axis.text.y=element_blank()) +
  # gganimate code to transition by year:
  transition_time(year) +
  ease_aes('cubic-in-out') +
  labs(title='Largest Cities in the United States',
       subtitle='Population in {round(frame_time,0)}',
       caption='Source: United States Census
michaeltoth.me / @michael_toth')

animate(p, nframes = 750, fps = 25, end_pause = 30, width = 800, height = 800)











my_theme <- ggplot(data = ranked_by_year1) +
  aes(x = -rank,group = poblacion, fill = Provincia) +
  aes(xmin = 0 ,
      xmax = poblacion / 1000000) +
  aes(ymin = rank - .45,
      ymax = rank + .45) +
  scale_y_discrete() +
  scale_x_continuous("",labels=scales::comma) +
  labs(fill = "") +
  geom_tile(aes(y = poblacion / 2, height = poblacion, fill = Provincia), width = 0.9)+
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
            size = 30, col = "grey18") 

   my_plot <- my_theme

library(gganimate)
options(gganimte.nframes = 20)
my_plot + gganimate::transition_time(year)
