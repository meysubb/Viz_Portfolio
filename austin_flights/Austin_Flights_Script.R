options(stringsAsFactors = FALSE)
austin_flights_df <- read.csv("data/ABIA.csv")

library(tidyverse)
library(maps)
library(ggmap)
library(ggrepel)
library(gridExtra)
library(grid)


### Time Series Calendar HeatMap
avg_delay <- austin_flights_df %>% group_by(Month,DayofMonth) %>% 
  summarise(dep_delay = mean(DepDelay[DepDelay>0],na.rm=T),
            arr_delay = mean(ArrDelay[ArrDelay>0],na.rm=T)) %>% ungroup() %>% 
  mutate(Year = 2008,
         monthweek = ceiling(DayofMonth/7),
         date = as.Date(paste(Year, Month, DayofMonth,sep="-"), "%Y-%m-%d"),
         day = weekdays(date),
         mnth = month.abb[Month],
         mnth = factor(mnth,levels=c("Jan","Feb","Mar",
                                               "Apr","May","Jun",
                                               "Jul","Aug","Sep",
                                               "Oct","Nov","Dec")),
         day = factor(day, levels= rev(c("Sunday", "Monday", 
                                                   "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))))

p1 <- ggplot(avg_delay, aes(monthweek, day, fill = dep_delay)) + 
  geom_tile(colour = "white") + 
  facet_wrap(~mnth,ncol=3) + 
  scale_fill_gradient(low='yellow', high="red",name='Delay (minutes)',
                      limits = c(5,70)) +
  labs(title = 'Average Flight Departures Delays',
       x = 'Week of Month', 
       y = '') + theme_bw() + theme(aspect.ratio = 1) 

p4 <- ggplot(avg_delay, aes(monthweek, day, fill = arr_delay)) + 
  geom_tile(colour = "white") + 
  facet_wrap(~mnth,ncol=3) + 
  scale_fill_gradient(low='yellow', high="red",name='Delay (minutes)',
                      limits=c(5,70)) +
  labs(title = 'Average Flight Arrival Delays',
       x = 'Week of Month', 
       y = '') + theme_bw() + theme(aspect.ratio = 1) 


# a set of personal choices for the map display
states <- map_data("state")
map.opts <- theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(), panel.background=element_blank(), 
                  axis.title.x=element_blank(), axis.title.y=element_blank(), 
                  axis.line=element_blank(), axis.ticks=element_blank(), 
                  axis.text.y = element_text(colour="#FFFFFF"), 
                  axis.text.x = element_text(colour = "#FFFFFF"))

### Departure Delays out of Austin
aus_dep <- austin_flights_df %>% filter(Origin == 'AUS') %>% 
  group_by(Dest,Origin) %>% summarize(delay_times = 
                                 mean(DepDelay[DepDelay>0],na.rm=T)) %>% ungroup() %>% 
  mutate(Dest = ifelse(Dest == "TUL","Tulsa",Dest),
    origin_loc = purrr::map(Origin, geocode, output = "latlon", source = "google", 
                                 messaging = FALSE),
         dest_loc = purrr::map(Dest, geocode, output = "latlon", source = "google", 
                               messaging = FALSE)) %>% unnest() %>% 
  rename("origin_lon"="lon","origin_lat"="lat","dest_lon"="lon1","dest_lat"="lat1")

## 75% quantile is 37.5

aus_dep_text <- aus_dep %>% filter(delay_times > 37.5)

p2 <- ggplot(states) + 
  geom_polygon(aes(x = long, y = lat,group=group),color='black',fill='grey') + map.opts  + 
  coord_fixed(1.3) + 
  geom_point(data=aus_dep,aes(x=dest_lon,y=dest_lat,size=delay_times,color=delay_times)) +
  geom_text_repel(data=aus_dep_text,aes(x=dest_lon,y=dest_lat,label=Dest),
                  fontface = 'bold', color = 'blue',
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(0.5, "lines"),
                  segment.color = 'grey50') +  
  scale_color_gradient(low='yellow', high="red",name='Delay (minutes)') +
  guides(color=FALSE,size=FALSE) + 
  labs(title = "Average Departure Delay Times")


### Departure Delays heading to Austin. 
aus_arrival <- austin_flights_df %>% filter(Dest == 'AUS') %>% 
  group_by(Origin,Dest) %>% summarize(delay_times = 
                                        mean(ArrDelay[ArrDelay>0],na.rm=T)) %>% ungroup() %>% 
  mutate(Origin = ifelse(Origin == "TUL","Tulsa",Origin),
         origin_loc = purrr::map(Origin, geocode, output = "latlon", source = "google", 
                                 messaging = FALSE),
         dest_loc = purrr::map(Dest, geocode, output = "latlon", source = "google", 
                               messaging = FALSE)) %>% unnest() %>% 
  rename("origin_lon"="lon","origin_lat"="lat","dest_lon"="lon1","dest_lat"="lat1")

### Quantile 36.02
aus_arr_text <- aus_arrival %>% filter(delay_times > 36.02)

p3 <- ggplot(states) + 
  geom_polygon(aes(x = long, y = lat,group=group),color='black',fill='grey') + map.opts  + 
  coord_fixed(1.3) + 
  geom_point(data=aus_arrival,aes(x=origin_lon,y=origin_lat,size=delay_times,color=delay_times)) +
  geom_text_repel(data=aus_arr_text,aes(x=origin_lon,y=origin_lat,label=Origin),
                  fontface = 'bold', color = 'blue',
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(0.5, "lines"),
                  segment.color = 'grey50') +  
  scale_color_gradient(low='yellow', high="red",name='Delay (minutes)') +
  guides(color=FALSE,size=FALSE) + 
  labs(title = "Average Arrival Delay Times")


grid.arrange(arrangeGrob(p2,p3,ncol=2),top = textGrob("Departure/Arrival Delays out of Austin-Bergstorm", gp = gpar(fontsize=18, fontface="bold.italic", fontsize=18)))

grid.arrange(p1,p4,ncol=2)


#grid.arrange(arrangeGrob(p2,p3, ncol=2),arrangeGrob(p1),arrangeGrob(p4),
#             top = textGrob("Departure/Arrival Delays out of Austin-Bergstorm", gp = gpar(fontsize=18, fontface="bold.italic", fontsize=18)),
#             nrow=3,heights=c(3,2,2))
