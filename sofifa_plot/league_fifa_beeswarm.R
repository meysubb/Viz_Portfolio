## Data: https://github.com/CJ-Mayes/SportsVizSunday/blob/main/Data/z%20Partnerships/GamesNightViz%20January%202023/fifa-23-complete-player-dataset.zip
## Viz Inspired by: https://twitter.com/robradburn/status/1616960545676034048?s=20
library(tidyverse)
library(ggbeeswarm)
library(ggrepel)
library(ggtext)

teams_df <- read_csv("sofifa_plot/fifa-23-complete-player-dataset/teams_fifa23.csv")
players_df <- read_csv("sofifa_plot/fifa-23-complete-player-dataset/players_fifa23.csv")

league_interest <- "French Ligue 1"

prepare_df <- function(teams_dat,players_dat,league,country){
  league_teams <- teams_dat %>% filter(grepl(league,League))
  #teams_in_league <- league_teams %>% pull(Name)
  players_in_league <- players_dat %>% inner_join(league_teams,by=c("Club"="Name"),suffix=c("_player","_team")) %>% 
    mutate(
      Club = fct_reorder(Club,Overall_team),
      in_country = Nationality == country 
    )
}

top_players <- players_in_league %>% group_by(Club) %>% 
  slice_max(Overall_player)

club_makeup <- players_in_league %>% group_by(Club,in_country) %>% 
  summarize(
    cnt = n()
  ) %>% ungroup() %>% group_by(Club) %>% 
  mutate(
    total = sum(cnt)
  ) %>% filter(in_country) %>% 
  mutate(
    pct = cnt/total
  ) %>% select(-in_country)

labels <- c()
club_list <- club_makeup %>% pull(Club) 
for (i in 1:length(club_list)){
  
  club_name <- club_list[i]
  club_df <- club_makeup %>% filter(Club==club_name)
  country_cnt <- club_df %>% pull(cnt)
  country_pct <- round(club_df %>% pull(pct) * 100,2)
  
  age <- players_in_league %>% select(Club,StartingAverageAge) %>% filter(Club==club_name) %>% slice(1) %>% 
    pull(StartingAverageAge)
  
  label_text <- glue::glue("**{club_name}**<br>
                           <span style = 'color:#0072B2;'>{country_cnt} French Players ({country_pct})%</span><br>
                           <span style = 'font-size:6pt;'>Starting XI Average age {age}</span>")
  labels <- c(labels, label_text)
}


p1 <- ggplot() + 
  geom_beeswarm(data=players_in_league,aes(x=Overall_player,y=Club,color=in_country,alpha=0.5)) + 
  geom_text_repel(data=top_players,aes(x=Overall_player,y=Club,label=FullName),hjust=0,
  size=3, segment.size=0.25, nudge_x=0.5, direction="y") +
  scale_y_discrete(name = NULL, 
                   labels = labels, 
                   expand = c(0.05,1)) +
  scale_x_continuous(limits=c(55,100),
                     breaks = seq(55,100,5)) + 
  scale_color_manual(values=c("blue","darkgrey")) + 
  theme_classic() + 
  labs(x="Overall Player Rating",y="",
       title = "Ligue 1 Player + Team Ratings") + 
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        panel.grid.major = element_line(),
        #axis.text.y = element_text(face="bold"),
        axis.text.y = ggtext::element_markdown())


clubs <- players_in_league %>% select(Club,Defence,Midfield,Attack) %>% 
  distinct() %>% 
  pivot_longer(-Club) %>% 
  mutate(
    value2 = 100
  )

# A pie/donut chart is a bar chart with polar coordinates
# Add polar coordinates and set the direction to -1 
# so the filled in part starts at the top and goes clockwise
# Specify x=2 and add xlim to get the donut hole 

font_family = "Inter"

library(ggh4x)

p2 <- ggplot(clubs) +
  geom_bar(aes(x = 2,
               y = value2,
               fill = "grey"), stat = "identity") +
  geom_bar(aes(x = 2,
               y = value,
               fill = name), stat = "identity") +
  facet_grid(fct_rev(Club)~name) + 
  coord_polar(theta = "y",
              direction = -1) + 
  xlim(.2,2.5) + 
  theme_void() + 
  theme(legend.position = "none",
        strip.text = element_text(colour = 'grey40',face="bold"),
        strip.text.y = element_blank()) + 
  geom_text(
    aes(label = value,color=name),
    x = 0, y = 0
  ) + 
  scale_fill_manual(
    values = c("yellow4","grey35","grey","aquamarine4")
  ) + 
  scale_color_manual(
    values = c("yellow4","grey35","aquamarine4")
  ) + 
  force_panelsizes(rows = 2, cols = 4, TRUE)
  

library(patchwork)

p1 + p2
