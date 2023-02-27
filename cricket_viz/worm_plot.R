create_match_worm_plot <- function(match_df,league_name){
  
  tbl <- create_table_summary(match_df)
  gtsave(tbl,"table.png")
  table_png <- png::readPNG("table.png", native = TRUE)
  
  target <- match_df %>% filter(innings==2) %>% slice_min(balls_remaining)
  venue <- target %>% pull(venue) %>% str_remove(., ",.*")
  date <- target %>% pull(start_date)
  
  inning_1 <- match_df %>% filter(innings==1)
  first_bat_team <- inning_1 %>% pull(batting_team) %>% unique()
  plot_1 <- create_team_worm_plot(inning_1,1)
  
  inning_2 <- match_df %>% filter(innings==2)
  second_bat_team <- inning_2 %>% pull(batting_team) %>% unique()
  plot_2 <- create_team_worm_plot(inning_2,2)
  
  title <- glue::glue("{first_bat_team} vs {second_bat_team}")
  subtitle <- glue::glue("{league_name} | {venue} | {date}")
  
  # patchwork
  patchwork <- (plot_1 + plot_2) / table_png
  patchwork + plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = '@msubbaiah1',
    theme = theme(plot.title = element_text(size = 20))
  )
}

create_team_worm_plot <- function(df,innings){
  wickets <- df %>% filter(wicket==TRUE) %>%
    arrange(-balls_remaining) %>% 
    mutate(
      wicket_num = cumsum(wicket)
    )
  
  bat_team <- df %>% pull(batting_team) %>% unique()
  final_ball <- df %>% slice_min(balls_remaining)
  runs_scored <- final_ball %>% pull(runs_scored_yet)
  wickets_lost <- final_ball %>% pull(wickets_lost_yet)
  
  title = glue::glue("{bat_team}: {runs_scored}/{wickets_lost} ")
  
  if(innings==1){
    # empty df for first innings, don't want to draw the target line
    target <- data.frame()
  }
  if(innings==2){
    target <- df %>% filter(innings==2) %>% slice_min(balls_remaining)
  }
  
  
  ggplot(df,aes(x=balls_remaining,y=runs_scored_yet)) + 
    geom_line() + 
    geom_vline(data=wickets,aes(xintercept=balls_remaining),linetype="dashed",color="firebrick3") + 
    geom_hline(data=target,aes(yintercept=target)) + 
    geom_label_repel(data=wickets,aes(label=wicket_num),box.padding = 0.5,fill="pink") + 
    labs(x="Balls Remaining",
         y="Runs",
         title = title) + 
    theme_light(base_size = 12) + 
    scale_x_reverse(limits=c(120,0),
                    expand=c(0,0)) 
  
}

create_table_summary <- function(df){
  df %>%
    group_by(batting_team) %>%
    summarise(
      Dots = sum(runs_off_bat == 0, na.rm = T),
      Ones = sum(runs_off_bat == 1, na.rm = T),
      Twos = sum(runs_off_bat == 2, na.rm = T) ,
      Fours = sum(runs_off_bat == 4, na.rm = T),
      Sixes = sum(runs_off_bat == 6, na.rm = T),
      balls_faced = sum(!extra_ball),
      #BP = Boundary Percent (% of balls hit for boundaries)
      dot_pct = round(Dots / sum(balls_faced), 2),
      bound_pct = round(sum(Fours + Sixes) / sum(balls_faced), 2)
    ) %>% 
    gt() %>% 
    cols_label(
      batting_team = "",
      balls_faced = "Balls Faced",
      dot_pct = "Dot %",
      bound_pct = "Boundary %"
    ) %>% 
    fmt_percent(
      columns = dot_pct:bound_pct,
      decimals = 1
    ) 
}


library(cricketdata)
library(patchwork)
library(gt)
library(ggplot2)
library(ggrepel)

ipl_bbb_df = fetch_cricsheet(competition = "ipl",gender = "male", type="bbb")

rr <- ipl_bbb_df %>% filter(season==2022) %>% slice_max(start_date) %>% 
  mutate(
    runs_off_ball = runs_off_bat + extra_ball,
    runs_off_ball = factor(runs_off_ball),
    phase = case_when(
      over <= 6 ~ "PowerPlay",
      over > 6 & over <= 15 ~ "Middle",
      over > 15 ~ "Death"
    )
  ) 


create_match_worm_plot(rr,"Indian Premier League")
ggsave("cricket_viz/ipl_22_final.png",height=12,width=16)
