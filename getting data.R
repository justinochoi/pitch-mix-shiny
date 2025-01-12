library(tidyverse) 
library(arrow)

# load in data 
statcast_24 <- read_parquet(
  "https://huggingface.co/datasets/Jensen-holm/statcast-era-pitches/resolve/main/data/statcast_era_pitches.parquet"
) %>% 
  filter(between(game_date, as.POSIXct('2024-04-01 19:00:00'), as.POSIXct('2024-09-30 19:00:00'))) 

# create pitch categories 
statcast_24 <- statcast_24 %>% 
  filter(!pitch_type %in% c('CS','EP','KN','PO','SV')) %>% 
  mutate(
    pitch_category = case_when(
      pitch_type %in% c('FA','FF','SI','FC') ~ 'fastball', 
      pitch_type %in% c('SL','CU','KC','ST') ~ 'breaking', 
      .default = 'offspeed'
    )
  )

# inning totals from fangraphs 
fg_sp <- read.csv(file.choose()) 

# only starting pitchers with min. 100 innings and 10% usage for each pitch type
pitchers <- statcast_24 %>% 
  filter(pitcher %in% fg_sp$MLBAMID) %>% 
  group_by(player_name) %>% 
  mutate(
    pitches = n(), 
    fastball_pct = sum(pitch_category == 'fastball') / pitches, 
    breaking_pct = sum(pitch_category == 'breaking') / pitches, 
    offspeed_pct = sum(pitch_category == 'offspeed') / pitches
  ) %>% 
  filter(offspeed_pct > .1 & breaking_pct > .1 & offspeed_pct > .1) %>% 
  ungroup() %>% 
  group_by(player_name, game_date) %>% 
  summarize(
    pitches = n(), 
    fastball_pct = sum(pitch_category == 'fastball') / pitches, 
    breaking_pct = sum(pitch_category == 'breaking') / pitches, 
    offspeed_pct = sum(pitch_category == 'offspeed') / pitches, 
    mean_woba = mean(woba_value, na.rm = T)
  )

# save result as csv file 
write.csv(pitchers, "pitchers.csv") 
  
  
  
  
