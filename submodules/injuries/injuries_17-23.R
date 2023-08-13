# Collect injury data
library(worldfootballR)
library(tidyverse)

setwd("~/FPL/Prediction Model/submodules/injuries")
outfield <- read_csv("~/FPL/Prediction Model/outfield_players.csv")
map <- player_dictionary_mapping() %>% 
  filter(PlayerFBref %in% outfield$Player)

injuries <- data.frame()
for(i in map$UrlTmarkt) {
  stats <- tm_player_injury_history(player_urls = i)
  injuries <- bind_rows(injuries, stats)
}

df <- injuries %>% left_join(map, by=c("player_url"="UrlTmarkt")) %>%
  mutate(Season=ifelse(season_injured=="16/17", 2017, NA)) %>%
  mutate(Season=ifelse(season_injured=="17/18", 2018, Season)) %>%
  mutate(Season=ifelse(season_injured=="18/19", 2019, Season)) %>%
  mutate(Season=ifelse(season_injured=="19/20", 2020, Season)) %>%
  mutate(Season=ifelse(season_injured=="20/21", 2021, Season)) %>%
  mutate(Season=ifelse(season_injured=="21/22", 2022, Season)) %>%
  mutate(Season=ifelse(season_injured=="22/23", 2023, Season)) %>%
  mutate(games_missed=as.numeric(games_missed)) %>%
  filter(!is.na(Season)) %>%
  group_by(PlayerFBref, Season) %>%
  summarize(games_missed=sum(games_missed)) %>%
  ungroup()

write.csv(df, "injuries.csv")