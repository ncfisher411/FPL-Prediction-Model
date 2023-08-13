## Getting keeper player data for 2019/20 season

library(tidyverse)
library(rstudioapi)
library(rvest)
library(janitor)
library(prismatic)
library(broom)
library(purrr)
library(worldfootballR)
library(fplr)
library(fplscrapR)
library(stringi)

#Set directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
maindir <- getwd()

## https://jaseziv.github.io/worldfootballR/articles/extract-fbref-data.html#overview
### Documentation of worldfoortballR above

#Player stats
df <- fb_league_stats(country="ENG", gender = "M", season_end_year = 2020,
                      tier="1st", stat_type = "keepers",
                      team_or_player = "player") %>%
  mutate(Season=2020) %>% select(-c("Rk")) %>%
  select(Player, Squad, Season, everything())
gc()

dir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(dir)), dir.create(file.path(dir)), FALSE)
write.csv(df, file="outputs/keeper_1920.csv", row.names = F)