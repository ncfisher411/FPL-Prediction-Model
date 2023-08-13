## Getting shooting player data for 2021/22 season

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
df <- fb_league_stats(country="ENG", gender = "M", season_end_year = 2022,
                      tier="1st", stat_type = "shooting",
                      team_or_player = "player") %>%
  mutate(Season=2022) %>%
  select(-c("Rk")) %>%
  select(Player, Squad, Season, everything())
gc()

maindir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(maindir)), dir.create(file.path(maindir)), FALSE)
write.csv(df, file="outputs/sht_2122.csv", row.names = F)