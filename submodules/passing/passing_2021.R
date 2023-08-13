## Getting passing data 2020/21 season

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

#Player stats
df <- fb_league_stats(country="ENG", gender = "M", season_end_year = 2021,
                      tier="1st", stat_type = "passing",
                      team_or_player = "player") %>%
  mutate(Season=2021) %>%
  select(-c("Rk")) %>%
  select(Player, Squad, Season, everything())
gc()

dir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(dir)), dir.create(file.path(dir)), FALSE)
write.csv(df, file="outputs/passing_2021.csv", row.names = F)