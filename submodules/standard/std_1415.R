## Getting standard player data for 2014/15 season

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
library(fuzzyjoin)

#Set directory
df2 <- read.csv("~/FPL/submodules/positions/positions_1415.csv") %>%
  rename_at(1, ~"Player") %>% mutate(Player=stri_trans_general(Player, "Latin-ASCII"))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
maindir <- getwd()

## https://jaseziv.github.io/worldfootballR/articles/extract-fbref-data.html#overview
### Documentation of worldfoortballR above

#Player stats
df <- fb_league_stats(country="ENG", gender = "M", season_end_year = 2015,
                      tier="1st", stat_type = "standard",
                      team_or_player = "player") %>%
  mutate(Season=2015) %>%
  select(-c("Rk", "Matches", "url")) %>%
  select(Player, Season, everything()) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))

df3 <- df %>%
  select(-c("Squad")) %>%
  group_by(Player, Nation, Age, Born, Pos, Season) %>%
  summarize(across( -c(`Mins_Per_90_Playing Time`,
                       `Gls_Per 90 Minutes`, `Ast_Per 90 Minutes`,
                       `G+A_Per 90 Minutes`, `G_minus_PK_Per 90 Minutes`,
                       `G+A_minus_PK_Per 90 Minutes`), sum),
            `Mins_Per_90_Playing Time`=mean(`Mins_Per_90_Playing Time`),
            `Gls_Per 90 Minutes`=mean(`Gls_Per 90 Minutes`),
            `Ast_Per 90 Minutes`=mean(`Ast_Per 90 Minutes`),
            `G+A_Per 90 Minutes`=mean(`G+A_Per 90 Minutes`),
            `G_minus_PK_Per 90 Minutes`=mean(`G_minus_PK_Per 90 Minutes`),
            `G_minus_PK_Per 90 Minutes`=mean(`G_minus_PK_Per 90 Minutes`)) %>%
  ungroup()
gc()

df2 <- df2 %>% select(Player,Team,Position,CS,Points,Bonus) %>%
  filter(Points != 0)
temp <- df$Player
temp2 <- df2$Player
setdiff(temp2, temp)

temp <- df %>% select(Player, Squad) %>%
  distinct(Player, .keep_all = T)

df4 <- df3 %>%
  left_join(df2, by=c("Player")) %>%
  mutate(CS=ifelse(grepl("Jazz Richards", Player), 2, CS)) %>%
  mutate(Points=ifelse(grepl("Jazz Richards", Player), 24, Points)) %>%
  mutate(Bonus=ifelse(grepl("Jazz Richards", Player),2, Bonus)) %>%
  mutate(CS=ifelse(grepl("Roberge", Player), 0, CS)) %>%
  mutate(Points=ifelse(grepl("Roberge", Player), 0, Points)) %>%
  mutate(Bonus=ifelse(grepl("Roberge", Player), 0, Bonus)) %>%
  mutate(Pos=Position) %>%
  select(-c("Position")) %>%
  distinct(Player, .keep_all = T) %>%
  left_join(temp, by=c("Player")) %>%
  select(Player, Nation, Age, Born, Pos, Squad, Season, Points, Bonus,
         `MP_Playing Time`, `Starts_Playing Time`, `Min_Playing Time`,
         Gls, Ast, `G+A`, G_minus_PK, PK, PKatt, CrdY, CrdR,
         `Mins_Per_90_Playing Time`, `Gls_Per 90 Minutes`, `Ast_Per 90 Minutes`,
         `G+A_Per 90 Minutes`, `G_minus_PK_Per 90 Minutes`, CS)
gc()

maindir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(maindir)), dir.create(file.path(maindir)), FALSE)
write.csv(df4, file="outputs/std_1415.csv", row.names = F)