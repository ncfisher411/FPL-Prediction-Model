# Get league matches, clean sheets for 2017/18 season

# Load packages
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

# Set directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
maindir <- getwd()

## Need match day results to get clean sheets
#ids <- fb_match_urls(
#  country = "ENG",
#  gender = "M",
#  season_end_year = 2018,
#  tier = "1st"
#)

#df <- fb_match_lineups(ids)
#write.csv(df, "match lineups/lineups1718.csv", row.names = F)
df <- read.csv("match lineups/lineups1718.csv")
gc()

#df2 <- fb_match_report(ids)
#write.csv(df2, file="game_results/results1718.csv")
df2 <- read.csv("game_results/results1718.csv")
gc()

df3 <- df %>%
  left_join(df2, by=c("Matchday"="Match_Date", "MatchURL"="Game_URL")) %>%
  mutate(played=ifelse(Min>0,1,0)) %>%
  mutate(played60=ifelse(Min>60,1,0)) %>%
  mutate(time_points=(1*played)+(1*played60)) %>%
  mutate(clean_sheet=ifelse(Home_Away=="Home" &
                              Min>60 &
                              Away_Score==0,1,0)) %>%
  mutate(Player_Name=stri_trans_general(Player_Name, "Latin-ASCII")) %>%
  mutate(concede2=ifelse(Away_Score>1 & played==1,1,0)) %>%
  mutate(concede4=ifelse(Away_Score>3 & played==1,1,0)) %>%
  mutate(concede6=ifelse(Away_Score>5 & played==1,1,0)) %>%
  mutate(concede8=ifelse(Away_Score>7 & played==1,1,0)) %>%
  mutate(concede10=ifelse(Away_Score>9 & played==1,1,0))
gc()

df4 <- df3 %>%
  mutate(Player_Name=stri_trans_general(Player_Name, "Latin-ASCII")) %>%
  group_by(Player_Name) %>%
  summarize(time_points=sum(time_points, na.rm=T),
            clean_sheets=sum(clean_sheet, na.rm=T),
            concede2=sum(concede2, na.rm = T),
            concede4=sum(concede4, na.rm = T),
            concede6=sum(concede6, na.rm = T),
            concede8=sum(concede8, na.rm=T),
            concede10=sum(concede10, na.rm=T)) %>%
  mutate(Season=2018)
gc()

df5 <- df3 %>%
  filter(grepl("(OG)",Away_Goals) |
           grepl("(OG)",Home_Goals)) %>%
  mutate(temp_away=str_split(Away_Goals, ";")) %>%
  unnest(temp_away) %>%
  mutate(temp_home=str_split(Home_Goals, ";")) %>%
  unnest(temp_home) %>%
  filter(str_detect(temp_away, Player_Name) & grepl("(OG)", temp_away) |
           str_detect(temp_home, Player_Name) & grepl("(OG)", temp_home)) %>%
  filter(!duplicated(Player_Name, MatchURL)) %>%
  mutate(own_goal=1) %>%
  mutate(Season=2018) %>%
  group_by(Player_Name, Season) %>%
  summarize(own_goals=sum(own_goal))
gc()

dir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(maindir)), dir.create(file.path(maindir)), FALSE)
write.csv(df4, file = "outputs/time_and_cs_1718.csv", row.names = F)
write.csv(df5, file="outputs/own_goals_1718.csv", row.names = F)
