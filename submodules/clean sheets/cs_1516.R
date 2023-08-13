# Get league matches, clean sheets for 2015/16 season

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

### Need match day results to get clean sheets
#ids <- fb_match_urls(
#  country="ENG",
#  gender = "M",
#  season_end_year = 2016,
#  tier = "1st"
#)
gc()

#df <- fb_match_lineups(ids)
## Save so you don't have to run this command again; takes time
ifelse(!dir.exists(file.path(paste(maindir, 'game_results', sep='/'))),
       dir.create(file.path(paste(maindir, 'game_results', sep='/'))), FALSE)
ifelse(!dir.exists(file.path(paste(maindir, 'match lineups', sep='/'))),
       dir.create(file.path(paste(maindir, 'match lineups', sep='/'))), FALSE)
#write.csv(df, file='match lineups/lineups1516.csv', row.names = F)
df <-read.csv('match lineups/lineups1516.csv')
gc()

#df2 <- fb_match_report(match_url=ids)
#write.csv(df2, file='game_results/results1516.csv', row.names = F)
df2 <- read.csv('game_results/results1516.csv') %>%
  select(Match_Date, Home_Team, Away_Team, Home_Score, Away_Score,
         Home_Goals, Away_Goals, Game_URL)
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
  mutate(Season=2016)
gc()

# Can also get own goals from this information
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
  mutate(Season=2016) %>%
  group_by(Player_Name, Season) %>%
  summarize(own_goals=sum(own_goal))
gc()

## Save files
dir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(maindir)), dir.create(file.path(maindir)), FALSE)
write.csv(df4, file = "outputs/time_and_cs_1516.csv", row.names = F)
write.csv(df5, file="outputs/own_goals_1516.csv", row.names = F)