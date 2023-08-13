# Get league matches, clean sheets for 2013/14 season

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
#df <- fotmob_get_league_matches(
#  country = "ENG",
#  league_name = "Premier League",
#  season = "2013/2014"
#)
gc()
#ids <- df$id
#write.csv(df, "game_results/results1314.csv", row.names = F)
df <- read.csv("game_results/results1314.csv")

#df2 <- fotmob_get_match_players(ids)
#write.csv(df2, "match lineups/lineups1314.csv")
df2 <- read.csv("match lineups/lineups1314.csv")
gc()

df3 <- df %>% 
  rename(score=`status.scoreStr`) %>%
  separate_wider_delim(score, " - ", names=c("home_score","away_score")) %>%
  mutate(home_score==as.numeric(home_score)) %>%
  mutate(away_score==as.numeric(away_score)) %>%
  rename(match_id=id) %>%
  data.frame() %>%
  mutate_at(c("match_id"), as.numeric) %>%
  select(match_id, home_score, away_score)

df4 <- df2 %>%
  filter(is_home_team==TRUE) %>%
  rename(home_team=team_name) %>%
  mutate(player=paste(first_name, last_name, sep=" ")) %>%
  select(match_id, home_team, player, time_subbed_on,
         time_subbed_off, is_starter, is_home_team) %>%
  left_join(df3, by=c("match_id")) %>%
    mutate(played=ifelse(is_starter==TRUE | !is.na(time_subbed_on),1,0)) %>%
  mutate(player=stri_trans_general(player, "Latin-ASCII")) %>%
  mutate(played60=ifelse(is_starter==TRUE & is.na(time_subbed_off) |
                           is_starter==TRUE & time_subbed_off>59 |
                           90-time_subbed_on>59, 1, 0)) %>%
  mutate(time_points=(1*played)+(1*played60)) %>%
  mutate(clean_sheet=ifelse(away_score==0 & played60==1,1,0)) %>%
  mutate(concede2=ifelse(away_score>1 & played==1,1,0)) %>%
  mutate(concede4=ifelse(away_score>3 & played==1,1,0)) %>%
  mutate(concede6=ifelse(away_score>5 & played==1,1,0)) %>%
  mutate(concede8=ifelse(away_score>7 & played==1,1,0)) %>%
  mutate(concede10=ifelse(away_score>9 & played==1,1,0)) %>%
  rename(team_name=home_team) %>%
  select(player, team_name, clean_sheet, time_points,
         concede2, concede4, concede6, concede8, concede10)

df5 <- df2 %>%
  filter(is_home_team==FALSE) %>%
  rename(away_team=team_name) %>%
  mutate(player=paste(first_name, last_name, sep=" ")) %>%
  select(match_id, away_team, player, time_subbed_on,
                 time_subbed_off, is_starter) %>%
  left_join(df3, by=c("match_id")) %>%
  mutate(played=ifelse(is_starter==TRUE | !is.na(time_subbed_on),1,0)) %>%
  mutate(player=stri_trans_general(player, "Latin-ASCII")) %>%
  mutate(played60=ifelse(is_starter==TRUE & is.na(time_subbed_off) |
                           is_starter==TRUE & time_subbed_off>59 |
                           90-time_subbed_on>59, 1, 0)) %>%
  mutate(time_points=(1*played)+(1*played60)) %>%
  mutate(clean_sheet=ifelse(away_score==0 & played60==1,1,0)) %>%
  mutate(concede2=ifelse(away_score>1 & played==1,1,0)) %>%
  mutate(concede4=ifelse(away_score>3 & played==1,1,0)) %>%
  mutate(concede6=ifelse(away_score>5 & played==1,1,0)) %>%
  mutate(concede8=ifelse(away_score>7 & played==1,1,0)) %>%
  mutate(concede10=ifelse(away_score>9 & played==1,1,0)) %>%
  rename(team_name=away_team) %>%
  select(player, team_name, clean_sheet, time_points, concede2,
         concede4, concede6, concede8, concede10)

temp <- df2 %>%
  mutate(player=paste(first_name, last_name, sep=" ")) %>%
  select(player, team_name) %>%
  mutate(dummy=1) %>%
  mutate(team_name=ifelse(grepl("West Ham", team_name), "West Ham", team_name)) %>%
  mutate(team_name=ifelse(grepl("Swansea", team_name), "Swansea City", team_name)) %>%
  mutate(team_name=ifelse(grepl("Tottenham", team_name), "Tottenham", team_name)) %>%
  mutate(team_name=ifelse(grepl("Manchester United", team_name), "Manchester Utd", team_name)) %>%
  mutate(team_name=ifelse(grepl("Newcastle United", team_name), "Newcastle Utd", team_name)) %>%
  mutate(team_name=ifelse(grepl("Hull", team_name), "Hull City", team_name)) %>%
  mutate(team_name=ifelse(grepl("Cardiff", team_name), "Cardiff City", team_name)) %>%
  mutate(team_name=ifelse(grepl("Norwich", team_name), "Norwich City", team_name)) %>%
  mutate(team_name=ifelse(grepl("Stoke", team_name), "Stoke City", team_name)) %>%
  mutate(team_name=ifelse(grepl("West Bromwich", team_name), "West Brom", team_name)) %>%
  mutate(player=stri_trans_general(player, "Latin-ASCII")) %>%
  group_by(player, team_name) %>%
  summarize(dummy=sum(dummy)) %>%
  ungroup() %>%
  select(player, team_name)


df6 <- rbind(df4, df5) %>%
  group_by(player) %>%
  summarize(clean_sheets=sum(clean_sheet, na.rm=T),
            time_points=sum(time_points, na.rm=T),
            concede2=sum(concede2, na.rm = T),
            concede4=sum(concede4, na.rm=T),
            concede6=sum(concede6, na.rm=T),
            concede8=sum(concede8, na.rm=T),
            concede10=sum(concede10, na.rm=T)) %>%
  left_join(temp, by=c("player"))

#df7 <- fb_match_results(
#  country="ENG",
#  gender="M",
#  season_end_year = 2014,
#  tier="1st"
#)
gc()
#ids <- df7$MatchURL

#df8 <- fb_match_report(ids)
#write.csv(df8, "game_results/fbref_1314.csv", row.names = F)
df8 <- read.csv("game_results/fbref_1314.csv")

df9 <- df %>%
  left_join(df8, by=c("home.name"="Home_Team", "away.name"="Away_Team")) %>%
  select(id, Home_Goals, Away_Goals)

df9 <- df2 %>%
  left_join(df9, by=c("match_id"="id")) %>%
  mutate(player=paste(first_name, last_name, sep=" ")) %>%
  select(match_id, player, Home_Goals, Away_Goals) %>%
  filter(grepl("(OG)", Home_Goals) | grepl("(OG)", Away_Goals)) %>%
  mutate(temp_home=str_split(Home_Goals, ";")) %>%
  unnest(temp_home) %>%
  mutate(temp_away=str_split(Away_Goals, ";")) %>%
  unnest(temp_away) %>%
  filter(str_detect(temp_away, player) & grepl("(OG)", temp_away) |
           str_detect(temp_home, player) & grepl("(OG)", temp_home)) %>%
  filter(!duplicated(player, match_id)) %>%
  mutate(own_goal=1) %>%
  group_by(player) %>%
  summarize(own_goals=sum(own_goal))

df6 <- df6 %>%
  left_join(df9, by=c("player")) %>%
  mutate(Season=2014)

dir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(dir)), dir.create(file.path(dir)), FALSE)
write.csv(df6, file = "outputs/cs_time_own_goals_1314.csv", row.names = F)

