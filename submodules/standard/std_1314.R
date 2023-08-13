## Getting standard player data for 2013/14 season

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
df2 <- read.csv("~/FPL/submodules/positions/positions_1314.csv") %>%
  rename_at(1, ~"Player") %>% mutate(Player=stri_trans_general(Player, "Latin-ASCII"))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
maindir <- getwd()

## https://jaseziv.github.io/worldfootballR/articles/extract-fbref-data.html#overview
### Documentation of worldfoortballR above

#Player stats
df <- fb_league_stats(country="ENG", gender = "M", season_end_year = 2014,
                      tier="1st", stat_type = "standard",
                      team_or_player = "player") %>%
  mutate(Season=2014) %>%
  select(-c("Rk", "Matches", "url")) %>%
  select(Player, Squad, Season, everything()) %>% 
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))
gc()
df[c("First", "Last")] <- str_split_fixed(df$Player, " ", 2)

df3 <- df %>% 
  mutate_if(is.character, list(~na_if(.,""))) %>%
  mutate(Last=ifelse(is.na(Last), First, Last)) %>%
  select(-c("First", "Squad")) %>%
  group_by(Player, Nation, Age, Born, Pos, Last, Season) %>%
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
  ungroup() %>%
  mutate(Last=ifelse(grepl("Alou", Player), "Alou Diarra", Last)) %>%
  mutate(Last=ifelse(grepl("Berget", Player), "Berget", Last)) %>%
  mutate(Last=ifelse(grepl("Oxlade", Player), "Chamberlain", Last)) %>%
  mutate(Last=ifelse(grepl("Daehli", Player), "Daehli", Last)) %>%
  mutate(Last=ifelse(grepl("David Luiz", Player), "David Luiz", Last)) %>%
  mutate(Last=ifelse(grepl("Eikrem", Player), "Eikrem", Last)) %>%
  mutate(Last=ifelse(grepl("Elliott Bennett", Player), "Elliott Bennett", Last)) %>%
  mutate(Last=ifelse(grepl("Guly", Player), "Guly", Last)) %>%
  mutate(Last=ifelse(grepl("David Luiz", Player), "David Luiz", Last)) %>% 
  mutate(Last=ifelse(grepl("Javi Garcia", Player), "Javi Garcia", Last)) %>%
  mutate(Last=ifelse(grepl("Ji Dong-won", Player), "Ji Dong-Won", Last)) %>%
  mutate(Last=ifelse(grepl("Joe Cole", Player), "Joe Cole", Last)) %>%
  mutate(Last=ifelse(grepl("Jose Canas", Player), "Jose Canas", Last)) %>%
  mutate(Last=ifelse(grepl("Jose Enrique", Player), "Jose Enrique", Last)) %>%
  mutate(Last=ifelse(grepl("David Luiz", Player), "David Luiz", Last)) %>%
  mutate(Last=ifelse(grepl("Kelvin Davis", Player), "Kelvin Davis", Last)) %>%
  mutate(Last=ifelse(grepl("Ki Sung-yueng", Player), "Ki Sung-Yueng", Last)) %>%
  mutate(Last=ifelse(grepl("Kim Bo-kyung", Player), "Kim Bo-Kyung", Last)) %>%
  mutate(Last=ifelse(grepl("Luis Alberto", Player), "Luis Alberto", Last)) %>%
  mutate(Last=ifelse(grepl("John Obi Mikel", Player), "Mikel", Last)) %>%
  mutate(Last=ifelse(grepl("John Arne Riise", Player), "Riise", Last)) %>%
  mutate(Last=ifelse(grepl("Ryan Bennett", Player), "Ryan Bennett", Last)) %>%
  mutate(Last=ifelse(grepl("Luis Alberto", Player), "Luis Alberto", Last)) %>%
  mutate(Last=ifelse(grepl("Sammy Ameobi", Player), "Sammy Ameobi", Last)) %>%
  mutate(Last=ifelse(grepl("Shola Ameobi", Player), "Shola Ameobi", Last)) %>%
  mutate(Last=ifelse(grepl("Steven Davis", Player), "Steven Davis", Last)) %>%
  mutate(Last=ifelse(grepl("Steven Taylor", Player), "Steven Taylor", Last)) %>%
  mutate(Last=ifelse(grepl("Luis Alberto", Player), "Luis Alberto", Last)) %>%
  mutate(Last=ifelse(grepl("Carlos Teixeira", Player), "Teixeira", Last)) %>%
  mutate(Last=ifelse(grepl("Luis Alberto", Player), "Luis Alberto", Last)) %>%
  mutate(Last=ifelse(grepl("Thievy", Player), "Thievy", Last)) %>%
  mutate(Last=ifelse(grepl("Theophile", Player), "Theophile", Last)) %>%
  mutate(Last=ifelse(grepl("van Persie", Player), "Van Persie", Last)) %>%
  mutate(Last=ifelse(grepl("Yaya Toure", Player), "Yaya Toure", Last)) %>%
  mutate(Last=ifelse(grepl("Luis Alberto", Player), "Luis Alberto", Last)) %>%
  mutate(Last=ifelse(grepl("Pozuelo", Player), "Alex Pozuelo", Last)) %>%
  mutate(Last=ifelse(grepl("Alvaro Vazquez", Player), "Alvaro", Last)) %>%
  mutate(Last=ifelse(grepl("N'Gog", Player), "Ngog", Last))

df2 <- df2 %>% select(Player,Team,Pos,CS,Points,Bonus) %>%
  filter(Points != 0)
temp <- df3$Last
temp2 <- df2$Player
setdiff(temp2, temp)

temp <- df %>% select(Player, Squad) %>%
  distinct(Player, .keep_all = T)

df4 <- df3 %>%
  left_join(df2, by=c("Last"="Player")) %>%
  distinct(Player, .keep_all = T) %>%
  mutate(Pos.x=Pos.y) %>%
  select(-c("Last", "Team", "Pos.y")) %>%
  left_join(temp, by=c("Player")) %>%
  select(Player, Nation, Age, Born, Pos.x, Squad, Season, Points, Bonus,
         `MP_Playing Time`, `Starts_Playing Time`, `Min_Playing Time`,
         Gls, Ast, `G+A`, G_minus_PK, PK, PKatt, CrdY, CrdR,
         `Mins_Per_90_Playing Time`, `Gls_Per 90 Minutes`, `Ast_Per 90 Minutes`,
         `G+A_Per 90 Minutes`, `G_minus_PK_Per 90 Minutes`, CS) %>%
  rename(Pos=Pos.x)
  

dir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(dir)), dir.create(file.path(dir)), FALSE)
write.csv(df4, file="outputs/std_1314.csv", row.names = F)