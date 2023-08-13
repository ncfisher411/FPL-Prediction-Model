## Getting standard player data for 2016/17 season

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

df2 <- read.csv("~/FPL/submodules/positions/positions_1617.csv") %>%
  rename(Pos=Position)
## https://jaseziv.github.io/worldfootballR/articles/extract-fbref-data.html#overview
### Documentation of worldfoortballR above

#Player stats
df <- fb_league_stats(country="ENG", gender = "M", season_end_year = 2017,
                      tier="1st", stat_type = "standard",
                      team_or_player = "player") %>%
  mutate(Season=2017) %>%
  select(-c("Rk", "Matches", "url")) %>%
  select(Player, Squad, Season, everything()) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))
gc()
df[c("First", "Last")] <- str_split_fixed(df$Player, " ", 2)

df3 <- df %>%
  mutate_if(is.character, list(~na_if(.,""))) %>%
  mutate(Last=ifelse(is.na(Last), First, Last)) %>%
  select(-c("Squad", "First")) %>%
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
  mutate(Last=ifelse(grepl("Bernardo", Player), "Bernardo", Last)) %>%
  mutate(Last=ifelse(grepl("Callum Wilson", Player), "Callum Wilson", Last)) %>%
  mutate(Last=ifelse(grepl("Tom Carroll", Player), "Tom Carroll", Last)) %>%
  mutate(Last=ifelse(grepl("Andy Carroll", Player), "Andy Carroll", Last)) %>%
  mutate(Last=ifelse(grepl("Chamberlain", Player), "Chamberlain", Last)) %>%
  mutate(Last=ifelse(grepl("Tom Carroll", Player), "Tom Carroll", Last)) %>%
  mutate(Last=ifelse(grepl("Christian Benteke", Player), "Christian Benteke", Last)) %>%
  mutate(Last=ifelse(grepl("Jonathan Benteke", Player), "Jonathan Benteke", Last)) %>%
  mutate(Last=ifelse(grepl("David Luiz", Player), "David Luiz", Last)) %>%
  mutate(Last=ifelse(grepl("Curtis Davies", Player), "Curtis Davies", Last)) %>%
  mutate(Last=ifelse(grepl("Ben Davies", Player), "Ben Davies", Last)) %>%
  mutate(Last=ifelse(grepl("Tom Davies", Player), "Tom Davies", Last)) %>%
  mutate(Last=ifelse(grepl("Michael Dawson", Player), "Michael Dawson", Last)) %>%
  mutate(Last=ifelse(grepl("Craig Dawson", Player), "Craig Dawson", Last)) %>%
  mutate(Last=ifelse(grepl("Christian Benteke", Player), "Christian Benteke", Last)) %>%
  mutate(Last=ifelse(grepl("Diouf", Player), "Diouf", Last)) %>%
  mutate(Last=ifelse(grepl("Evandro", Player), "Evandro", Last)) %>%
  mutate(Last=ifelse(grepl("Darren Fletcher", Player), "Darren Fletcher", Last)) %>%
  mutate(Last=ifelse(grepl("Ashley Fletcher", Player), "Ashley Fletcher", Last)) %>%
  mutate(Last=ifelse(grepl("Darron Gibson", Player), "Darron Gibson", Last)) %>%
  mutate(Last=ifelse(grepl("Ben Gibson", Player), "Ben Gibson", Last)) %>%
  mutate(Last=ifelse(grepl("Heurelho", Player), "Heurelho Gomes", Last)) %>%
  mutate(Last=ifelse(grepl("Angel Gomes", Player), "Angel Gomes", Last)) %>%
  mutate(Last=ifelse(grepl("Andre Gray", Player), "Andre Gray", Last)) %>%
  mutate(Last=ifelse(grepl("Demarai", Player), "Demarai Gray", Last)) %>%
  mutate(Last=ifelse(grepl("Christian Benteke", Player), "Christian Benteke", Last)) %>%
  mutate(Last=ifelse(grepl("Gudmundsson", Player), "Gudmundsson", Last)) %>%
  mutate(Last=ifelse(grepl("Gueye", Player), "Gueye", Last)) %>%
  mutate(Last=ifelse(grepl("Abel Hernandez", Player), "Abel Hernandez", Last)) %>%
  mutate(Last=ifelse(grepl("Luis Hernandez", Player), "Luis Hernandez", Last)) %>%
  mutate(Last=ifelse(grepl("Joel Castro", Player), "Joel Pereira", Last)) %>%
  mutate(Last=ifelse(grepl("Jonathan Benteke", Player), "Jonathan Benteke", Last)) %>%
  mutate(Last=ifelse(grepl("David Jones", Player), "David Jones", Last)) %>%
  mutate(Last=ifelse(grepl("Phil Jones", Player), "Phil Jones", Last)) %>%
  mutate(Last=ifelse(grepl("Billy Jones", Player), "Billy Jones", Last)) %>%
  mutate(Last=ifelse(grepl("Michael Keane", Player), "Michael Keane", Last)) %>%
  mutate(Last=ifelse(grepl("Will Keane", Player), "Will Keane", Last)) %>%
  mutate(Last=ifelse(grepl("Joshua King", Player), "Joshua King", Last)) %>%
  mutate(Last=ifelse(grepl("Andy King", Player), "Andy King", Last)) %>%
  mutate(Last=ifelse(grepl("Arouna Kone", Player), "Arouna Kone", Last)) %>%
  mutate(Last=ifelse(grepl("Lamine Kone", Player), "Lamine Kone", Last)) %>%
  mutate(Last=ifelse(grepl("Shane Long", Player), "Shane Long", Last)) %>%
  mutate(Last=ifelse(grepl("Kevin Long", Player), "Kevin Long", Last)) %>%
  mutate(Last=ifelse(grepl("Ryan Mason", Player), "Ryan Mason", Last)) %>%
  mutate(Last=ifelse(grepl("Brandon Mason", Player), "Brandon Mason", Last)) %>%
  mutate(Last=ifelse(grepl("Ndong", Player), "Ndong", Last)) %>%
  mutate(Last=ifelse(grepl("Jonas Olsson", Player), "Jonas Olsson", Last)) %>%
  mutate(Last=ifelse(grepl("Martin Olsson", Player), "Martin Olsson", Last)) %>%
  mutate(Last=ifelse(grepl("Mamadou Sakho", Player), "Mamadou Sakho", Last)) %>%
  mutate(Last=ifelse(grepl("Diafra Sakho", Player), "Diafra Sakho", Last)) %>%
  mutate(Last=ifelse(grepl("Adam Smith", Player), "Adam Smith", Last)) %>%
  mutate(Last=ifelse(grepl("Brad Smith", Player), "Brad Smith", Last)) %>%
  mutate(Last=ifelse(grepl("Heung-min", Player), "Son", Last)) %>%
  mutate(Last=ifelse(grepl("Steve Cook", Player), "Steve Cook", Last)) %>%
  mutate(Last=ifelse(grepl("Antonio Valencia", Player), "Antonio Valencia", Last)) %>%
  mutate(Last=ifelse(grepl("Enner Valencia", Player), "Enner Valencia", Last)) %>%
  mutate(Last=ifelse(grepl("Stephen Ward", Player), "Stephen Ward", Last)) %>%
  mutate(Last=ifelse(grepl("Joel Ward", Player), "Joel Ward", Last)) %>%
  mutate(Last=ifelse(grepl("Yaya Toure", Player), "Yaya Toure", Last))
gc()

df2 <- df2 %>% rename(Player=1) %>%
  select(Player,Team,Pos,CS,Points,Bonus) %>%
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
  mutate(Points=ifelse(grepl("Arbeloa", Player),0,Points)) %>%
  mutate(Pos.x=ifelse(grepl("Arbeloa", Player),"DEF",Pos.x)) %>%
  mutate(Bonus=ifelse(grepl("Arbeloa", Player),0,Bonus)) %>%
  mutate(CS=ifelse(grepl("Arbeloa", Player),0,CS)) %>%
  mutate(Points=ifelse(grepl("Eleftheriou", Player),0,Points)) %>%
  mutate(Pos.x=ifelse(grepl("Eleftheriou", Player),"DEF",Pos.x)) %>%
  mutate(Bonus=ifelse(grepl("Eleftheriou", Player),0,Bonus)) %>%
  mutate(CS=ifelse(grepl("Eleftheriou", Player),0,CS)) %>%
  mutate(Points=ifelse(grepl("Husband", Player),0,Points)) %>%
  mutate(Pos.x=ifelse(grepl("Husband", Player),"MID",Pos.x)) %>%
  mutate(Bonus=ifelse(grepl("Husband", Player),0,Bonus)) %>%
  mutate(CS=ifelse(grepl("Husband", Player),0,CS)) %>%
  mutate(Points=ifelse(grepl("Marc Wilson", Player),0,Points)) %>%
  mutate(Pos.x=ifelse(grepl("Marc Wilson", Player),"DEF",Pos.x)) %>%
  mutate(Bonus=ifelse(grepl("Marc Wilson", Player),0,Bonus)) %>%
  mutate(CS=ifelse(grepl("Marc Wilson", Player),0,CS)) %>%
  left_join(temp, by=c("Player")) %>%
  select(Player, Nation, Age, Born, Pos.x, Squad, Season, Points, Bonus,
         `MP_Playing Time`, `Starts_Playing Time`, `Min_Playing Time`,
         Gls, Ast, `G+A`, G_minus_PK, PK, PKatt, CrdY, CrdR,
         `Mins_Per_90_Playing Time`, `Gls_Per 90 Minutes`, `Ast_Per 90 Minutes`,
         `G+A_Per 90 Minutes`, `G_minus_PK_Per 90 Minutes`, CS) %>%
  rename(Pos=Pos.x)

maindir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(maindir)), dir.create(file.path(maindir)), FALSE)
write.csv(df4, file="outputs/std_1617.csv", row.names = F)