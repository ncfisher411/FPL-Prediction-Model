## Getting standard player data for 2022/23 season

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

df2 <- read.csv("~/FPL/submodules/positions/positions_2223.csv") %>%
  rename(Pos=Position, Player=1) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))
## https://jaseziv.github.io/worldfootballR/articles/extract-fbref-data.html#overview
### Documentation of worldfoortballR above

#Player stats
df <- fb_league_stats(country="ENG", gender = "M", season_end_year = 2023,
                      tier="1st", stat_type = "standard",
                      team_or_player = "player") %>%
  mutate(Season=2023) %>%
  select(-c("Rk", "Matches", "url")) %>%
  select(Player, Squad, Season, everything()) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))
gc()
df[c("First", "Last")] <- str_split_fixed(df$Player, " ", 2)

df3 <- df %>%
  mutate(Last=ifelse(grepl("Adam Armstrong", Player), "Adam Armstrong", Last)) %>%
  mutate(Last=ifelse(grepl("Stuart Armstrong", Player), "Stuart Armstrong", Last)) %>%
  mutate(Last=ifelse(grepl("Andre Ayew", Player), "Andre Ayew", Last)) %>%
  mutate(Last=ifelse(grepl("Abdoulaye", Player), "Abdoulaye Doucoure", Last)) %>%
  mutate(Last=ifelse(grepl("Alex Moreno", Player), "Alex Moreno", Last)) %>%
  mutate(Last=ifelse(grepl("Adama Traore", Player), "Adama Traore", Last)) %>%
  mutate(Last=ifelse(grepl("Andreas Pereira", Player), "Andreas Pereira", Last)) %>%
  mutate(Last=ifelse(grepl("Bech", Player), "Bech", Last)) %>%
  mutate(Last=ifelse(grepl("Ben Davies", Player), "Ben Davies", Last)) %>%
  mutate(Last=ifelse(grepl("Ben Johnson", Player), "Ben Johnson", Last)) %>%
  mutate(Last=ifelse(grepl("Ben White", Player), "Ben White", Last)) %>%
  mutate(Last=ifelse(grepl("Bernardo", Player), "Bernardo", Last)) %>%
  mutate(Last=ifelse(grepl("Bertrand Traore", Player), "Bertrand Traore", Last)) %>%
  mutate(Last=ifelse(grepl("Brennan Johnson", Player), "Brennan Johnson", Last)) %>%
  mutate(Last=ifelse(grepl("Callum Wilson", Player), "Callum Wilson", Last)) %>%
  mutate(Last=ifelse(grepl("Cheick Doucoure", Player), "Cheick Doucoure", Last)) %>%
  mutate(Last=ifelse(grepl("Daniel James", Player), "Daniel James", Last)) %>%
  mutate(Last=ifelse(grepl("Danny Ward", Player), "Danny Ward", Last)) %>%
  mutate(Last=ifelse(grepl("Darwin", Player), "Darwin", Last)) %>%
  mutate(Last=ifelse(grepl("Datro Fofana", Player), "Datro Fofana", Last)) %>%
  mutate(Last=ifelse(grepl("Davinson Sanchez", Player), "Davinson Sanchez", Last)) %>%
  mutate(Last=ifelse(grepl("Dean Henderson", Player), "Dean Henderson", Last)) %>%
  mutate(Last=ifelse(grepl("Dele", Player), "Dele", Last)) %>%
  mutate(Last=ifelse(grepl("Diego Carlos", Player), "Diego Carlos", Last)) %>%
  mutate(Last=ifelse(grepl("Diego Costa", Player), "Diego Costa", Last)) %>%
  mutate(Last=ifelse(grepl("Edouard Mendy", Player), "Edouard Mendy", Last)) %>%
  mutate(Last=ifelse(grepl("Emiliano Martinez", Player), "Emiliano Martinez", Last)) %>%
  mutate(Last=ifelse(grepl("Enciso", Player), "Enciso", Last)) %>%
  mutate(Last=ifelse(grepl("Enzo", Player), "Enzo", Last)) %>%
  mutate(Last=ifelse(grepl("Dos Santos", Player), "Gabriel", Last)) %>%
  mutate(Last=ifelse(grepl("Gueye", Player), "Gueye", Last)) %>%
  mutate(Last=ifelse(grepl("Junior Traore", Player), "Hamed Junior Traore", Last)) %>%
  mutate(Last=ifelse(grepl("Harry Wilson", Player), "Harry Wilson", Last)) %>%
  mutate(Last=ifelse(grepl("Harvey White", Player), "Harvey White", Last)) %>%
  mutate(Last=ifelse(grepl("Hwang", Player), "Hwang", Last)) %>%
  mutate(Last=ifelse(grepl("Jamal Lewis", Player), "Jamal Lewis", Last)) %>%
  mutate(Last=ifelse(grepl("Joao Felix", Player), "Joao Felix", Last)) %>%
  mutate(Last=ifelse(grepl("Joao Gomes", Player), "Joao Gomes", Last)) %>%
  mutate(Last=ifelse(grepl("Joe Gomez", Player), "Joe Gomez", Last)) %>%
  mutate(Last=ifelse(grepl("Joel Ward", Player), "Joel Ward", Last)) %>%
  mutate(Last=ifelse(grepl("Jonny Castro", Player), "Jonny", Last)) %>%
  mutate(Last=ifelse(grepl("Jordan Ayew", Player), "Jordan Ayew", Last)) %>%
  mutate(Last=ifelse(grepl("Jordan Henderson", Player), "Jordan Henderson", Last)) %>%
  mutate(Last=ifelse(grepl("Kalvin Phillips", Player), "Kalvin Phillips", Last)) %>%
  mutate(Last=ifelse(grepl("Kamaldeen", Player), "Kamaldeen", Last)) %>%
  mutate(Last=ifelse(grepl("Kenny Tete", Player), "Kenny Tete", Last)) %>%
  mutate(Last=ifelse(grepl("Kristiansen", Player), "Kristiansen", Last)) %>%
  mutate(Last=ifelse(grepl("Lewis Cook", Player), "Lewis Cook", Last)) %>%
  mutate(Last=ifelse(grepl("Lisandro Martinez", Player), "Lisandro Martinez", Last)) %>%
  mutate(Last=ifelse(grepl("Lucas Moura", Player), "Lucas Moura", Last)) %>%
  mutate(Last=ifelse(grepl("Luis Diaz", Player), "Luis Diaz", Last)) %>%
  mutate(Last=ifelse(grepl("Mateo Joseph", Player), "Mateo Joseph", Last)) %>%
  mutate(Last=ifelse(grepl("Matheus", Player), "Matheus", Last)) %>%
  mutate(Last=ifelse(grepl("Nampalys Mendy", Player), "Nampalys Mendy", Last)) %>%
  mutate(Last=ifelse(grepl("Nathaniel Chalobah", Player), "Nathaniel Chalobah", Last)) %>%
  mutate(Last=ifelse(grepl("Nathaniel Phillips", Player), "Nathaniel Phillips", Last)) %>%
  mutate(Last=ifelse(grepl("Neco Williams", Player), "Neco Williams", Last)) %>%
  mutate(Last=ifelse(grepl("Pedro Neto", Player), "Pedro Neto", Last)) %>%
  mutate(Last=ifelse(grepl("Porro", Player), "Pedro Porro", Last)) %>%
  mutate(Last=ifelse(grepl("Reece James", Player), "Reece James", Last)) %>%
  mutate(Last=ifelse(grepl("Renan Lodi", Player), "Renan Lodi", Last)) %>%
  mutate(Last=ifelse(grepl("Ricardo", Player), "Ricardo", Last)) %>%
  mutate(Last=ifelse(grepl("Rico Lewis", Player), "Rico Lewis", Last)) %>%
  mutate(Last=ifelse(grepl("Robert Sanchez", Player), "Robert Sanchez", Last)) %>%
  mutate(Last=ifelse(grepl("Sambi", Player), "Sambi", Last)) %>%
  mutate(Last=ifelse(grepl("Sarr", Player), "Sarr", Last)) %>%
  mutate(Last=ifelse(grepl("Sergio Gomez", Player), "Sergio Gomez", Last)) %>%
  mutate(Last=ifelse(grepl("Heung-min", Player), "Son", Last)) %>%
  mutate(Last=ifelse(grepl("Steve Cook", Player), "Steve Cook", Last)) %>%
  mutate(Last=ifelse(grepl("Alcantara", Player), "Thiago", Last)) %>%
  mutate(Last=ifelse(grepl("Thiago Silva", Player), "Thiago Silva", Last)) %>%
  mutate(Last=ifelse(grepl("Tom Davies", Player), "Tom Davies", Last)) %>%
  mutate(Last=ifelse(grepl("Tosin", Player), "Tosin", Last)) %>%
  mutate(Last=ifelse(grepl("Toti", Player), "Toti", Last)) %>%
  mutate(Last=ifelse(grepl("Trevoh Chalobah", Player), "Trevoh Chalobah", Last)) %>%
  mutate(Last=ifelse(grepl("Wesley Fofana", Player), "Wesley Fofana", Last)) %>%
  mutate(Last=ifelse(grepl("Jorgensen", Player), "Zanka", Last)) %>%
  mutate(Last=ifelse(grepl("Hecke", Player), "van Hecke", Last)) %>%
  mutate(Last=ifelse(grepl("Anthony Gordon", Player), "Anthony Gordon", Last)) %>%
  mutate(Last=ifelse(grepl("Cunha", Player), "Cunha", Last)) %>%
  mutate(Last=ifelse(grepl("Boubacar Traore", Player), "Boubacar Traore", Last)) %>%
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
  ungroup()
gc()

df2 <- df2 %>% rename(Player=1) %>%
  select(Player,Team,Pos,CS,Points,Bonus) %>%
  filter(Points != 0) %>%
  group_by(Player, Team, Pos) %>%
  summarize(Points=sum(Points),
            Bonus=sum(Bonus),
            CS=sum(CS))
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
  rename(Pos=Pos.x) %>%
  mutate(Points=ifelse(grepl("Isaac Price", Player),0,Points)) %>%
  mutate(Pos=ifelse(grepl("Isaac Price", Player),"MID",Pos)) %>%
  mutate(CS=ifelse(grepl("Isaac Price", Player),0,CS)) %>%
  mutate(Bonus=ifelse(grepl("Isaac Price", Player),0,Bonus)) %>%
  mutate(Points=ifelse(grepl("Yan Valery", Player),0,Points)) %>%
  mutate(Pos=ifelse(grepl("Yan Valery", Player),"DEF",Pos)) %>%
  mutate(CS=ifelse(grepl("Yan Valery", Player),0,CS)) %>%
  mutate(Bonus=ifelse(grepl("Yan Valery", Player),0,Bonus))

maindir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(maindir)), dir.create(file.path(maindir)), FALSE)
write.csv(df4, file="outputs/std_2223.csv", row.names = F)