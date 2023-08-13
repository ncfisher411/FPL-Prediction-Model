## Getting standard player data for 2015/16 season

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

df2 <- read.csv("~/FPL/submodules/positions/positions_1516.csv")
## https://jaseziv.github.io/worldfootballR/articles/extract-fbref-data.html#overview
### Documentation of worldfoortballR above

#Player stats
df <- fb_league_stats(country="ENG", gender = "M", season_end_year = 2016,
                      tier="1st", stat_type = "standard",
                      team_or_player = "player") %>%
  mutate(Season=2016) %>%
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
  mutate(Last=ifelse(grepl("Andre Ayew", Player), "Andre Ayew", Last)) %>%
  mutate(Last=ifelse(grepl("Jordan Ayew", Player), "Jordan Ayew", Last)) %>%
  mutate(Last=ifelse(grepl("Baba Rahman", Player), "Baba Rahman", Last)) %>%
  mutate(Last=ifelse(grepl("Fraizer Campbell", Player), "Fraizer Campbell", Last)) %>%
  mutate(Last=ifelse(grepl("Joel Campbell", Player), "Joel Campbell", Last)) %>%
  mutate(Last=ifelse(grepl("Carles Gil", Player), "Carles Gil", Last)) %>%
  mutate(Last=ifelse(grepl("Tom Carroll", Player), "Tom Carroll", Last)) %>%
  mutate(Last=ifelse(grepl("Andy Carroll", Player), "Andy Carroll", Last)) %>%
  mutate(Last=ifelse(grepl("Oxlade-Chamberlain", Player), "Chamberlain", Last)) %>%
  mutate(Last=ifelse(grepl("Crespo", Player), "Crespo", Last)) %>%
  mutate(Last=ifelse(grepl("Kelvin Davis", Player), "Davis K", Last)) %>%
  mutate(Last=ifelse(grepl("Diouf", Player), "Diouf", Last)) %>%
  mutate(Last=ifelse(grepl("Gueye", Player), "Gueye", Last)) %>%
  mutate(Last=ifelse(grepl("Jordi Gomez", Player), "Jordi Gomez", Last)) %>%
  mutate(Last=ifelse(grepl("Glen Johnson", Player), "Glen Johnson", Last)) %>%
  mutate(Last=ifelse(grepl("Bradley Johnson", Player), "Bradley Johnson", Last)) %>%
  mutate(Last=ifelse(grepl("Adam Johnson", Player), "Adam Johnson", Last)) %>%
  mutate(Last=ifelse(grepl("Phil Jones", Player), "Phil Jones", Last)) %>%
  mutate(Last=ifelse(grepl("Jurado", Player), "Jurado", Last)) %>%
  mutate(Last=ifelse(grepl("Glen Johnson", Player), "Glen Johnson", Last)) %>%
  mutate(Last=ifelse(grepl("Sung-yueng", Player), "Ki Sung-yueng", Last)) %>%
  mutate(Last=ifelse(grepl("Joshua King", Player), "Joshua King", Last)) %>%
  mutate(Last=ifelse(grepl("Andy King", Player), "Andy King", Last)) %>%
  mutate(Last=ifelse(grepl("Lamine Kone", Player), "Lamine Kone", Last)) %>%
  mutate(Last=ifelse(grepl("Arouna Kone", Player), "Arouna Kone", Last)) %>%
  mutate(Last=ifelse(grepl("Chung-yong", Player), "Lee Chung-yong", Last)) %>%
  mutate(Last=ifelse(grepl("Glen Johnson", Player), "Glen Johnson", Last)) %>%
  mutate(Last=ifelse(grepl("Alex McCarthy", Player), "Alex McCarthy", Last)) %>%
  mutate(Last=ifelse(grepl("James McCarthy", Player), "James McCarthy", Last)) %>%
  mutate(Last=ifelse(grepl("Obi Mikel", Player), "Mikel", Last)) %>%
  mutate(Last=ifelse(grepl("Martin Olsson", Player), "Martin Olsson", Last)) %>%
  mutate(Last=ifelse(grepl("Jonas Olsson", Player), "Jonas Olsson", Last)) %>%
  mutate(Last=ifelse(grepl("Paredes", Player), "Paredes", Last)) %>%
  mutate(Last=ifelse(grepl("Ryan Bennett", Player), "Ryan Bennett", Last)) %>%
  mutate(Last=ifelse(grepl("Heung-min", Player), "Son", Last)) %>%
  mutate(Last=ifelse(grepl("Steven Taylor", Player), "Steven Taylor", Last)) %>%
  mutate(Last=ifelse(grepl("Alexis Sanchez", Player), "Alexis Sanchez", Last)) %>%
  mutate(Last=ifelse(grepl("Carlos Sanchez", Player), "Carlos Sanchez", Last)) %>%
  mutate(Last=ifelse(grepl("Dionatan Teixeira", Player), "Dionatan Teixeira", Last)) %>%
  mutate(Last=ifelse(grepl("Joao Carlos Teixeira", Player), "Joao Carlos Teixeira", Last)) %>%
  mutate(Last=ifelse(grepl("Danny Ward", Player), "Danny Ward", Last)) %>%
  mutate(Last=ifelse(grepl("Joel Ward", Player), "Joel Ward", Last)) %>%
  mutate(Last=ifelse(grepl("Ashley Williams", Player), "Ashley Williams", Last)) %>%
  mutate(Last=ifelse(grepl("Jonny Williams", Player), "Jonny Williams", Last)) %>%
  mutate(Last=ifelse(grepl("Marc Wilson", Player), "Marc Wilson", Last)) %>%
  mutate(Last=ifelse(grepl("Callum Wilson", Player), "Callum Wilson", Last)) %>%
  mutate(Last=ifelse(grepl("James Wilson", Player), "James Wilson", Last)) %>%
  mutate(Last=ifelse(grepl("Yaya Toure", Player), "Yaya Toure", Last)) %>%
  mutate(Last=ifelse(grepl("Kolo Toure", Player), "Kolo Toure", Last))
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
  left_join(temp, by=c("Player")) %>%
  mutate(Points=ifelse(grepl("Gamboa", Player),0,Points)) %>%
  mutate(Pos.x=ifelse(grepl("Gamboa", Player),"DEF",Pos.x)) %>%
  mutate(Bonus=ifelse(grepl("Gamboa", Player),0,Bonus)) %>%
  mutate(CS=ifelse(grepl("Gamboa", Player),0,CS)) %>%
  select(Player, Nation, Age, Born, Pos.x, Squad, Season, Points, Bonus,
         `MP_Playing Time`, `Starts_Playing Time`, `Min_Playing Time`,
         Gls, Ast, `G+A`, G_minus_PK, PK, PKatt, CrdY, CrdR,
         `Mins_Per_90_Playing Time`, `Gls_Per 90 Minutes`, `Ast_Per 90 Minutes`,
         `G+A_Per 90 Minutes`, `G_minus_PK_Per 90 Minutes`, CS) %>%
  rename(Pos=Pos.x)

maindir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(maindir)), dir.create(file.path(maindir)), FALSE)
write.csv(df4, file="outputs/std_1516.csv", row.names = F)
