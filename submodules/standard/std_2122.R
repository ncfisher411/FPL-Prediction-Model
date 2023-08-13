## Getting standard player data for 2021/22 season

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

df2 <- read.csv("~/FPL/submodules/positions/positions_2122.csv") %>%
  rename(Pos=Position, Player=1) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))
## https://jaseziv.github.io/worldfootballR/articles/extract-fbref-data.html#overview
### Documentation of worldfoortballR above

#Player stats
df <- fb_league_stats(country="ENG", gender = "M", season_end_year = 2022,
                      tier="1st", stat_type = "standard",
                      team_or_player = "player") %>%
  mutate(Season=2022) %>%
  select(-c("Rk", "Matches", "url")) %>%
  select(Player, Squad, Season, everything()) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))
gc()
df[c("First", "Last")] <- str_split_fixed(df$Player, " ", 2)

df3 <- df %>%
  mutate(Last=ifelse(grepl("Andre Gomes", Player), "Andre Gomes", Last)) %>%
  mutate(Last=ifelse(grepl("Ashley Barnes", Player), "Ashley Barnes", Last)) %>%
  mutate(Last=ifelse(grepl("Harvey Barnes", Player), "Harvey Barnes", Last)) %>%
  mutate(Last=ifelse(grepl("Bernardo Silva", Player), "Bernardo Silva", Last)) %>%
  mutate(Last=ifelse(grepl("Ashley Barnes", Player), "Ashley Barnes", Last)) %>%
  mutate(Last=ifelse(grepl("Charlie Cresswell", Player), "Charlie Cresswell", Last)) %>%
  mutate(Last=ifelse(grepl("Aaron Cresswell", Player), "Aaron Cresswell", Last)) %>%
  mutate(Last=ifelse(grepl("Tom Davies", Player), "Tom Davies", Last)) %>%
  mutate(Last=ifelse(grepl("Ben Davies", Player), "Ben Davies", Last)) %>%
  mutate(Last=ifelse(grepl("Douglas Luiz", Player), "Douglas Luiz", Last)) %>%
  mutate(Last=ifelse(grepl("Emerson", Player), "Emerson Royal", Last)) %>%
  mutate(Last=ifelse(grepl("Palmieri", Player), "Emerson", Last)) %>%
  mutate(Last=ifelse(grepl("Fabio Silva", Player), "Fabio Silva", Last)) %>%
  mutate(Last=ifelse(grepl("Evan Ferguson", Player), "Evan Ferguson", Last)) %>%
  mutate(Last=ifelse(grepl("Nathan Ferguson", Player), "Nathan Ferguson", Last)) %>%
  mutate(Last=ifelse(grepl("Alvaro Fernandez", Player), "Alvaro Fernandez", Last)) %>%
  mutate(Last=ifelse(grepl("Federico Fernandez", Player), "Federico Fernandez", Last)) %>%
  mutate(Last=ifelse(grepl("Dos Santos", Player), "Gabriel", Last)) %>%
  mutate(Last=ifelse(grepl("Hjelde", Player), "Hjelde", Last)) %>%
  mutate(Last=ifelse(grepl("Hwang", Player), "Hwang", Last)) %>%
  mutate(Last=ifelse(grepl("Anthony Gordon", Player), "Anthony Gordon", Last)) %>%
  mutate(Last=ifelse(grepl("Kaide Gordon", Player), "Kaide Gordon", Last)) %>%
  mutate(Last=ifelse(grepl("Sam Greenwood", Player), "Sam Greenwood", Last)) %>%
  mutate(Last=ifelse(grepl("Mason Greenwood", Player), "Mason Greenwood", Last)) %>%
  mutate(Last=ifelse(grepl("Sam Greenwood", Player), "Sam Greenwood", Last)) %>%
  mutate(Last=ifelse(grepl("Gudmundsson", Player), "Gudmundsson", Last)) %>%
  mutate(Last=ifelse(grepl("Reece James", Player), "Reece James", Last)) %>%
  mutate(Last=ifelse(grepl("Daniel James", Player), "Daniel James", Last)) %>%
  mutate(Last=ifelse(grepl("Curtis Jones", Player), "Curtis Jones", Last)) %>%
  mutate(Last=ifelse(grepl("Phil Jones", Player), "Phil Jones", Last)) %>%
  mutate(Last=ifelse(grepl("Jonny Castro", Player), "Jonny", Last)) %>%
  mutate(Last=ifelse(grepl("Joao Pedro", Player), "Joao Pedro", Last)) %>%
  mutate(Last=ifelse(grepl("Kayky", Player), "Kayky", Last)) %>%
  mutate(Last=ifelse(grepl("Kevin Long", Player), "Kevin Long", Last)) %>%
  mutate(Last=ifelse(grepl("Shane Long", Player), "Shane Long", Last)) %>%
  mutate(Last=ifelse(grepl("Lucas Moura", Player), "Lucas Moura", Last)) %>%
  mutate(Last=ifelse(grepl("Luis Diaz", Player), "Luis Diaz", Last)) %>%
  mutate(Last=ifelse(grepl("Shane Long", Player), "Shane Long", Last)) %>%
  mutate(Last=ifelse(grepl("Mcatee", Player), "McAtee", Last)) %>%
  mutate(Last=ifelse(grepl("Philogene", Player), "Philogene-Bidace", Last)) %>%
  mutate(Last=ifelse(grepl("Sakyi", Player), "Rak-Sakyi", Last)) %>%
  mutate(Last=ifelse(grepl("Samir", Player), "Samir", Last)) %>%
  mutate(Last=ifelse(grepl("Edouard Mendy", Player), "Edouard Mendy", Last)) %>%
  mutate(Last=ifelse(grepl("Nampalys Mendy", Player), "Nampalys Mendy", Last)) %>%
  mutate(Last=ifelse(grepl("Benjamin Mendy", Player), "Benjamin Mendy", Last)) %>%
  mutate(Last=ifelse(grepl("Neco Williams", Player), "Neco Williams", Last)) %>%
  mutate(Last=ifelse(grepl("Connor Roberts", Player), "Connor Roberts", Last)) %>%
  mutate(Last=ifelse(grepl("Tyler Roberts", Player), "Tyler Roberts", Last)) %>%
  mutate(Last=ifelse(grepl("Adam Armstrong", Player), "Adam Armstrong", Last)) %>%
  mutate(Last=ifelse(grepl("Stuart Armstrong", Player), "Stuart Armstrong", Last)) %>%
  mutate(Last=ifelse(grepl("Malang Sarr", Player), "Malang Sarr", Last)) %>%
  mutate(Last=ifelse(grepl("Ismaila Sarr", Player), "Ismalia Sarr", Last)) %>%
  mutate(Last=ifelse(grepl("Saul", Player), "Saul", Last)) %>%
  mutate(Last=ifelse(grepl("Toti", Player), "Toti", Last)) %>%
  mutate(Last=ifelse(grepl("Wesley Moraes", Player), "Wesley", Last)) %>%
  mutate(Last=ifelse(grepl("Jorgensen", Player), "Zanka", Last)) %>%
  mutate(Last=ifelse(grepl("Heung-min", Player), "Son", Last)) %>%
  mutate(Last=ifelse(grepl("Dale Stephens", Player), "Dale Stephens", Last)) %>%
  mutate(Last=ifelse(grepl("Jack Stephens", Player), "Jack Stephens", Last)) %>%
  mutate(Last=ifelse(grepl("Robert Sanchez", Player), "Robert Sanchez", Last)) %>%
  mutate(Last=ifelse(grepl("Davinson Sanchez", Player), "Davinson Sanchez", Last)) %>%
  mutate(Last=ifelse(grepl("Mads Bech Sorensen", Player), "Mads Bech Sorensen", Last)) %>%
  mutate(Last=ifelse(grepl("Jakob Sorensen", Player), "Jakob Sorensen", Last)) %>%
  mutate(Last=ifelse(grepl("Mads Bech Sorensen", Player), "Mads Bech Sorensen", Last)) %>%
  mutate(Last=ifelse(grepl("Alcantara", Player), "Thiago", Last)) %>%
  mutate(Last=ifelse(grepl("Thiago Silva", Player), "Thiago Silva", Last)) %>%
  mutate(Last=ifelse(grepl("Bertrand Traore", Player), "Bertrand Traore", Last)) %>%
  mutate(Last=ifelse(grepl("Adama Traore", Player), "Adama Traore", Last)) %>%
  mutate(Last=ifelse(grepl("Joel Ward", Player), "Joel Ward", Last)) %>%
  mutate(Last=ifelse(grepl("Danny Ward", Player), "Danny Ward", Last)) %>%
  mutate(Last=ifelse(grepl("Callum Wilson", Player), "Callum Wilson", Last)) %>%
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
  rename(Pos=Pos.x) %>%
  mutate(Points=ifelse(grepl("Bali Mumba", Player),0,Points)) %>%
  mutate(Pos=ifelse(grepl("Bali Mumba", Player),"DEF",Pos)) %>%
  mutate(CS=ifelse(grepl("Bali Mumba", Player),0,CS)) %>%
  mutate(Bonus=ifelse(grepl("Bali Mumba", Player),0,Bonus)) %>%
  mutate(Points=ifelse(grepl("Zimmermann", Player),0,Points)) %>%
  mutate(Pos=ifelse(grepl("Zimmermann", Player),"DEF",Pos)) %>%
  mutate(CS=ifelse(grepl("Zimmermann", Player),0,CS)) %>%
  mutate(Bonus=ifelse(grepl("Zimmermann", Player),0,Bonus)) %>%
  mutate(Points=ifelse(grepl("Cody Drameh", Player),0,Points)) %>%
  mutate(Pos=ifelse(grepl("Cody Drameh", Player),"DEF",Pos)) %>%
  mutate(CS=ifelse(grepl("Cody Drameh", Player),0,CS)) %>%
  mutate(Bonus=ifelse(grepl("Cody Drameh", Player),0,Bonus)) %>%
  mutate(Points=ifelse(grepl("Kolasinac", Player),0,Points)) %>%
  mutate(Pos=ifelse(grepl("Kolasinac", Player),"DEF",Pos)) %>%
  mutate(CS=ifelse(grepl("Kolasinac", Player),0,CS)) %>%
  mutate(Bonus=ifelse(grepl("Kolasinac", Player),0,Bonus))

maindir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(maindir)), dir.create(file.path(maindir)), FALSE)
write.csv(df4, file="outputs/std_2122.csv", row.names = F)