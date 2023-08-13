## Getting standard player data for 2020/21 season

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

df2 <- read.csv("~/FPL/submodules/positions/positions_2021.csv") %>%
  rename(Pos=Position, Player=1) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))
gc()
## https://jaseziv.github.io/worldfootballR/articles/extract-fbref-data.html#overview
### Documentation of worldfoortballR above

#Player stats
df <- fb_league_stats(country="ENG", gender = "M", season_end_year = 2021,
                      tier="1st", stat_type = "standard",
                      team_or_player = "player") %>%
  mutate(Season=2021) %>%
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
  mutate(Last=ifelse(grepl("Andre Gomes", Player), "Andre Gomes", Last)) %>%
  mutate(Last=ifelse(grepl("Anguissa", Player), "Anguissa", Last)) %>%
  mutate(Last=ifelse(grepl("Ashley Barnes", Player), "Ashley Barnes", Last)) %>%
  mutate(Last=ifelse(grepl("Harvey Barnes", Player), "Harvey Barnes", Last)) %>%
  mutate(Last=ifelse(grepl("Bernardo Silva", Player), "Bernardo Silva", Last)) %>%
  mutate(Last=ifelse(grepl("Joe Bryan", Player), "Joe Bryan", Last)) %>%
  mutate(Last=ifelse(grepl("Kean Bryan", Player), "Kean Bryan", Last)) %>%
  mutate(Last=ifelse(grepl("Callum Robinson", Player), "Callum Robinson", Last)) %>%
  mutate(Last=ifelse(grepl("Carlos Vinicius", Player), "Carlos Vinicius", Last)) %>%
  mutate(Last=ifelse(grepl("Chamberlain", Player), "Chamberlain", Last)) %>%
  mutate(Last=ifelse(grepl("Cedric Soares", Player), "Cedric", Last)) %>%
  mutate(Last=ifelse(grepl("David Luiz", Player), "David Luiz", Last)) %>%
  mutate(Last=ifelse(grepl("Tom Davies", Player), "Tom Davies", Last)) %>%
  mutate(Last=ifelse(grepl("Ben Davies", Player), "Ben Davies", Last)) %>%
  mutate(Last=ifelse(grepl("Keinan Davis", Player), "Keinan Davis", Last)) %>%
  mutate(Last=ifelse(grepl("Leif Davis", Player), "Leif Davis", Last)) %>%
  mutate(Last=ifelse(grepl("Bobby Reid", Player), "Decordova-Reid", Last)) %>%
  mutate(Last=ifelse(grepl("Douglas Luiz", Player), "Douglas Luiz", Last)) %>%
  mutate(Last=ifelse(grepl("Palmieri", Player), "Emerson", Last)) %>%
  mutate(Last=ifelse(grepl("Fabio Silva", Player), "Fabio Silva", Last)) %>%
  mutate(Last=ifelse(grepl("Felipe Anderson", Player), "Felipe Anderson", Last)) %>%
  mutate(Last=ifelse(grepl("Fish", Player), "Fish", Last)) %>%
  mutate(Last=ifelse(grepl("Gabriel Dos", Player), "Gabriel", Last)) %>%
  mutate(Last=ifelse(grepl("Gudmundsson", Player), "Gudmundsson", Last)) %>%
  mutate(Last=ifelse(grepl("Jordan Henderson", Player), "Jordan Henderson", Last)) %>%
  mutate(Last=ifelse(grepl("Dean Henderson", Player), "Dean Henderson", Last)) %>%
  mutate(Last=ifelse(grepl("Jack Robinson", Player), "Jack Robinson", Last)) %>%
  mutate(Last=ifelse(grepl("Reece James", Player), "Reece James", Last)) %>%
  mutate(Last=ifelse(grepl("Daniel James", Player), "Daniel James", Last)) %>%
  mutate(Last=ifelse(grepl("Kevin Long", Player), "Kevin Long", Last)) %>%
  mutate(Last=ifelse(grepl("Shane Long", Player), "Shane Long", Last)) %>%
  mutate(Last=ifelse(grepl("Lucas Moura", Player), "Lucas Moura", Last)) %>%
  mutate(Last=ifelse(grepl("Matthew Longstaff", Player), "Matthew Longstaff", Last)) %>%
  mutate(Last=ifelse(grepl("James McCarthy", Player), "James McCarthy", Last)) %>%
  mutate(Last=ifelse(grepl("Alex McCarthy", Player), "Alex McCarthy", Last)) %>%
  mutate(Last=ifelse(grepl("Edouard Mendy", Player), "Edouard Mendy", Last)) %>%
  mutate(Last=ifelse(grepl("Nampalys Mendy", Player), "Nampalys Mendy", Last)) %>%
  mutate(Last=ifelse(grepl("Benjamin Mendy", Player), "Benjamin Mendy", Last)) %>%
  mutate(Last=ifelse(grepl("Neco Williams", Player), "Neco Williams", Last)) %>%
  mutate(Last=ifelse(grepl("Ricardo Pereira", Player), "Ricardo Pereira", Last)) %>%
  mutate(Last=ifelse(grepl("Matheus Pereira", Player), "Matheus Pereira", Last)) %>%
  mutate(Last=ifelse(grepl("Kalvin Phillips", Player), "Kalvin Phillips", Last)) %>%
  mutate(Last=ifelse(grepl("Nathaniel Phillips", Player), "Nathaniel Phillips", Last)) %>%
  mutate(Last=ifelse(grepl("Matt Phillips", Player), "Matt Phillips", Last)) %>%
  mutate(Last=ifelse(grepl("Philogene", Player), "Philogene-Bidace", Last)) %>%
  mutate(Last=ifelse(grepl("Poveda", Player), "Poveda-Ocampo", Last)) %>%
  mutate(Last=ifelse(grepl("Kayne Ramsey", Player), "Kalvin Phillips", Last)) %>%
  mutate(Last=ifelse(grepl("Rhys", Player), "Rhys Williams", Last)) %>%
  mutate(Last=ifelse(grepl("Jay Rodriguez", Player), "Jay Rodriguez", Last)) %>%
  mutate(Last=ifelse(grepl("James Rodriguez", Player), "James Rodriguez", Last)) %>%
  mutate(Last=ifelse(grepl("Runarsson", Player), "Runarsson", Last)) %>%
  mutate(Last=ifelse(grepl("Sean Longstaff", Player), "Sean Longstaff", Last)) %>%
  mutate(Last=ifelse(grepl("Heung-min", Player), "Son", Last)) %>%
  mutate(Last=ifelse(grepl("Dale Stephens", Player), "Dale Stephens", Last)) %>%
  mutate(Last=ifelse(grepl("Jack Stephens", Player), "Jack Stephens", Last)) %>%
  mutate(Last=ifelse(grepl("Robert Sanchez", Player), "Robert Sanchez", Last)) %>%
  mutate(Last=ifelse(grepl("Davinson Sanchez", Player), "Davinson Sanchez", Last)) %>%
  mutate(Last=ifelse(grepl("Neil Taylor", Player), "Neil Taylor", Last)) %>%
  mutate(Last=ifelse(grepl("Charlie Taylor", Player), "Charlie Taylor", Last)) %>%
  mutate(Last=ifelse(grepl("Alcantara", Player), "Thiago", Last)) %>%
  mutate(Last=ifelse(grepl("Thiago Silva", Player), "Thiago Silva", Last)) %>%
  mutate(Last=ifelse(grepl("Andros Townsend", Player), "Andros Townsend", Last)) %>%
  mutate(Last=ifelse(grepl("Conor Townsend", Player), "Conor Townsend", Last)) %>%
  mutate(Last=ifelse(grepl("Bertrand Traore", Player), "Bertrand Traore", Last)) %>%
  mutate(Last=ifelse(grepl("Adama Traore", Player), "Adama Traore", Last)) %>%
  mutate(Last=ifelse(grepl("Wesley Moraes", Player), "Wesley", Last)) %>%
  mutate(Last=ifelse(grepl("Willian Jose", Player), "Willian Jose", Last)) %>%
  mutate(Last=ifelse(grepl("Callum Wilson", Player), "Callum Wilson", Last)) %>%
  mutate(Last=ifelse(grepl("Kayne Ramsey", Player), "Ramsay", Last)) %>%
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
  mutate(Points=ifelse(grepl("Tomori", Player),0,Points)) %>%
  mutate(Pos=ifelse(grepl("Tomori", Player),"DEF",Pos)) %>%
  mutate(CS=ifelse(grepl("Tomori", Player),0,CS)) %>%
  mutate(Bonus=ifelse(grepl("Tomori", Player),0,Bonus)) %>%
  mutate(Points=ifelse(grepl("Hector", Player),0,Points)) %>%
  mutate(Pos=ifelse(grepl("Hector", Player),"DEF",Pos)) %>%
  mutate(CS=ifelse(grepl("Hector", Player),0,CS)) %>%
  mutate(Bonus=ifelse(grepl("Hector", Player),0,Bonus)) %>%
  mutate(Points=ifelse(grepl("Tchaptchet", Player),0,Points)) %>%
  mutate(Pos=ifelse(grepl("Tchaptchet", Player),"DEF",Pos)) %>%
  mutate(CS=ifelse(grepl("Tchaptchet", Player),0,CS)) %>%
  mutate(Bonus=ifelse(grepl("Tchaptchet", Player),0,Bonus)) %>%
  mutate(Points=ifelse(grepl("Fosu-Mensah", Player),0,Points)) %>%
  mutate(Pos=ifelse(grepl("Fosu-Mensah", Player),"DEF",Pos)) %>%
  mutate(CS=ifelse(grepl("Fosu-Mensah", Player),0,CS)) %>%
  mutate(Bonus=ifelse(grepl("Fosu-Mensah", Player),0,Bonus))
gc()

maindir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(maindir)), dir.create(file.path(maindir)), FALSE)
write.csv(df4, file="outputs/std_2021.csv", row.names = F)