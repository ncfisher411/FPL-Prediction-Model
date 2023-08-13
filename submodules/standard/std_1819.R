## Getting standard player data for 2018/19 season

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

df2 <- read.csv("~/FPL/submodules/positions/positions_1819.csv") %>%
  rename(Pos=Position)
## https://jaseziv.github.io/worldfootballR/articles/extract-fbref-data.html#overview
### Documentation of worldfoortballR above

#Player stats
df <- fb_league_stats(country="ENG", gender = "M", season_end_year = 2019,
                      tier="1st", stat_type = "standard",
                      team_or_player = "player") %>%
  mutate(Season=2019) %>%
  select(-c("Rk", "Matches", "url")) %>%
  select(Player, Squad, Season, everything()) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))
gc()
df[c("First", "Last")] <- str_split_fixed(df$Player, " ", 2)

df3 <- df %>%
  mutate_if(is.character, list(~na_if(.,""))) %>%
  mutate(Last=ifelse(is.na(Last), First, Last)) %>%
  select(-c("Squad", "First")) %>%
  mutate(Last=ifelse(grepl("Adam Smith", Player), "Adam Smith", Last)) %>%
  mutate(Last=ifelse(grepl("Andre Gomes", Player), "Andre Gomes", Last)) %>%
  mutate(Last=ifelse(grepl("Ashley Barnes", Player), "Ashley Barnes", Last)) %>%
  mutate(Last=ifelse(grepl("Harvey Barnes", Player), "Harvey Barnes", Last)) %>%
  mutate(Last=ifelse(grepl("Joe Bennett", Player), "Joe Bennett", Last)) %>%
  mutate(Last=ifelse(grepl("Ryan Bennett", Player), "Ryan Bennett", Last)) %>%
  mutate(Last=ifelse(grepl("Bernardo Silva", Player), "Bernardo Silva", Last)) %>%
  mutate(Last=ifelse(grepl("Oxlade-Chamberlain", Player), "Chamberlain", Last)) %>%
  mutate(Last=ifelse(grepl("Javier Hernandez", Player), "Chicharito", Last)) %>%
  mutate(Last=ifelse(grepl("Seamus Coleman", Player), "Seamus Coleman", Last)) %>%
  mutate(Last=ifelse(grepl("Joel Coleman", Player), "Joel Coleman", Last)) %>%
  mutate(Last=ifelse(grepl("David Luiz", Player), "David Luiz", Last)) %>%
  mutate(Last=ifelse(grepl("David Silva", Player), "David Silva", Last)) %>%
  mutate(Last=ifelse(grepl("Emerson Palmieri", Player), "Emerson", Last)) %>%
  mutate(Last=ifelse(grepl("Fabricio", Player), "Fabri", Last)) %>%
  mutate(Last=ifelse(grepl("Felipe Anderson", Player), "Felipe Anderson", Last)) %>%
  mutate(Last=ifelse(grepl("Demarai Gray", Player), "Demarai Gray", Last)) %>%
  mutate(Last=ifelse(grepl("Andre Gray", Player), "Andre Gray", Last)) %>%
  mutate(Last=ifelse(grepl("Gudmundsson", Player), "Gudmundsson", Last)) %>%
  mutate(Last=ifelse(grepl("Gueye", Player), "Gueye", Last)) %>%
  mutate(Last=ifelse(grepl("Jazz Richards", Player), "Jazz Richards", Last)) %>%
  mutate(Last=ifelse(grepl("Tyreke Johnson", Player), "Tyreke Johnson", Last)) %>%
  mutate(Last=ifelse(grepl("Ben Johnson", Player), "Ben Johnson", Last)) %>%
  mutate(Last=ifelse(grepl("Yaya Toure", Player), "Yaya Toure", Last)) %>%
  mutate(Last=ifelse(grepl("Jonny Castro", Player), "Jonny", Last)) %>%
  mutate(Last=ifelse(grepl("Yaya Toure", Player), "Yaya Toure", Last)) %>%
  mutate(Last=ifelse(grepl("Kepa", Player), "Kepa", Last)) %>%
  mutate(Last=ifelse(grepl("Lewis Cook", Player), "Lewis Cook", Last)) %>%
  mutate(Last=ifelse(grepl("Lucas Perez", Player), "Lucas", Last)) %>%
  mutate(Last=ifelse(grepl("Lucas Moura", Player), "Lucas Moura", Last)) %>%
  mutate(Last=ifelse(grepl("James McCarthy", Player), "James McCarthy", Last)) %>%
  mutate(Last=ifelse(grepl("Alex McCarthy", Player), "Alex McCarthy", Last)) %>%
  mutate(Last=ifelse(grepl("Nampalys Mendy", Player), "Nampalys Mendy", Last)) %>%
  mutate(Last=ifelse(grepl("Benjamin Mendy", Player), "Benjamin Mendy", Last)) %>%
  mutate(Last=ifelse(grepl("Josh Murphy", Player), "Josh Murphy", Last)) %>%
  mutate(Last=ifelse(grepl("Jacob Murphy", Player), "Jacob Murphy", Last)) %>%
  mutate(Last=ifelse(grepl("Ricardo Pereira", Player), "Ricardo Pereira", Last)) %>%
  mutate(Last=ifelse(grepl("Andreas Pereira", Player), "Andreas Pereira", Last)) %>%
  mutate(Last=ifelse(grepl("Kayne Ramsey", Player), "Ramsay", Last)) %>%
  mutate(Last=ifelse(grepl("Diego Rico", Player), "Diego Rico", Last)) %>%
  mutate(Last=ifelse(grepl("Sergio Rico", Player), "Sergio Rico", Last)) %>%
  mutate(Last=ifelse(grepl("Jack Simpson", Player), "Jack Simpson", Last)) %>%
  mutate(Last=ifelse(grepl("Danny Simpson", Player), "Danny Simpson", Last)) %>%
  mutate(Last=ifelse(grepl("Sokratis", Player), "Sokratis", Last)) %>%
  mutate(Last=ifelse(grepl("Heung-min", Player), "Son", Last)) %>%
  mutate(Last=ifelse(grepl("Stankovic", Player), "Stankovic", Last)) %>%
  mutate(Last=ifelse(grepl("Dale Stephens", Player), "Dale Stephens", Last)) %>%
  mutate(Last=ifelse(grepl("Jack Stephens", Player), "Jack Stephens", Last)) %>%
  mutate(Last=ifelse(grepl("Steve Cook", Player), "Steve Cook", Last)) %>%
  mutate(Last=ifelse(grepl("Alexis Sanchez", Player), "Alexis Sanchez", Last)) %>%
  mutate(Last=ifelse(grepl("Davinson Sanchez", Player), "Davinson Sanchez", Last)) %>%
  mutate(Last=ifelse(grepl("Stephen Ward", Player), "Stephen Ward", Last)) %>%
  mutate(Last=ifelse(grepl("Danny Ward", Player), "Danny Ward", Last)) %>%
  mutate(Last=ifelse(grepl("Joel Ward", Player), "Joel Ward", Last)) %>%
  mutate(Last=ifelse(grepl("Xande Silva", Player), "Xande Silva", Last)) %>%
  mutate(Last=ifelse(grepl("Jorgensen", Player), "Zanka", Last)) %>%
  mutate(Last=ifelse(grepl("Carlos Sanchez", Player), "Carlos Sanchez", Last)) %>%
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
  rename(Pos=Pos.x)

maindir <- paste(maindir, "outputs", sep = '/')
ifelse(!dir.exists(file.path(maindir)), dir.create(file.path(maindir)), FALSE)
write.csv(df4, file="outputs/std_1819.csv", row.names = F)