# Combine different data types here for modeling
library(tidyverse)
library(rstudioapi)
library(stringr)
library(stringi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
dir <- getwd()

#---------- Standard -----------#
setwd(paste(dir, "submodules/standard/outputs", sep="/"))
files <- list.files(pattern="\\.csv")
dfs <- list()
for (file in files){
  fileName <- tools::file_path_sans_ext(file)
  fileName <- gsub("std_", "", fileName)
  data <- read.csv(file)
  assign(fileName, data)
  dfs[[fileName]] <- get(fileName)
}
gc()

common <- colnames(data)
for (i in 2:length(dfs)) {
  common <- intersect(common, colnames(dfs[[i]]))
}
for(i in 1:length(dfs)) {
  dfs[[i]] <- dfs[[i]][, common]
}
for(i in 1:length(dfs)) {
  fileName <- names(dfs[i])
  data.frame(assign(fileName, dfs[i]))
}
gc()
`1314` <- `1314` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1314.",""))
`1415` <- `1415` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1415.",""))
`1516` <- `1516` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1516.",""))
`1617` <- `1617` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1617.",""))
`1718` <- `1718` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1718.",""))
`1819` <- `1819` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1819.",""))
`1920` <- `1920` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1920.",""))
`2021` <- `2021` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2021.",""))
`2122` <- `2122` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2122.",""))
`2223` <- `2223` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2223.",""))

standard <- rbind(`1314`,`1516`,`1617`,`1718`,`1819`,`1920`,`2021`,`2122`,`2223`) %>%
  distinct(Player,Season, .keep_all = T) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))



#------------ Shooting --------------#
setwd(paste(dir, "submodules/shooting/outputs", sep="/"))
files <- list.files(pattern="\\.csv")
dfs <- list()
for (file in files){
  fileName <- tools::file_path_sans_ext(file)
  fileName <- gsub("sht_", "", fileName)
  data <- read.csv(file)
  assign(fileName, data)
  dfs[[fileName]] <- get(fileName)
}
gc()

common <- colnames(data)
for (i in 2:length(dfs)) {
  common <- intersect(common, colnames(dfs[[i]]))
}
for(i in 1:length(dfs)) {
  dfs[[i]] <- dfs[[i]][, common]
}
for(i in 1:length(dfs)) {
  fileName <- names(dfs[i])
  data.frame(assign(fileName, dfs[i]))
}
gc()
`1314` <- `1314` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1314.",""))
`1415` <- `1415` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1415.",""))
`1516` <- `1516` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1516.",""))
`1617` <- `1617` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1617.",""))
`1718` <- `1718` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1718.",""))
`1819` <- `1819` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1819.",""))
`1920` <- `1920` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1920.",""))
`2021` <- `2021` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2021.",""))
`2122` <- `2122` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2122.",""))
`2223` <- `2223` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2223.",""))

shooting <- rbind(`1314`,`1516`,`1617`,`1718`,`1819`,`1920`,`2021`,`2122`,`2223`) %>%
  distinct(Player,Season, .keep_all = T) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))


#----------- Passing -------------#
setwd(paste(dir, "submodules/passing/outputs", sep="/"))
files <- list.files(pattern="\\.csv")
dfs <- list()
for (file in files){
  fileName <- tools::file_path_sans_ext(file)
  fileName <- gsub("passing_", "", fileName)
  data <- read.csv(file)
  assign(fileName, data)
  dfs[[fileName]] <- get(fileName)
}
gc()

common <- colnames(data)
for (i in 2:length(dfs)) {
  common <- intersect(common, colnames(dfs[[i]]))
}
for(i in 1:length(dfs)) {
  dfs[[i]] <- dfs[[i]][, common]
}
for(i in 1:length(dfs)) {
  fileName <- names(dfs[i])
  data.frame(assign(fileName, dfs[i]))
}
gc()
`1314` <- `1314` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1314.",""))
`1415` <- `1415` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1415.",""))
`1516` <- `1516` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1516.",""))
`1617` <- `1617` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1617.",""))
`1718` <- `1718` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1718.",""))
`1819` <- `1819` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1819.",""))
`1920` <- `1920` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1920.",""))
`2021` <- `2021` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2021.",""))
`2122` <- `2122` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2122.",""))
`2223` <- `2223` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2223.",""))

passing <- rbind(`1314`,`1516`,`1617`,`1718`,`1819`,`1920`,`2021`,`2122`,`2223`) %>%
  distinct(Player,Season, .keep_all = T) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))

#--------------------- Own goals ----------------------------#
setwd(paste(dir, "submodules/clean sheets/outputs", sep="/"))
trim_leading <- function(x) sub("^\\s+","",x)


## 1314
`1314` <- read.csv("cs_time_own_goals_1314.csv") %>%
  rename(Player=player, Squad=team_name) %>%
  select(Player, Season, own_goals, concede2,
         concede4, concede6, concede8, concede10)
    `1314`$Player <- trim_leading(`1314`$Player)

## 1415
temp <- read.csv("own_goals_1415.csv")
`1415` <- read.csv("time_and_cs_1415.csv") %>%
  rename(Player=Player_Name) %>%
  left_join(temp, by=c("Player"="Player_Name", "Season"="Season")) %>%
  select(Player, Season, own_goals, concede2,
         concede4, concede6, concede8, concede10)

## 1516
temp <- read.csv("own_goals_1516.csv")
`1516` <- read.csv("time_and_cs_1516.csv") %>%
  rename(Player=Player_Name) %>%
  left_join(temp, by=c("Player"="Player_Name", "Season"="Season")) %>%
  select(Player, Season, own_goals, concede2,
         concede4, concede6, concede8, concede10)

## 1617
temp <- read.csv("own_goals_1617.csv")
`1617` <- read.csv("time_and_cs_1617.csv") %>%
  rename(Player=Player_Name) %>%
  left_join(temp, by=c("Player"="Player_Name", "Season"="Season")) %>%
  select(Player, Season, own_goals, concede2,
         concede4, concede6, concede8, concede10)

## 1718
temp <- read.csv("own_goals_1718.csv")
`1718` <- read.csv("time_and_cs_1718.csv") %>%
  rename(Player=Player_Name) %>%
  left_join(temp, by=c("Player"="Player_Name", "Season"="Season")) %>%
  select(Player, Season, own_goals, concede2,
         concede4, concede6, concede8, concede10)

##1819
temp <- read.csv("own_goals_1819.csv")
`1819` <- read.csv("time_and_cs_1819.csv") %>%
  rename(Player=Player_Name) %>%
  left_join(temp, by=c("Player"="Player_Name", "Season"="Season")) %>%
  select(Player, Season, own_goals, concede2,
         concede4, concede6, concede8, concede10)

##1920
temp <- read.csv("own_goals_1920.csv")
`1920` <- read.csv("time_and_cs_1920.csv") %>%
  rename(Player=Player_Name) %>%
  left_join(temp, by=c("Player"="Player_Name", "Season"="Season")) %>%
  select(Player, Season, own_goals, concede2,
         concede4, concede6, concede8, concede10)

## 2021
temp <- read.csv("own_goals_2021.csv")
`2021` <- read.csv("time_and_cs_2021.csv")%>%
  rename(Player=Player_Name) %>%
  left_join(temp, by=c("Player"="Player_Name", "Season"="Season")) %>%
  select(Player, Season, own_goals, concede2,
         concede4, concede6, concede8, concede10)

##2122
temp <- read.csv("own_goals_2122.csv")
`2122` <- read.csv("time_and_cs_2122.csv") %>%
  rename(Player=Player_Name) %>%
  left_join(temp, by=c("Player"="Player_Name", "Season"="Season")) %>%
  select(Player, Season, own_goals, concede2,
         concede4, concede6, concede8, concede10)

##2223
temp <- read.csv("own_goals_2223.csv")
`2223` <- read.csv("time_and_cs_2223.csv") %>%
  rename(Player=Player_Name) %>%
  left_join(temp, by=c("Player"="Player_Name", "Season"="Season")) %>%
  select(Player, Season, own_goals, concede2,
         concede4, concede6, concede8, concede10)

clean_sheets <- rbind(`1314`,`1516`,`1617`,`1718`,`1819`,`1920`,`2021`,`2122`,`2223`) %>%
  distinct(Player,Season, .keep_all = T) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))

#------------------- Keepers ---------------------#
setwd(paste(dir, "submodules/keepers/outputs", sep="/"))
files <- list.files(pattern="\\.csv")
dfs <- list()
for (file in files){
  fileName <- tools::file_path_sans_ext(file)
  fileName <- gsub("keepers_", "", fileName)
  data <- read.csv(file)
  assign(fileName, data)
  dfs[[fileName]] <- get(fileName)
}
gc()

common <- colnames(data)
for (i in 2:length(dfs)) {
  common <- intersect(common, colnames(dfs[[i]]))
}
for(i in 1:length(dfs)) {
  dfs[[i]] <- dfs[[i]][, common]
}
for(i in 1:length(dfs)) {
  fileName <- names(dfs[i])
  data.frame(assign(fileName, dfs[i]))
}
gc()
keeper_1314 <- keeper_1314 %>% data.frame() %>% rename_all(~stringr::str_replace(.,"keeper_1314.",""))
keeper_1415 <- keeper_1415 %>% data.frame() %>% rename_all(~stringr::str_replace(.,"keeper_1415.",""))
keeper_1516 <- keeper_1516 %>% data.frame() %>% rename_all(~stringr::str_replace(.,"keeper_1516.",""))
keeper_1617 <- keeper_1617 %>% data.frame() %>% rename_all(~stringr::str_replace(.,"keeper_1617.",""))
keeper_1718 <- keeper_1718 %>% data.frame() %>% rename_all(~stringr::str_replace(.,"keeper_1718.",""))
keeper_1819 <- keeper_1819 %>% data.frame() %>% rename_all(~stringr::str_replace(.,"keeper_1819.",""))
keeper_1920 <- keeper_1920 %>% data.frame() %>% rename_all(~stringr::str_replace(.,"keeper_1920.",""))
keeper_2021 <- keeper_2021 %>% data.frame() %>% rename_all(~stringr::str_replace(.,"keeper_2021.",""))
keeper_2122 <- keeper_2122 %>% data.frame() %>% rename_all(~stringr::str_replace(.,"keeper_2122.",""))
keeper_2223 <- keeper_2223 %>% data.frame() %>% rename_all(~stringr::str_replace(.,"keeper_2223.",""))

keepers <- rbind(keeper_1314, keeper_1415, keeper_1516, keeper_1617, keeper_1718,
                 keeper_1819, keeper_1920, keeper_2021, keeper_2122, keeper_2223) %>%
  distinct(Player,Season, .keep_all = T) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))


##-------------------- Defense --------------------##
setwd(paste(dir, "submodules/defense/outputs", sep="/"))
files <- list.files(pattern="\\.csv")
dfs <- list()
for (file in files){
  fileName <- tools::file_path_sans_ext(file)
  fileName <- gsub("def_", "", fileName)
  data <- read.csv(file)
  assign(fileName, data)
  dfs[[fileName]] <- get(fileName)
}
gc()

common <- colnames(data)
for (i in 2:length(dfs)) {
  common <- intersect(common, colnames(dfs[[i]]))
}
for(i in 1:length(dfs)) {
  dfs[[i]] <- dfs[[i]][, common]
}
for(i in 1:length(dfs)) {
  fileName <- names(dfs[i])
  data.frame(assign(fileName, dfs[i]))
}
gc()

`1314` <- `1314` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1314.",""))
`1415` <- `1415` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1415.",""))
`1516` <- `1516` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1516.",""))
`1617` <- `1617` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1617.",""))
`1718` <- `1718` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1718.",""))
`1819` <- `1819` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1819.",""))
`1920` <- `1920` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1920.",""))
`2021` <- `2021` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2021.",""))
`2122` <- `2122` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2122.",""))
`2223` <- `2223` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2223.",""))

defense <- rbind(`1314`,`1516`,`1617`,`1718`,`1819`,`1920`,`2021`,`2122`,`2223`) %>%
  distinct(Player,Season, .keep_all = T) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))


##-------------------- Possession --------------------##
setwd(paste(dir, "submodules/possession/outputs", sep="/"))
files <- list.files(pattern="\\.csv")
dfs <- list()
for (file in files){
  fileName <- tools::file_path_sans_ext(file)
  fileName <- gsub("pos_", "", fileName)
  data <- read.csv(file)
  assign(fileName, data)
  dfs[[fileName]] <- get(fileName)
}
gc()

common <- colnames(data)
for (i in 2:length(dfs)) {
  common <- intersect(common, colnames(dfs[[i]]))
}
for(i in 1:length(dfs)) {
  dfs[[i]] <- dfs[[i]][, common]
}
for(i in 1:length(dfs)) {
  fileName <- names(dfs[i])
  data.frame(assign(fileName, dfs[i]))
}
gc()

`1314` <- `1314` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1314.",""))
`1415` <- `1415` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1415.",""))
`1516` <- `1516` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1516.",""))
`1617` <- `1617` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1617.",""))
`1718` <- `1718` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1718.",""))
`1819` <- `1819` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1819.",""))
`1920` <- `1920` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X1920.",""))
`2021` <- `2021` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2021.",""))
`2122` <- `2122` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2122.",""))
`2223` <- `2223` %>% data.frame() %>% rename_all(~stringr::str_replace(.,"X2223.",""))

possession <- rbind(`1314`,`1516`,`1617`,`1718`,`1819`,`1920`,`2021`,`2122`,`2223`) %>%
  distinct(Player,Season, .keep_all = T) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII"))

#---- Combine for outfield players ------#
    ## Have to generalize positions, will lose some accuracy
## Apply scoring criteria by position
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

outfield <- standard %>%
  left_join(shooting, by=c("Player", "Squad", "Season")) %>%
  select(-ends_with(".y")) %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x")) %>%
  left_join(passing, by=c("Player", "Squad", "Season")) %>%
  select(-ends_with(".y")) %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x")) %>%
  left_join(defense, by=c("Player", "Squad", "Season")) %>%
  select(-ends_with(".y")) %>%
  rename_with(~gsub("\\.x$","",.), ends_with(".x")) %>%
  left_join(possession, by=c("Player", "Squad", "Season")) %>%
  select(-ends_with(".y")) %>%
  rename_with(~gsub("\\.x$","",.), ends_with(".x")) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII")) %>%
  left_join(clean_sheets, by=c("Player", "Season")) %>%
  filter(Pos!="GKP") %>%
  select(Player, Squad, Season, Nation, Pos, Age, Born, Points, everything())
gc()

temp <- read.csv("outfield_column_names.csv") %>%
  filter(Drop==1)
outfield <- outfield %>%
  select(-c("Mins_Per_90", "Gls_Standard", "PK_Standard", "PKatt_Standard", "Matches",
            "url", "Tkl_Challenges"))

temp <- read.csv("outfield_column_names.csv") %>%
  filter(is.na(Drop))
temp2 <- temp$Stat
temp3 <- colnames(outfield)
setdiff(temp3, temp2)
temp2 <- temp$Var
colnames(outfield) <- temp2

outfield <- outfield %>%
  group_by(Squad, Season, Position) %>%
  mutate(SquadRank=rank(-MinutesPlayed)) %>%
  ungroup()


#---------- Combine for keepers ---------------#
    ## Will lose accuracy due to lack of save data
temp <- standard %>%
  filter(Pos=="GKP") %>%
  select(Player, Squad, Season, Points, Bonus, Pos)

keeps <- keepers %>%
  left_join(temp, by=c("Player", "Squad", "Season")) %>%
  mutate(Pos.x=Pos.y) %>%
  select(-ends_with(".y")) %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x")) %>%
  left_join(shooting, by=c("Player", "Squad", "Season")) %>%
  select(-ends_with(".y")) %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x")) %>%
  left_join(standard, by=c("Player", "Squad", "Season")) %>%
  select(-ends_with(".y")) %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x")) %>%
  left_join(passing, by=c("Player", "Squad", "Season")) %>%
  select(-ends_with(".y")) %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x")) %>%
  left_join(defense, by=c("Player", "Squad", "Season")) %>%
  select(-ends_with(".y")) %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x")) %>%
  left_join(possession, by=c("Player", "Squad", "Season")) %>%
  select(-ends_with(".y")) %>%
  rename_with(~gsub("\\.x$", "", .), ends_with(".x")) %>%
  mutate(Player=stri_trans_general(Player, "Latin-ASCII")) %>%
  left_join(clean_sheets, by=(c("Player", "Season"))) %>%
  select(Player, Squad, Season, Nation, Pos, Age, Born, Points, Bonus, everything())

temp <- read.csv("keepers_column_titles.csv") %>%
  filter(Drop==1)
keeps <- keeps %>%
  select(-c("Matches", "url", "Mins_Per_90", "Gls_Standard", "PK_Standard",
            "PKatt_Standard", "Tkl_Challenges"))
temp <- read.csv("keepers_column_titles.csv") %>%
  filter(is.na(Drop))
temp2 <- temp$Var
colnames(keeps) <- temp2

keeps <- keeps %>%
  group_by(Squad, Season, Position) %>%
  mutate(SquadRank=rank(-MinutesPlayed)) %>%
  ungroup()

## Save files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
write.csv(outfield, "outfield_players.csv")
write.csv(keeps, "keepers.csv")
