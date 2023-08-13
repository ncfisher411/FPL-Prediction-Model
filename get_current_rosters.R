# Get current rosters
library(tidyverse)
library(worldfootballR)
library(stringi)

## Get transfers and their stats from last year. Adjust the stats for regression based on league strength

map <- player_dictionary_mapping() 

teams <- tm_league_team_urls(country_name = "England", start_year = "2023")

transfers <- data.frame()
for (team in teams) {
  roster <- tm_team_transfers(team_url = team, transfer_window = "all")
  transfers <- bind_rows(transfers, roster)
}
df <- transfers %>% filter(transfer_type=="Arrivals") %>%
  filter(league_2!="Premier League")

df2 <- transfers %>% filter(transfer_type=="Departures") %>%
  filter(league_2!="Premier League")

departures <- map %>% filter(UrlTmarkt %in% df2$player_url) %>%
  mutate(PlayerFBref=stri_trans_general(PlayerFBref, "Latin-ASCII"))

arrivals <- map %>% filter(UrlTmarkt %in% df$player_url) %>%
  mutate(PlayerFBref=stri_trans_general(PlayerFBref, "Latin-ASCII")) %>%
  filter(!PlayerFBref %in% departures$PlayerFBref)

temp <- arrivals$UrlFBref

### Standard data
# standard <- data.frame()
# for(url in temp) {
#   stats <- fb_player_season_stats(url, stat_type = "standard")
#   standard <- bind_rows(standard, stats)
# }
# gc()
# standard <- standard %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII"))
# temp <- setdiff(unique(arrivals$PlayerFBref), unique(standard$player_name))
# write.csv(standard, "submodules/standard/Std_otherLeaguePlayers.csv")
standard <- read.csv("submodules/standard/Std_otherLeaguePlayers.csv") %>%
  select(-X) %>% 
  mutate(player_name=stri_trans_general(player_name, "Latin-ASCII")) %>%
  filter(Season=="2021-2022" | Season=="2022-2023") %>%
  mutate(Season2=ifelse(Season=="2021-2022",2022,2023)) %>%
  mutate(Season=Season2) %>%
  select(-Season2) %>%
  filter(grepl("1.", Comp) | grepl("2.", Comp) | grepl("3.", Comp)) %>%
  mutate(Comp=substr(Comp, start=4, stop = nchar(Comp))) %>%
  rename(League=Comp) %>%
  filter(!grepl("PL", League) & !grepl("Lg", League)) %>%
  mutate(MP=ifelse(is.na(MP) & !is.na(MP_Time), MP_Time, MP)) %>%
  select(-MP_Time) %>%
  mutate(player_name=ifelse(player_name=="Ionu<U+021B> Radu","Ionut Radu",player_name))
temp <- unique(standard$League)
temp
temp <- setdiff(unique(arrivals$UrlFBref), unique(standard$player_url))
temp

## Shooting
# temp <- arrivals$UrlFBref
# shooting <- data.frame()
# for(url in temp) {
#   stats <- fb_player_season_stats(url, stat_type = "shooting")
#   shooting <- bind_rows(shooting, stats)
# }
# gc()
# shooting <- shooting %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII"))
# temp <- setdiff(unique(shooting$player_name), unique(arrivals$PlayerFBref))
# write.csv(shooting, "submodules/shooting/Sht_otherLeaguePlayers.csv")
shooting <- read.csv("submodules/shooting/Sht_otherLeaguePlayers.csv") %>%
  select(-X) %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII")) %>%
  filter(Season=="2021-2022" | Season=="2022-2023") %>%
  mutate(Season2=ifelse(Season=="2021-2022",2022,2023)) %>%
  mutate(Season=Season2) %>%
  select(-Season2) %>%
  filter(grepl("1.", Comp) | grepl("2.", Comp) | grepl("3.", Comp)) %>%
  mutate(Comp=substr(Comp, start=4, stop = nchar(Comp))) %>%
  rename(League=Comp) %>%
  filter(!grepl("PL", League) & !grepl("Lg", League)) %>%
  #select(-LgRank) %>%
  mutate(player_name=ifelse(player_name=="Ionu<U+021B> Radu","Ionut Radu",player_name))
temp <- unique(shooting$League)
temp
temp <- setdiff(unique(arrivals$UrlFBref), unique(shooting$player_url))
temp


##Defense
# defense <- data.frame()
# temp <- arrivals$UrlFBref
# for(url in temp) {
#   stats <- fb_player_season_stats(url, stat_type = "defense")
#   defense <- bind_rows(defense, stats)
# }
# gc()
# defense <- defense %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII"))
# temp <- setdiff(unique(defense$player_name), unique(arrivals$PlayerFBref))
# write.csv(defense, "submodules/defense/def_otherLeaguePlayers.csv")
defense <- read.csv("submodules/defense/def_otherLeaguePlayers.csv") %>%
  select(-X) %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII")) %>%
  filter(Season=="2021-2022" | Season=="2022-2023") %>%
  mutate(Season2=ifelse(Season=="2021-2022",2022,2023)) %>%
  mutate(Season=Season2) %>%
  select(-Season2) %>%
  filter(grepl("1.", Comp) | grepl("2.", Comp) | grepl("3.", Comp)) %>%
  mutate(Comp=substr(Comp, start=4, stop = nchar(Comp))) %>%
  rename(League=Comp) %>%
  filter(!grepl("PL", League) & !grepl("Lg", League)) %>%
  # select(-LgRank) %>%
  mutate(player_name=ifelse(player_name=="Ionu<U+021B> Radu","Ionut Radu",player_name))
temp <- unique(defense$League)
temp
temp <- setdiff(unique(arrivals$UrlFBref), unique(defense$player_url))
temp


## Passing
# passing <- data.frame()
# temp <- arrivals$UrlFBref
# for(url in temp) {
#   stats <- fb_player_season_stats(url, stat_type = "passing")
#   passing <- bind_rows(passing, stats)
# }
# gc()
# passing <- passing %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII"))
# temp <- setdiff(unique(defense$player_name), unique(arrivals$PlayerFBref))
# write.csv(passing, "submodules/passing/pass_otherLeaguePlayers.csv")
passing <- read.csv("submodules/passing/pass_otherLeaguePlayers.csv") %>%
  select(-X) %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII")) %>%
  filter(Season=="2021-2022" | Season=="2022-2023") %>%
  mutate(Season2=ifelse(Season=="2021-2022",2022,2023)) %>%
  mutate(Season=Season2) %>%
  select(-Season2) %>%
  filter(grepl("1.", Comp) | grepl("2.", Comp) | grepl("3.", Comp)) %>%
  mutate(Comp=substr(Comp, start=4, stop = nchar(Comp))) %>%
  rename(League=Comp) %>%
  filter(!grepl("PL", League) & !grepl("Lg", League)) %>%
  # select(-LgRank) %>%
  mutate(player_name=ifelse(player_name=="Ionu<U+021B> Radu","Ionut Radu",player_name))
temp <- unique(passing$League)
temp
temp <- setdiff(unique(arrivals$UrlFBref), unique(passing$player_url))
temp


## Possession
# possession <- data.frame()
# temp <- arrivals$UrlFBref
# for(url in temp) {
#   stats <- fb_player_season_stats(url, stat_type = "possession")
#   possession <- bind_rows(possession, stats)
# }
# gc()
# possession <- possession %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII"))
# temp <- setdiff(unique(possession$player_name), unique(arrivals$PlayerFBref))
# write.csv(possession, "submodules/possession/pos_otherLeaguePlayers.csv")
possession <- read.csv("submodules/possession/pos_otherLeaguePlayers.csv") %>%
  select(-X) %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII")) %>%
  filter(Season=="2021-2022" | Season=="2022-2023") %>%
  mutate(Season2=ifelse(Season=="2021-2022",2022,2023)) %>%
  mutate(Season=Season2) %>%
  select(-Season2) %>%
  filter(grepl("1.", Comp) | grepl("2.", Comp) | grepl("3.", Comp)) %>%
  mutate(Comp=substr(Comp, start=4, stop = nchar(Comp))) %>%
  rename(League=Comp) %>%
  filter(!grepl("PL", League) & !grepl("Lg", League)) %>%
  # select(-LgRank) %>%
  mutate(player_name=ifelse(player_name=="Ionu<U+021B> Radu","Ionut Radu",player_name))
temp <- unique(possession$League)
temp
temp <- setdiff(unique(arrivals$UrlFBref), unique(possession$player_url))
temp


## Goalkeeping
# gk <- data.frame()
# temp <- arrivals$UrlFBref
# for(url in temp) {
#   stats <- fb_player_season_stats(url, stat_type = "keeper")
#   gk <- bind_rows(gk, stats)
# }
# gc()
# write.csv(gk, "submodules/keepers/gk_otherLeaguePlayers.csv")
gk <- read.csv("submodules/keepers/gk_otherLeaguePlayers.csv") %>%
  select(-X) %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII")) %>%
  filter(Season=="2021-2022" | Season=="2022-2023") %>%
  mutate(Season2=ifelse(Season=="2021-2022",2022,2023)) %>%
  mutate(Season=Season2) %>%
  select(-Season2) %>%
  filter(grepl("1.", Comp) | grepl("2.", Comp) | grepl("3.", Comp)) %>%
  mutate(Comp=substr(Comp, start=4, stop = nchar(Comp))) %>%
  rename(League=Comp) %>%
  filter(!grepl("PL", League) & !grepl("Lg", League)) %>%
  mutate(player_name=ifelse(player_name=="Ionu<U+021B> Radu","Ionut Radu",player_name))
temp <- unique(gk$League)
temp
temp <- arrivals %>% filter(TmPos=="Goalkeeper")
temp2 <- setdiff(unique(temp$UrlFBref), unique(gk$player_url))
temp2

### Game logs
#  df3 <- data.frame()
#  temp <- arrivals$UrlFBref
#  for(url in temp) {
#  tryCatch({ stats <- fb_player_match_logs(url, season_end_year = 2022, stat_type = "summary")
# 
#  if(!("OG_Performance" %in% colnames(stats))) {
#     stats$OG_Performance <- NA
#    }
# 
#  if(!("Result" %in% colnames(stats))) {
#    stats$Result <- NA
#   }
# 
#  if(!("Min" %in% colnames(stats))) {
#    stats$Min <- NA
#  }
# 
#  stats2 <- stats[, c("Player", "Result", "Comp", "Min", "OG_Performance")]
#  df3 <- rbind(df3, stats2)}, error = function(e) {
#    cat("Error occured for URL", url, "\n")
#   })
#  }
#  gc()
# 
#  df4 <- data.frame()
#  temp <- arrivals$UrlFBref
#  for(url in temp) {
#    tryCatch({ stats <- fb_player_match_logs(url, season_end_year = 2023, stat_type = "summary")
# 
#    if(!("OG_Performance" %in% colnames(stats))) {
#      stats$OG_Performance <- NA
#    }
# 
#    if(!("Result" %in% colnames(stats))) {
#      stats$Result <- NA
#    }
# 
#    if(!("Min" %in% colnames(stats))) {
#      stats$Min <- NA
#    }
# 
#    stats2 <- stats[, c("Player", "Result", "Comp", "Min", "OG_Performance")]
#    df4 <- rbind(df4, stats2)}, error = function(e) {
#      cat("Error occured for URL", url, "\n")
#    })
#  }
#  gc()
# 
#  df5 <- df3 %>%
#    mutate(Result=substr(Result, nchar(Result), nchar(Result))) %>%
#    mutate(Result=as.numeric(Result)) %>%
#    rename(opp_score=Result, OwnGoals=5) %>%
#    mutate(CleanSheets=ifelse(opp_score==0 & Min > 59, 1, 0)) %>%
#    mutate(concede2=ifelse(opp_score>1,1,0)) %>%
#    mutate(concede4=ifelse(opp_score>3,1,0)) %>%
#    mutate(concede6=ifelse(opp_score>5,1,0)) %>%
#    mutate(concede8=ifelse(opp_score>7,1,0)) %>%
#    mutate(concede10=ifelse(opp_score>9,1,0)) %>%
#    mutate(time_points=ifelse(Min<60,1,0)) %>%
#    mutate(time_points=ifelse(Min>59,2,0)) %>%
#    group_by(Player) %>%
#    summarize(CS=sum(CleanSheets, na.rm=T),
#              own_goals=sum(OwnGoals, na.rm = T),
#              time_points=sum(time_points, na.rm=T),
#              concede2=sum(concede2, na.rm = T),
#              concede4=sum(concede4, na.rm=T),
#              concede6=sum(concede6, na.rm = T),
#              concede8=sum(concede8, na.rm = T),
#              concede10=sum(concede10, na.rm=T)) %>%
#    ungroup() %>%
#    mutate(Player=stri_trans_general(Player, "Latin-ASCII")) %>%
#    mutate(Player=ifelse(Player=="Ionu<U+021B> Radu","Ionut Radu",Player)) %>%
#    mutate(Season=2022)
#  temp <- setdiff(unique(arrivals$PlayerFBref), unique(df5$Player))
#  temp
# 
#  df6 <- df4 %>%
#    mutate(Result=substr(Result, nchar(Result), nchar(Result))) %>%
#    mutate(Result=as.numeric(Result)) %>%
#    rename(opp_score=Result, OwnGoals=5) %>%
#    mutate(CleanSheets=ifelse(opp_score==0 & Min > 59, 1, 0)) %>%
#    mutate(concede2=ifelse(opp_score>1,1,0)) %>%
#    mutate(concede4=ifelse(opp_score>3,1,0)) %>%
#    mutate(concede6=ifelse(opp_score>5,1,0)) %>%
#    mutate(concede8=ifelse(opp_score>7,1,0)) %>%
#    mutate(concede10=ifelse(opp_score>9,1,0)) %>%
#    mutate(time_points=ifelse(Min<60,1,0)) %>%
#    mutate(time_points=ifelse(Min>59,2,0)) %>%
#    group_by(Player) %>%
#    summarize(CS=sum(CleanSheets, na.rm=T),
#              own_goals=sum(OwnGoals, na.rm = T),
#              time_points=sum(time_points, na.rm=T),
#              concede2=sum(concede2, na.rm = T),
#              concede4=sum(concede4, na.rm=T),
#              concede6=sum(concede6, na.rm = T),
#              concede8=sum(concede8, na.rm = T),
#              concede10=sum(concede10, na.rm=T)) %>%
#    ungroup() %>%
#    mutate(Player=stri_trans_general(Player, "Latin-ASCII")) %>%
#    mutate(Player=ifelse(Player=="Ionu<U+021B> Radu","Ionut Radu",Player)) %>%
#    mutate(Season=2023)
#  temp <- setdiff(unique(arrivals$PlayerFBref), unique(df6$Player))
#  temp
# 
# match_logs <- rbind(df5, df6)
# write.csv(match_logs, "submodules/clean sheets/match_logs_otherLeaguePlayers.csv")
match_logs <- read.csv("submodules/clean sheets/match_logs_otherLeaguePlayers.csv")

# Join all stats
## outfield
temp <- c("player_name", "player_url", "Season", "Age", "Squad", "Country", "League")
temp2 <- arrivals %>% rename(player_name=1, Position=4) %>%
  mutate(Position=ifelse(grepl("Forward", Position),"FWD",Position)) %>%
  mutate(Position=ifelse(grepl("Striker", Position),"FWD",Position)) %>%
  mutate(Position=ifelse(grepl("Attack", Position),"FWD",Position)) %>%
  mutate(Position=ifelse(grepl("attack", Position),"FWD",Position)) %>%
  mutate(Position=ifelse(grepl("Midfield", Position),"MID",Position)) %>%
  mutate(Position=ifelse(grepl("Winger", Position),"MID",Position)) %>%
  mutate(Position=ifelse(grepl("midfield", Position),"MID",Position)) %>%
  mutate(Position=ifelse(grepl("Back", Position),"DEF",Position)) %>%
  mutate(Position=ifelse(grepl("Defence", Position),"DEF",Position)) %>%
  mutate(Position=ifelse(grepl("Defender", Position),"DEF",Position)) %>%
  mutate(Position=ifelse(grepl("Goalkeeper", Position),"GKP",Position)) %>%
  select(player_name, Position)
df7 <- standard %>% 
  left_join(temp2, by="player_name") %>%
  left_join(shooting, by=temp) %>%
  select(-contains(".y")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$","",.))) %>%
  left_join(defense, by=temp) %>%
  select(-contains(".y")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$","",.))) %>%
  left_join(passing, by=temp) %>%
  select(-contains(".y")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$","",.))) %>%
  left_join(possession, by=temp) %>%
  select(-contains(".y")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$","",.))) %>%
  left_join(match_logs, by=c("player_name"="Player", "Season"="Season")) %>%
  select(-contains(".y")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$","",.))) %>%
  mutate(League=ifelse(League=="First Division A", "Pro League A", League)) %>%
  distinct(player_name, Season, Squad, League, .keep_all = T)
df8 <- df7 %>% filter(Position=="GKP") %>%
  left_join(gk, by=temp) %>%
  select(-contains(".y")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$","",.)))
df9 <- df7 %>% filter(Position!="GKP")

## MUST HAVE THE COMBINED DATA SET PROCESSED FOR THIS STEP
outfield <- read.csv("outfield_players.csv") %>% filter(Season > 2021)
keepers <- read.csv("keepers.csv") %>% filter(Season > 2021)
temp <- read.csv("outfield_column_names.csv")
temp2 <- read.csv("keepers_column_titles.csv")


### Need to filter leagues and create factors for reducing points relative to the premier league
##### Leverages the FiveThirtyEight Soccer Club Power Index by averaging the index rating by league, updated June 2023
##### https://projects.fivethirtyeight.com/soccer-predictions/global-club-rankings/ 

temp <- unique(df7$League)
temp

spi <- read.csv("soccer-spi/spi_global_rankings.csv") %>% group_by(league) %>%
  summarize(spi=mean(spi)) %>% mutate(league=stri_trans_general(league, "Latin-ASCII")) %>%
  mutate(league2=NA) %>%
  mutate(league2=ifelse(league=="German Bundesliga", "Bundesliga", league2)) %>%
  mutate(league2=ifelse(league=="Barclays Premier League", "Premier League", league2)) %>%
  mutate(league2=ifelse(league=="French Ligue 1", "Ligue 1", league2)) %>%
  mutate(league2=ifelse(league=="Dutch Eredivisie", "Eredivisie", league2)) %>%
  mutate(league2=ifelse(league=="Italy Serie A", "Serie A", league2)) %>%
  mutate(league2=ifelse(league=="English League Championship", "Championship", league2)) %>%
  mutate(league2=ifelse(league=="Portuguese Liga", "Primeira Liga", league2)) %>%
  mutate(league2=ifelse(league=="Spanish Primera Division", "La Liga", league2)) %>%
  mutate(league2=ifelse(league=="Scottish Premiership", "Premiership", league2)) %>%
  mutate(league2=ifelse(league=="English League One", "League One", league2)) %>%
  mutate(league2=ifelse(league=="Turkish Turkcell Super Lig", "Süper Lig", league2)) %>%
  mutate(league2=ifelse(league=="Danish SAS-Ligaen", "Superliga", league2)) %>%
  mutate(league2=ifelse(league=="Belgian Jupiler League", "Pro League A", league2)) %>%
  mutate(league2=ifelse(league=="French Ligue 2", "Ligue 2", league2)) %>%
  mutate(league2=ifelse(league=="Greek Super League", "Super League", league2)) %>%
  mutate(league2=ifelse(league=="German 2. Bundesliga", "2. Bundesliga", league2)) %>%
  # mutate(league2=ifelse(league=="Belgian Jupiler League", "First Division A", league2)) %>%
  select(league2, spi) %>% rename(League=1) %>% arrange(-spi) %>% filter(!is.na(League))

setdiff(unique(df7$League), unique(spi$League))

###### Calculate missing values, fill in the gaps using rankings from here:
###### https://www.teamform.com/en/league-ranking/world, using most similar league in ranking
###### Eerste Divisie, using League One: 23.6/67.4=X/61.2
temp <- data.frame(League="Eerste Divisie", spi=23.6*61.2/67.4) 
spi <- rbind(spi, temp)


## Factor is taken as a percentage of SPI, applied broadly to statistics to estimate minutes, goals, etc when translating stats from other leagues
spi <- spi %>% mutate(penalty=spi/spi[1]) %>% select(League, penalty)


df9 <- df9 %>% select(-player_url, -Country, -time_points, -Mins_Per_90, -PrgP, -X) %>%
  rename(Player=player_name, MatchesPlayed=MP, MatchesStarted=Starts_Time,
         MinutesPlayed=Min_Time, MinutesPer90=Mins_Per_90_Time, Goals=Gls,
         Assists=Ast, GoalContributions=G.A, GoalsMinusPKs=G_minus_PK,
         Pkgoals=PK, Pkattempts=PKatt, YellowCards=CrdY, RedCards=CrdR,
         GoalsPer90=Gls_Per_Minutes, AssistsPer90=Ast_Per_Minutes, 
         GoalsContributionsPer90=G.A_Per_Minutes, GoalsNoPKsPer90=G_minus_PK_Per_Minutes,
         GoalsStd=Gls_Standard, Shots=Sh_Standard,
         ShotsOnTarget=SoT_Standard, PctShotsOnTarget=SoT_percent_Standard,
         GoalsPerShot=G_per_Sh_Standard, GoalsPerShotOnTarget=G_per_SoT_Standard,
         AvgShotDistance=Dist_Standard, PKstd=PK_Standard, Tackles=Tkl_Tackles,
         TacklePossession=TklW_Tackles, DribTackles=Tkl_Challenges, Def3rdTackles=Def.3rd_Tackles,
         Mid3rdTackles=Mid.3rd_Tackles, Atk3rdTackles=Att.3rd_Tackles, TackleAttempt=Att_Challenges,
         TacklePct=Tkl_percent_Challenges, LostTackle=Lost_Challenges, TotalBlocks=Blocks_Blocks,
         ShotBlocks=Sh_Blocks, PassBlocks=Pass_Blocks, Interceptions=Int,
         `Tackle&Interception`=Tkl.Int, Clearances=Clr, ErrorsToShots=Err, CompletePasses=Cmp_Total,
         PassAttempt=Att_Total, PassCompletePct=Cmp_percent_Total, TotalPassDistance=TotDist_Total,
         ProgressivePassDistance=PrgDist_Total, CompleteShortPass=Cmp_Short, ShortPassAttempt=Att_Short,
         ShortPassCompletePct=Cmp_percent_Short, CompleteMedPass=Cmp_Medium, MedPassAttempt=Att_Medium,
         MedPassCompletePct=Cmp_percent_Medium, CompleteLongPass=Cmp_Long, LongPassAttempt=Att_Long,
         LongPassCompletePct=Cmp_percent_Long, KeyPasses=KP, PassFinal3rd=Final_Third,
         PassPKArea=PPA, CrossPKArea=CrsPA, NumberTouches=Touches_Touches,
         DefPKTouches=Def.Pen_Touches, Def3rdTouches=Def.3rd_Touches, Mid3rdTouches=Mid.3rd_Touches,
         Atk3rdTouches=Att.3rd_Touches, AtkPKTouches=Att.Pen_Touches, LiveBallTouches=Live_Touches,
         AttackingTakeOn=Att_Take_Ons, SuccessfulTakeOn=Succ_Take_Ons, TakeOnPct=Succ_percent_Take_Ons,
         TimesTackled=Tkld_Take_Ons, TimesTackledPct=Tkld_percent_Take_Ons, Carries=Carries_Carries,
         CarryDistance=TotDist_Carries, ProgressiveCarryDistance=PrgDist_Carries,
         Atk3rdCarries=Final_Third_Carries, AtkPKCarries=CPA_Carries, Miscontrols=Mis_Carries,
         Dispossed=Dis_Carries, CleanSheets=CS, OwnGoals=own_goals) %>%
  left_join(spi, by="League") %>%
  mutate_at(vars(-Player, -Squad, -MatchesPlayed, -MatchesStarted, -MinutesPlayed, -Squad, -League,
                 -Position, -YellowCards, -RedCards, -Age, -CleanSheets, -OwnGoals, -concede2,
                 -concede4, -concede6, -concede8, -concede10, -Season), ~ . *penalty)

temp <- intersect(names(df9), names(outfield))
df9 <- df9 %>% select(temp)

df8 <- df8 %>% select(-player_url, -Country, -time_points, -Mins_Per_90, -PrgP, -X) %>%
  rename(Player=player_name, MatchesPlayed=MP, MatchesStarted=Starts_Time,
         MinutesPlayed=Min_Time, MinutesPer90=Mins_Per_90_Time, Goals=Gls,
         Assists=Ast, GoalContributions=G.A, GoalsMinusPKs=G_minus_PK,
         Pkgoals=PK, Pkattempts=PKatt, YellowCards=CrdY, RedCards=CrdR,
         GoalsPer90=Gls_Per_Minutes, AssistsPer90=Ast_Per_Minutes, 
         GoalsContributionsPer90=G.A_Per_Minutes, GoalsNoPKsPer90=G_minus_PK_Per_Minutes,
         GoalsStd=Gls_Standard, Shots=Sh_Standard,
         ShotsOnTarget=SoT_Standard, PctShotsOnTarget=SoT_percent_Standard,
         GoalsPerShot=G_per_Sh_Standard, GoalsPerShotOnTarget=G_per_SoT_Standard,
         AvgShotDistance=Dist_Standard, PKstd=PK_Standard, Tackles=Tkl_Tackles,
         TacklePossession=TklW_Tackles, DribTackles=Tkl_Challenges, Def3rdTackles=Def.3rd_Tackles,
         Mid3rdTackles=Mid.3rd_Tackles, Atk3rdTackles=Att.3rd_Tackles, TackleAttempt=Att_Challenges,
         TacklePct=Tkl_percent_Challenges, LostTackle=Lost_Challenges, TotalBlocks=Blocks_Blocks,
         ShotBlocks=Sh_Blocks, PassBlocks=Pass_Blocks, Interceptions=Int,
         `Tackle&Interception`=Tkl.Int, Clearances=Clr, ErrorsToShots=Err, CompletePasses=Cmp_Total,
         PassAttempt=Att_Total, PassCompletePct=Cmp_percent_Total, TotalPassDistance=TotDist_Total,
         ProgressivePassDistance=PrgDist_Total, CompleteShortPass=Cmp_Short, ShortPassAttempt=Att_Short,
         ShortPassCompletePct=Cmp_percent_Short, CompleteMedPass=Cmp_Medium, MedPassAttempt=Att_Medium,
         MedPassCompletePct=Cmp_percent_Medium, CompleteLongPass=Cmp_Long, LongPassAttempt=Att_Long,
         LongPassCompletePct=Cmp_percent_Long, KeyPasses=KP, PassFinal3rd=Final_Third,
         PassPKArea=PPA, CrossPKArea=CrsPA, NumberTouches=Touches_Touches,
         DefPKTouches=Def.Pen_Touches, Def3rdTouches=Def.3rd_Touches, Mid3rdTouches=Mid.3rd_Touches,
         Atk3rdTouches=Att.3rd_Touches, AtkPKTouches=Att.Pen_Touches, LiveBallTouches=Live_Touches,
         AttackingTakeOn=Att_Take_Ons, SuccessfulTakeOn=Succ_Take_Ons, TakeOnPct=Succ_percent_Take_Ons,
         TimesTackled=Tkld_Take_Ons, TimesTackledPct=Tkld_percent_Take_Ons, Carries=Carries_Carries,
         CarryDistance=TotDist_Carries, ProgressiveCarryDistance=PrgDist_Carries,
         Atk3rdCarries=Final_Third_Carries, AtkPKCarries=CPA_Carries, Miscontrols=Mis_Carries,
         Dispossed=Dis_Carries, CleanSheets=CS, OwnGoals=own_goals, GoalsAgainst=GA,
         GoalsAgainstPer90=GA90, ShotsOnTargetAgainst=SoTA, SavePercent=Save_percent,
         Wins=W, Draws=D, Losses=L, CleansSheetPct=CS_percent, PKsAgainst=PKatt_Penalty_Kicks,
         PKgoalsAgainst=PKA_Penalty_Kicks, PKsaves=PKsv_Penalty_Kicks, PKMissedAgainst=PKm_Penalty_Kicks,
         PKSavePct=Save_percent_Penalty_Kicks) %>%
  left_join(spi, by="League") %>%
  mutate_at(vars(-Player, -Squad, -MatchesPlayed, -MatchesStarted, -MinutesPlayed, -Squad, -League,
                 -Position, -YellowCards, -RedCards, -Age, -CleanSheets, -OwnGoals, -concede2,
                 -concede4, -concede6, -concede8, -concede10, -GoalsAgainst,
                 -Wins, -Draws, -Losses, -Season), ~ . *penalty)

temp2 <- intersect(names(df8), names(keepers))
df8 <- df8 %>% select(temp2)

temp <- fb_league_urls(country="ENG", gender="M", season_end_year = 2023, tier="2nd")
temp <- fb_teams_urls(temp)[1:3]

url <- data.frame()
for(i in temp){
stats <- fb_player_urls(i) %>% data.frame()
url <- bind_rows(url, stats)
}
gc()
url <- url$.

#standard <- data.frame()
# for(i in url) {
#   stats <- fb_player_season_stats(i, stat_type = "standard") %>%
#     mutate(Season=as.character(Season))
#   standard <- bind_rows(standard, stats)
# }
gc()
# write.csv(standard, "submodules/standard/Std_promotedPlayers.csv")
standard <- read.csv("submodules/standard/Std_promotedPlayers.csv") %>%
  select(-X) %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII")) %>%
  filter(Season=="2021-2022" | Season=="2022-2023") %>%
  mutate(Season2=ifelse(Season=="2021-2022",2022,2023)) %>%
  mutate(Season=Season2) %>%
  select(-Season2) %>%
  filter(grepl("1.", Comp) | grepl("2.", Comp) | grepl("3.", Comp)) %>%
  mutate(Comp=substr(Comp, start=4, stop = nchar(Comp))) %>%
  rename(League=Comp) %>%
  filter(!grepl("PL", League) & !grepl("Lg", League)) %>%
  select(-LgRank)

# shooting <- data.frame()
# for(i in url) {
#   stats <- fb_player_season_stats(i, stat_type = "shooting")
#   shooting <- bind_rows(shooting, stats)
# }
# gc()
# write.csv(shooting, "submodules/shooting/Sht_promotedPlayers.csv")
shooting <- read.csv("submodules/shooting/Sht_promotedPlayers.csv") %>%
  select(-X) %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII")) %>%
  filter(Season=="2021-2022" | Season=="2022-2023") %>%
  mutate(Season2=ifelse(Season=="2021-2022",2022,2023)) %>%
  mutate(Season=Season2) %>%
  select(-Season2) %>%
  filter(grepl("1.", Comp) | grepl("2.", Comp) | grepl("3.", Comp)) %>%
  mutate(Comp=substr(Comp, start=4, stop = nchar(Comp))) %>%
  rename(League=Comp) %>%
  filter(!grepl("PL", League) & !grepl("Lg", League)) %>%
  select(-LgRank)

# defense <- data.frame()
# for(i in url) {
#   stats <- fb_player_season_stats(i, stat_type = "defense")
#   defense <- bind_rows(defense, stats)
# }
# gc()
# write.csv(defense, "submodules/defense/def_promotedPlayers.csv")
defense <- read.csv("submodules/defense/def_promotedPlayers.csv") %>%
  select(-X) %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII")) %>%
  filter(Season=="2021-2022" | Season=="2022-2023") %>%
  mutate(Season2=ifelse(Season=="2021-2022",2022,2023)) %>%
  mutate(Season=Season2) %>%
  select(-Season2) %>%
  filter(grepl("1.", Comp) | grepl("2.", Comp) | grepl("3.", Comp)) %>%
  mutate(Comp=substr(Comp, start=4, stop = nchar(Comp))) %>%
  rename(League=Comp) %>%
  filter(!grepl("PL", League) & !grepl("Lg", League)) %>%
  select(-LgRank)

# passing <- data.frame()
# for(i in url) {
#   stats <- fb_player_season_stats(i, stat_type = "passing")
#   passing <- bind_rows(passing, stats)
# }
# gc()
# write.csv(passing, "submodules/passing/pass_promotedPlayers.csv")
passing <- read.csv("submodules/passing/pass_promotedPlayers.csv") %>%
  select(-X) %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII")) %>%
  filter(Season=="2021-2022" | Season=="2022-2023") %>%
  mutate(Season2=ifelse(Season=="2021-2022",2022,2023)) %>%
  mutate(Season=Season2) %>%
  select(-Season2) %>%
  filter(grepl("1.", Comp) | grepl("2.", Comp) | grepl("3.", Comp)) %>%
  mutate(Comp=substr(Comp, start=4, stop = nchar(Comp))) %>%
  rename(League=Comp) %>%
  filter(!grepl("PL", League) & !grepl("Lg", League))

# possession <- data.frame()
# for(i in url) {
#   stats <- fb_player_season_stats(i, stat_type = "possession")
#   possession <- bind_rows(possession, stats)
# }
# gc()
# write.csv(possession, "submodules/possession/pos_promotedPlayers.csv")
possession <- read.csv("submodules/possession/pos_promotedPlayers.csv") %>%
  select(-X) %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII")) %>%
  filter(Season=="2021-2022" | Season=="2022-2023") %>%
  mutate(Season2=ifelse(Season=="2021-2022",2022,2023)) %>%
  mutate(Season=Season2) %>%
  select(-Season2) %>%
  filter(grepl("1.", Comp) | grepl("2.", Comp) | grepl("3.", Comp)) %>%
  mutate(Comp=substr(Comp, start=4, stop = nchar(Comp))) %>%
  rename(League=Comp) %>%
  filter(!grepl("PL", League) & !grepl("Lg", League)) %>%
  select(-LgRank)

# gk <- data.frame()
# for(i in url) {
#   stats <- fb_player_season_stats(i, stat_type = "keeper")
#   gk <- bind_rows(gk, stats)
# }
# gc()
# write.csv(gk, "submodules/keepers/gk_promotedPlayers.csv")
gk <- read.csv("submodules/keepers/gk_promotedPlayers.csv") %>%
  select(-X) %>% mutate(player_name=stri_trans_general(player_name, "Latin-ASCII")) %>%
  filter(Season=="2021-2022" | Season=="2022-2023") %>%
  mutate(Season2=ifelse(Season=="2021-2022",2022,2023)) %>%
  mutate(Season=Season2) %>%
  select(-Season2) %>%
  filter(grepl("1.", Comp) | grepl("2.", Comp) | grepl("3.", Comp)) %>%
  mutate(Comp=substr(Comp, start=4, stop = nchar(Comp))) %>%
  rename(League=Comp) %>%
  filter(!grepl("PL", League) & !grepl("Lg", League))

### Game logs
# df3 <- data.frame()
# for(i in url) {
# tryCatch({ stats <- fb_player_match_logs(i, season_end_year = 2022, stat_type = "summary")
# 
# if(!("OG_Performance" %in% colnames(stats))) {
#    stats$OG_Performance <- NA
#   }
# 
# if(!("Result" %in% colnames(stats))) {
#   stats$Result <- NA
#  }
# 
# if(!("Min" %in% colnames(stats))) {
#   stats$Min <- NA
# }
# 
# stats2 <- stats[, c("Player", "Result", "Comp", "Min", "OG_Performance")]
# df3 <- rbind(df3, stats2)}, error = function(e) {
#   cat("Error occured for URL", url, "\n")
#  })
# }
# gc()

# df4 <- data.frame()
# for(i in url) {
#   tryCatch({ stats <- fb_player_match_logs(i, season_end_year = 2023, stat_type = "summary")
# 
#   if(!("OG_Performance" %in% colnames(stats))) {
#     stats$OG_Performance <- NA
#   }
# 
#   if(!("Result" %in% colnames(stats))) {
#     stats$Result <- NA
#   }
# 
#   if(!("Min" %in% colnames(stats))) {
#     stats$Min <- NA
#   }
# 
#   stats2 <- stats[, c("Player", "Result", "Comp", "Min", "OG_Performance")]
#   df4 <- bind_rows(df4, stats2)}, error = function(e) {
#     cat("Error occured for URL", url, "\n")
#   })
# }
# gc()
# 
# df5 <- df3 %>%
#   mutate(Result=substr(Result, nchar(Result), nchar(Result))) %>%
#   mutate(Result=as.numeric(Result)) %>%
#   rename(opp_score=Result, OwnGoals=5) %>%
#   mutate(CleanSheets=ifelse(opp_score==0 & Min > 59, 1, 0)) %>%
#   mutate(concede2=ifelse(opp_score>1,1,0)) %>%
#   mutate(concede4=ifelse(opp_score>3,1,0)) %>%
#   mutate(concede6=ifelse(opp_score>5,1,0)) %>%
#   mutate(concede8=ifelse(opp_score>7,1,0)) %>%
#   mutate(concede10=ifelse(opp_score>9,1,0)) %>%
#   mutate(time_points=ifelse(Min<60,1,0)) %>%
#   mutate(time_points=ifelse(Min>59,2,0)) %>%
#   group_by(Player) %>%
#   summarize(CS=sum(CleanSheets, na.rm=T),
#             own_goals=sum(OwnGoals, na.rm = T),
#             time_points=sum(time_points, na.rm=T),
#             concede2=sum(concede2, na.rm = T),
#             concede4=sum(concede4, na.rm=T),
#             concede6=sum(concede6, na.rm = T),
#             concede8=sum(concede8, na.rm = T),
#             concede10=sum(concede10, na.rm=T)) %>%
#   ungroup() %>%
#   mutate(Player=stri_trans_general(Player, "Latin-ASCII")) %>%
#   mutate(Season=2022)
# 
# 
# df6 <- df4 %>%
#   mutate(Result=substr(Result, nchar(Result), nchar(Result))) %>%
#   mutate(Result=as.numeric(Result)) %>%
#   rename(opp_score=Result, OwnGoals=5) %>%
#   mutate(CleanSheets=ifelse(opp_score==0 & Min > 59, 1, 0)) %>%
#   mutate(concede2=ifelse(opp_score>1,1,0)) %>%
#   mutate(concede4=ifelse(opp_score>3,1,0)) %>%
#   mutate(concede6=ifelse(opp_score>5,1,0)) %>%
#   mutate(concede8=ifelse(opp_score>7,1,0)) %>%
#   mutate(concede10=ifelse(opp_score>9,1,0)) %>%
#   mutate(time_points=ifelse(Min<60,1,0)) %>%
#   mutate(time_points=ifelse(Min>59,2,0)) %>%
#   group_by(Player) %>%
#   summarize(CS=sum(CleanSheets, na.rm=T),
#             own_goals=sum(OwnGoals, na.rm = T),
#             time_points=sum(time_points, na.rm=T),
#             concede2=sum(concede2, na.rm = T),
#             concede4=sum(concede4, na.rm=T),
#             concede6=sum(concede6, na.rm = T),
#             concede8=sum(concede8, na.rm = T),
#             concede10=sum(concede10, na.rm=T)) %>%
#   ungroup() %>%
#   mutate(Player=stri_trans_general(Player, "Latin-ASCII")) %>%
#   mutate(Season=2023)
# 
# match_logs <- bind_rows(df5, df6)
#write.csv(match_logs, "submodules/clean sheets/match_logs_promotedPlayers.csv")
match_logs <- read.csv("submodules/clean sheets/match_logs_promotedPlayers.csv")


# Join all stats
## outfield
temp <- c("player_name", "player_url", "Season", "Age", "Squad", "Country", "League")
temp2 <- map %>% rename(player_name=1, Position=4) %>%
  mutate(Position=ifelse(grepl("Forward", Position),"FWD",Position)) %>%
  mutate(Position=ifelse(grepl("Striker", Position),"FWD",Position)) %>%
  mutate(Position=ifelse(grepl("Attack", Position),"FWD",Position)) %>%
  mutate(Position=ifelse(grepl("attack", Position),"FWD",Position)) %>%
  mutate(Position=ifelse(grepl("Midfield", Position),"MID",Position)) %>%
  mutate(Position=ifelse(grepl("Winger", Position),"MID",Position)) %>%
  mutate(Position=ifelse(grepl("midfield", Position),"MID",Position)) %>%
  mutate(Position=ifelse(grepl("Back", Position),"DEF",Position)) %>%
  mutate(Position=ifelse(grepl("Defence", Position),"DEF",Position)) %>%
  mutate(Position=ifelse(grepl("Defender", Position),"DEF",Position)) %>%
  mutate(Position=ifelse(grepl("Goalkeeper", Position),"GKP",Position)) %>%
  select(player_name, Position)
df10 <- standard %>% 
  left_join(temp2, by="player_name") %>%
  left_join(shooting, by=temp) %>%
  select(-contains(".y")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$","",.))) %>%
  left_join(defense, by=temp) %>%
  select(-contains(".y")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$","",.))) %>%
  left_join(passing, by=temp) %>%
  select(-contains(".y")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$","",.))) %>%
  left_join(possession, by=temp) %>%
  select(-contains(".y")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$","",.))) %>%
  left_join(match_logs, by=c("player_name"="Player", "Season"="Season")) %>%
  select(-contains(".y")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$","",.))) %>%
  mutate(League=ifelse(League=="First Division A", "Pro League A", League)) %>%
  distinct(player_name, Season, Squad, League, .keep_all = T)
df11 <- df10 %>% filter(Position=="GKP") %>%
  left_join(gk, by=temp) %>%
  select(-contains(".y")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = funs(sub("[.]x$","",.)))
df12 <- df10 %>% filter(Position!="GKP")

###### Calculate more missing spi values, fill in the gaps using rankings from here:
###### https://www.teamform.com/en/league-ranking/world, using most similar league in ranking
spi <- read.csv("soccer-spi/spi_global_rankings.csv") %>% group_by(league) %>%
  summarize(spi=mean(spi)) %>% mutate(league=stri_trans_general(league, "Latin-ASCII")) %>%
  mutate(league2=NA) %>%
  mutate(league2=ifelse(league=="German Bundesliga", "Bundesliga", league2)) %>%
  mutate(league2=ifelse(league=="Barclays Premier League", "Premier League", league2)) %>%
  mutate(league2=ifelse(league=="French Ligue 1", "Ligue 1", league2)) %>%
  mutate(league2=ifelse(league=="English League Championship", "Championship", league2)) %>%
  mutate(league2=ifelse(league=="Scottish Premiership", "Premiership", league2)) %>%
  mutate(league2=ifelse(league=="English League One", "League One", league2)) %>%
  mutate(league2=ifelse(league=="Turkish Turkcell Super Lig", "Süper Lig", league2)) %>%
  mutate(league2=ifelse(league=="Belgian Jupiler League", "Pro League A", league2)) %>%
  mutate(league2=ifelse(league=="German 2. Bundesliga", "2. Bundesliga", league2)) %>%
  select(league2, spi) %>% rename(League=1) %>% arrange(-spi) %>% filter(!is.na(League))



###### First Division B, using League One: 23.6/67.4=X/57.3
temp <- data.frame(League="First Division B", spi=23.6*57.3/67.4) 
spi <- rbind(spi, temp)

## Factor is taken as a percentage of SPI, applied broadly to statistics to estimate minutes, goals, etc when translating stats from other leagues
spi <- spi %>% mutate(penalty=spi/spi[1]) %>% select(League, penalty)


df12 <- df12 %>% select(-player_url, -Country, -time_points, -Mins_Per_90, -PrgP, -X) %>%
  rename(Player=player_name, MatchesPlayed=MP, MatchesStarted=Starts_Time,
         MinutesPlayed=Min_Time, MinutesPer90=Mins_Per_90_Time, Goals=Gls,
         Assists=Ast, GoalContributions=G.A, GoalsMinusPKs=G_minus_PK,
         Pkgoals=PK, Pkattempts=PKatt, YellowCards=CrdY, RedCards=CrdR,
         GoalsPer90=Gls_Per_Minutes, AssistsPer90=Ast_Per_Minutes, 
         GoalsContributionsPer90=G.A_Per_Minutes, GoalsNoPKsPer90=G_minus_PK_Per_Minutes,
         GoalsStd=Gls_Standard, Shots=Sh_Standard,
         ShotsOnTarget=SoT_Standard, PctShotsOnTarget=SoT_percent_Standard,
         GoalsPerShot=G_per_Sh_Standard, GoalsPerShotOnTarget=G_per_SoT_Standard,
         AvgShotDistance=Dist_Standard, PKstd=PK_Standard, Tackles=Tkl_Tackles,
         TacklePossession=TklW_Tackles, DribTackles=Tkl_Challenges, Def3rdTackles=Def.3rd_Tackles,
         Mid3rdTackles=Mid.3rd_Tackles, Atk3rdTackles=Att.3rd_Tackles, TackleAttempt=Att_Challenges,
         TacklePct=Tkl_percent_Challenges, LostTackle=Lost_Challenges, TotalBlocks=Blocks_Blocks,
         ShotBlocks=Sh_Blocks, PassBlocks=Pass_Blocks, Interceptions=Int,
         `Tackle&Interception`=Tkl.Int, Clearances=Clr, ErrorsToShots=Err, CompletePasses=Cmp_Total,
         PassAttempt=Att_Total, PassCompletePct=Cmp_percent_Total, TotalPassDistance=TotDist_Total,
         ProgressivePassDistance=PrgDist_Total, CompleteShortPass=Cmp_Short, ShortPassAttempt=Att_Short,
         ShortPassCompletePct=Cmp_percent_Short, CompleteMedPass=Cmp_Medium, MedPassAttempt=Att_Medium,
         MedPassCompletePct=Cmp_percent_Medium, CompleteLongPass=Cmp_Long, LongPassAttempt=Att_Long,
         LongPassCompletePct=Cmp_percent_Long, KeyPasses=KP, PassFinal3rd=Final_Third,
         PassPKArea=PPA, CrossPKArea=CrsPA, NumberTouches=Touches_Touches,
         DefPKTouches=Def.Pen_Touches, Def3rdTouches=Def.3rd_Touches, Mid3rdTouches=Mid.3rd_Touches,
         Atk3rdTouches=Att.3rd_Touches, AtkPKTouches=Att.Pen_Touches, LiveBallTouches=Live_Touches,
         AttackingTakeOn=Att_Take_Ons, SuccessfulTakeOn=Succ_Take_Ons, TakeOnPct=Succ_percent_Take_Ons,
         TimesTackled=Tkld_Take_Ons, TimesTackledPct=Tkld_percent_Take_Ons, Carries=Carries_Carries,
         CarryDistance=TotDist_Carries, ProgressiveCarryDistance=PrgDist_Carries,
         Atk3rdCarries=Final_Third_Carries, AtkPKCarries=CPA_Carries, Miscontrols=Mis_Carries,
         Dispossed=Dis_Carries, CleanSheets=CS, OwnGoals=own_goals) %>%
  left_join(spi, by="League") %>%
  mutate_at(vars(-Player, -Squad, -MatchesPlayed, -MatchesStarted, -MinutesPlayed, -Squad, -League,
                 -Position, -YellowCards, -RedCards, -Age, -CleanSheets, -OwnGoals, -concede2,
                 -concede4, -concede6, -concede8, -concede10, -Season), ~ . *penalty)

temp <- intersect(names(df12), names(outfield))
df12 <- df12 %>% select(temp)

df11 <- df11 %>% select(-player_url, -Country, -time_points, -Mins_Per_90, -PrgP, -X) %>%
  rename(Player=player_name, MatchesPlayed=MP, MatchesStarted=Starts_Time,
         MinutesPlayed=Min_Time, MinutesPer90=Mins_Per_90_Time, Goals=Gls,
         Assists=Ast, GoalContributions=G.A, GoalsMinusPKs=G_minus_PK,
         Pkgoals=PK, Pkattempts=PKatt, YellowCards=CrdY, RedCards=CrdR,
         GoalsPer90=Gls_Per_Minutes, AssistsPer90=Ast_Per_Minutes, 
         GoalsContributionsPer90=G.A_Per_Minutes, GoalsNoPKsPer90=G_minus_PK_Per_Minutes,
         GoalsStd=Gls_Standard, Shots=Sh_Standard,
         ShotsOnTarget=SoT_Standard, PctShotsOnTarget=SoT_percent_Standard,
         GoalsPerShot=G_per_Sh_Standard, GoalsPerShotOnTarget=G_per_SoT_Standard,
         AvgShotDistance=Dist_Standard, PKstd=PK_Standard, Tackles=Tkl_Tackles,
         TacklePossession=TklW_Tackles, DribTackles=Tkl_Challenges, Def3rdTackles=Def.3rd_Tackles,
         Mid3rdTackles=Mid.3rd_Tackles, Atk3rdTackles=Att.3rd_Tackles, TackleAttempt=Att_Challenges,
         TacklePct=Tkl_percent_Challenges, LostTackle=Lost_Challenges, TotalBlocks=Blocks_Blocks,
         ShotBlocks=Sh_Blocks, PassBlocks=Pass_Blocks, Interceptions=Int,
         `Tackle&Interception`=Tkl.Int, Clearances=Clr, ErrorsToShots=Err, CompletePasses=Cmp_Total,
         PassAttempt=Att_Total, PassCompletePct=Cmp_percent_Total, TotalPassDistance=TotDist_Total,
         ProgressivePassDistance=PrgDist_Total, CompleteShortPass=Cmp_Short, ShortPassAttempt=Att_Short,
         ShortPassCompletePct=Cmp_percent_Short, CompleteMedPass=Cmp_Medium, MedPassAttempt=Att_Medium,
         MedPassCompletePct=Cmp_percent_Medium, CompleteLongPass=Cmp_Long, LongPassAttempt=Att_Long,
         LongPassCompletePct=Cmp_percent_Long, KeyPasses=KP, PassFinal3rd=Final_Third,
         PassPKArea=PPA, CrossPKArea=CrsPA, NumberTouches=Touches_Touches,
         DefPKTouches=Def.Pen_Touches, Def3rdTouches=Def.3rd_Touches, Mid3rdTouches=Mid.3rd_Touches,
         Atk3rdTouches=Att.3rd_Touches, AtkPKTouches=Att.Pen_Touches, LiveBallTouches=Live_Touches,
         AttackingTakeOn=Att_Take_Ons, SuccessfulTakeOn=Succ_Take_Ons, TakeOnPct=Succ_percent_Take_Ons,
         TimesTackled=Tkld_Take_Ons, TimesTackledPct=Tkld_percent_Take_Ons, Carries=Carries_Carries,
         CarryDistance=TotDist_Carries, ProgressiveCarryDistance=PrgDist_Carries,
         Atk3rdCarries=Final_Third_Carries, AtkPKCarries=CPA_Carries, Miscontrols=Mis_Carries,
         Dispossed=Dis_Carries, CleanSheets=CS, OwnGoals=own_goals, GoalsAgainst=GA,
         GoalsAgainstPer90=GA90, ShotsOnTargetAgainst=SoTA, SavePercent=Save_percent,
         Wins=W, Draws=D, Losses=L, CleansSheetPct=CS_percent, PKsAgainst=PKatt_Penalty_Kicks,
         PKgoalsAgainst=PKA_Penalty_Kicks, PKsaves=PKsv_Penalty_Kicks, PKMissedAgainst=PKm_Penalty_Kicks,
         PKSavePct=Save_percent_Penalty_Kicks) %>%
  left_join(spi, by="League") %>%
  mutate_at(vars(-Player, -Squad, -MatchesPlayed, -MatchesStarted, -MinutesPlayed, -Squad, -League,
                 -Position, -YellowCards, -RedCards, -Age, -CleanSheets, -OwnGoals, -concede2,
                 -concede4, -concede6, -concede8, -concede10, -GoalsAgainst,
                 -Wins, -Draws, -Losses, -Season), ~ . *penalty)

temp2 <- intersect(names(df11), names(keepers))
df11 <- df11 %>% select(temp2)

temp2 <- data.frame()
for(i in teams){
  stats<- tm_squad_stats(i)
  temp2<- bind_rows(temp2, stats)
}
temp <- map %>% filter(UrlTmarkt %in% temp2$player_url) %>% select(PlayerFBref) %>%
  mutate(PlayerFBref=stri_trans_general(PlayerFBref, "Latin-ASCII"))
temp3 <- teams %>% data.frame() %>% rename(url=1) %>% 
  mutate(url=substring(url, 1, nchar(url)-1)) %>%
  mutate(url=paste0(url, "2"))

## Develop a squad ranking based on minutes played
temp4 <- data.frame()
for(i in teams){
  stats <- tm_squad_stats(i)
  temp4 <- rbind(temp4, stats)
}

temp <- map %>% filter(UrlTmarkt %in% temp4$player_url) %>% 
  select(PlayerFBref, UrlTmarkt, TmPos) %>%
  mutate(PlayerFBref=stri_trans_general(PlayerFBref, "Latin-ASCII")) %>%
  mutate(TmPos=ifelse(grepl("Forward", TmPos),"FWD",TmPos)) %>%
  mutate(TmPos=ifelse(grepl("Striker", TmPos),"FWD",TmPos)) %>%
  mutate(TmPos=ifelse(TmPos=="attack","FWD",TmPos)) %>%
  mutate(TmPos=ifelse(grepl("Midfield", TmPos),"MID",TmPos)) %>%
  mutate(TmPos=ifelse(grepl("Winger", TmPos),"MID",TmPos)) %>%
  mutate(TmPos=ifelse(grepl("midfield", TmPos),"MID",TmPos)) %>%
  mutate(TmPos=ifelse(grepl("Back", TmPos),"DEF",TmPos)) %>%
  mutate(TmPos=ifelse(grepl("Defence", TmPos),"DEF",TmPos)) %>%
  mutate(TmPos=ifelse(grepl("Defender", TmPos),"DEF",TmPos)) %>%
  mutate(TmPos=ifelse(grepl("Goalkeeper", TmPos),"GKP",TmPos))
temp5 <- temp4 %>% select(team_name, player_url)
temp2 <- temp %>% left_join(temp4, by=c("UrlTmarkt"="player_url")) %>% 
  select(PlayerFBref, team_name)
temp <- temp %>% select(PlayerFBref, TmPos)
temp6 <- data.frame(PlayerFBref="Gabriel Dos Santos", TmPos="DEF")
temp <- rbind(temp, temp6)
temp6 <- data.frame(PlayerFBref="Gabriel Dos Santos", team_name="Arsenal FC")
temp2 <- rbind(temp2, temp6)

### Assign weight of 3 to 2023 statistics
#### More complex calculation - if mintues played were higher in 2022, re-assign weight to 2022
#### Varied the weight for 2022; don't want to be be capturing too much change in play that may be due decline
df <- outfield %>% filter(!outfield$Player %in% departures$PlayerFBref) %>%
  filter(!Player %in% df9$Player) %>% filter(!Player %in% df12$Player) %>%
  bind_rows(df9) %>% bind_rows(df12) %>% filter(Player %in% temp$PlayerFBref) %>%
  left_join(temp, by=c("Player"="PlayerFBref")) %>%
  mutate(Position=ifelse(Position!=TmPos, TmPos, Position)) %>%
  distinct(Player, Season, .keep_all = T) %>%
  select(-Nation, -Age, -Born, -Points, -Bonus, -Squad, -TmPos) %>%
  arrange(Player) %>%
  group_by(Player) %>%
  mutate(higher_year=ifelse(
    any(Season==2022, na.rm=T) && any(Season==2023, na.rm=T),
    ifelse(MinutesPlayed[Season==2022] > MinutesPlayed[Season==2023], 2022, 2023), NA)) %>%
  ungroup() %>%
  mutate(higher_year=ifelse(is.na(higher_year) & Season==2023, 2023, higher_year)) %>%
  mutate(higher_year=ifelse(is.na(higher_year) & Season==2022, 2022, higher_year)) %>%
  group_by(Player, Position) %>%
  summarize(across(
    where(is.numeric), ~ifelse(higher_year==2022,
      ifelse(any(Season==2022, na.rm=T) && any(Season==2023, na.rm=T),
                           (2*.[Season==2022]+.[Season==2023])/3,
                           .[Season==2022]
                   ),
      ifelse(any(Season==2022, na.rm=T) && any(Season==2023, na.rm=T),
             (.[Season==2022]+3*.[Season==2023])/4,
             .[Season==2023])
      ))) %>%
  ungroup() %>%
  mutate(MinutesPlayed=ifelse(MatchesPlayed > 38, MinutesPer90*90, MinutesPlayed)) %>%
  mutate(MatchesStarted=ifelse(MatchesStarted > 38, (MatchesStarted/46)*38, MatchesStarted)) %>%
  mutate(MatchesPlayed=ifelse(MatchesPlayed > 38, (MatchesPlayed/46)*38, MatchesPlayed)) %>%
  left_join(temp2, by=c("Player"="PlayerFBref"))
  

# New arrivals have their statistics reduced
temp5 <- df %>% filter(is.na(X))
quantile(temp5$MinutesPlayed, na.rm=T)

# Organize additional penalties to stats and squad rank for new players by percentiles of minutes played:
## If in top quantile, 90% applied to all stats
## If in second quantile, 75% applied to all stats
## If in third quantile, 50% applied to all stats
## If in bottom quantile, 30% applied to all stats

batch_rank <- function(x) {
  ranked <- rank(x)
  batch <- 1+(ranked-1) %/% 11
  return(batch)
}

temp5 <- df %>% filter(is.na(X)) %>%
  mutate(percentile=round(percent_rank(MinutesPlayed),2)) %>%
  mutate(factor=ifelse(percentile<0.26,0.3,NA)) %>%
  mutate(factor=ifelse(percentile>0.25,0.5,factor)) %>%
  mutate(factor=ifelse(percentile>0.5,0.75,factor)) %>%
  mutate(factor=ifelse(percentile>0.75,0.9,factor)) %>%
  select(-percentile) %>%
  mutate_at(vars(-Player, -team_name, -Position, -X), ~ . *factor) %>%
  select(-factor) %>% distinct(Player, .keep_all = T)

df3 <- df %>% distinct(Player, Position, .keep_all = T) %>%
  filter(!Player %in% temp5$Player) %>%
  bind_rows(temp5) %>%
  group_by(team_name, Position) %>%
  mutate(SquadRank=rank(-MinutesPlayed)) %>%
  ungroup() %>%
  select(-X, -Season) %>%
  mutate_at(vars(-Player, -Position, -team_name), as.numeric) %>%
  mutate(across(everything(), ~ replace(., is.nan(.), NA)))

df2 <- keepers %>% filter(!Player %in% departures$PlayerFBref) %>%
  filter(!Player %in% df8$Player) %>% filter(!Player %in% df11$Player) %>%
  bind_rows(df8) %>% bind_rows(df11) %>% filter(Player %in% temp$PlayerFBref) %>%
  distinct(Player, Season, .keep_all = T) %>% 
  select(-Nation, -Age, -Born, -Points, -Bonus, -Squad) %>%
  arrange(Player) %>%
  group_by(Player) %>%
  mutate(higher_year=ifelse(
    any(Season==2022, na.rm=T) && any(Season==2023, na.rm=T),
    ifelse(MinutesPlayed[Season==2022] > MinutesPlayed[Season==2023], 2022, 2023), NA)) %>%
  ungroup() %>%
  mutate(higher_year=ifelse(is.na(higher_year) & Season==2023, 2023, higher_year)) %>%
  mutate(higher_year=ifelse(is.na(higher_year) & Season==2022, 2022, higher_year)) %>%
  group_by(Player, Position) %>%
  summarize(across(where(is.numeric),
                   ~ifelse(higher_year==2022,
                           ifelse(any(Season==2022, na.rm=T) && any(Season==2023, na.rm=T),
                                  (2*.[Season==2022]+.[Season==2023])/3,
                                  .[Season==2022]
                               ),
                               ifelse(any(Season==2022, na.rm=T) && any(Season==2023, na.rm=T),
                                      (.[Season==2022]+3*.[Season==2023])/4,
                                      .[Season==2023])
    ))) %>%
  ungroup() %>%
  mutate(MinutesPlayed=ifelse(MatchesPlayed > 38, MinutesPer90*90, MinutesPlayed)) %>%
  mutate(MatchesStarted=ifelse(MatchesStarted > 38, (MatchesStarted/46)*38, MatchesStarted)) %>%
  mutate(MatchesPlayed=ifelse(MatchesPlayed > 38, (MatchesPlayed/46)*38, MatchesPlayed)) %>%
  left_join(temp2, by=c("Player"="PlayerFBref"))

temp5 <- df2 %>% filter(is.na(X)) %>%
  mutate(percentile=round(percent_rank(MinutesPlayed),2)) %>%
  mutate(factor=ifelse(percentile<0.26,0.3,NA)) %>%
  mutate(factor=ifelse(percentile>0.25,0.5,factor)) %>%
  mutate(factor=ifelse(percentile>0.5,0.75,factor)) %>%
  mutate(factor=ifelse(percentile>0.75,0.9,factor)) %>%
  select(-percentile) %>%
  mutate_at(vars(-Player, -team_name, -Position, -X), ~ . *factor) %>%
  select(-factor) 

df4 <- df2 %>% filter(!Player %in% temp5$Player) %>%
  bind_rows(temp5) %>%
  group_by(team_name) %>%
  mutate(SquadRank=rank(-MinutesPlayed)) %>%
  select(-X, -Season) %>%
  distinct(Player, Position, .keep_all = T)


## Get injury data
temp <- map %>% filter(PlayerFBref %in% df3$Player) %>% select(UrlTmarkt)
# injuries <- data.frame()
# for(i in temp$UrlTmarkt) {
#   stats <- tm_player_injury_history(player_urls = i)
#   injuries <- bind_rows(injuries, stats)
# }
# write.csv(injuries, "submodules/injuries/injuries_current.csv")
injuries <- read.csv("submodules/injuries/injuries_current.csv") %>%
  mutate(Season=ifelse(season_injured=="21/22", 2022, NA)) %>%
  mutate(Season=ifelse(season_injured=="22/23", 2023, Season)) %>%
  filter(!is.na(Season)) %>%
  group_by(player_url) %>%
  summarize(InjuryGamesMissed=mean(games_missed, na.rm=T)) %>%
  ungroup()

temp <- map %>% 
  mutate(PlayerFBref=stri_trans_general(PlayerFBref, "Latin-ASCII")) %>%
  filter(PlayerFBref %in% df3$Player) %>%
  select(PlayerFBref, UrlTmarkt) %>%
  left_join(injuries, by=c("UrlTmarkt"="player_url")) %>%
  select(-UrlTmarkt) %>%
  distinct(PlayerFBref, .keep_all = T)


df <- df3 %>% left_join(temp, by=c("Player"="PlayerFBref"))

temp <- map %>%
  mutate(PlayerFBref=stri_trans_general(PlayerFBref, "Latin-ASCII")) %>%
  filter(PlayerFBref %in% df4$Player) %>%
  select(PlayerFBref, UrlTmarkt) %>%
  left_join(injuries, by=c("UrlTmarkt"="player_url")) %>%
  select(-UrlTmarkt) %>%
  distinct(PlayerFBref, .keep_all = T)

df2 <- df4 %>% left_join(temp, by=c("Player"="PlayerFBref"))

## Save for use in the model
write.csv(df, "outfield_new_players.csv")
write.csv(df2, "keepers_new_players.csv")

