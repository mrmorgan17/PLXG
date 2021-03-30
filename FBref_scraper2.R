library(rvest)
library(tidyverse)

pl_season_list <- read_html('https://fbref.com/en/comps/9/history/Premier-League-Seasons')

pl_season_df <- pl_season_list %>%
  html_table()

pl_season_href <- pl_season_list %>%
  html_nodes("tr") %>%
  html_nodes("th") %>% 
  html_nodes("a") %>%
  html_attr("href") %>% 
  data.frame() 

season_tag <- str_extract_all(pl_season_href[,1], '[:graph:]+(?=\\-Premier)') %>% 
  str_split(., '\\/') %>% 
  unlist() %>% 
  .[seq(6, length(.), 6)]

season <- str_extract_all(pl_season_href[,1], '[:graph:]+(?=\\-Premier)') %>% 
  str_split(., '\\/') %>% 
  unlist() %>% 
  .[seq(7, length(.), 6)]

pl_season_href <- data.frame(season = season, season_tag = season_tag)

pl_team_list <- list()
pl_team_df <- list()
pl_team_href <- list()
team_tag <- list()
team <- list()
pl_teams <- list()

for (i in 1:3) {
  pl_team_list[[i]] <- read_html(paste0('https://fbref.com/en/comps/9/', season_tag[i], '/', season[i], '-Premier-League-Stats'))
  
  pl_team_df[[i]] <- pl_team_list[[i]] %>% 
    html_table()
  
  pl_team_href[[i]] <- pl_team_list[[i]] %>% 
    html_nodes("td") %>%
    html_nodes("a") %>%
    html_attr("href") %>% 
    data.frame() %>% 
    filter(str_detect(., 'squads') == TRUE) %>% 
    unique()
  
  team_tag[[i]] <- do.call(rbind, str_extract_all(pl_team_href[[i]][,1], '(?<=\\/)[:alnum:]{8}(?=\\/)'))
  
  team[[i]] <- str_extract_all(pl_team_href[[i]][,1], '[:graph:]+(?=\\-Stats)') %>% 
    str_split(., '\\/') %>% 
    unlist() %>% 
    .[seq(6, length(.), 6)]
  
  pl_teams[[i]] <- data.frame(team = team[[i]], team_tag = team_tag[[i]], season = season[i], season_tag = season_tag[i])
}

pl_teams <- do.call(rbind, pl_teams)

table_types = c('shooting', 'keeper', 'passing', 'passing_types', 'gca', 'defense', 'possession')

##############
## Shooting ##
##############

Pl_team_shooting <- list()

for (i in 1:nrow(pl_teams)) {
  Pl_team_shooting[[i]] <- xml2::read_html(paste0('https://fbref.com/en/squads/', pl_teams$team_tag[i], '/', pl_teams$season[i], '/matchlogs/s', pl_teams$season_tag[i], '/', table_types[1], '/', pl_teams$team[i], '-Match-Logs-Premier-League')) %>% 
    rvest::html_table() %>%
    data.frame()
  
  names(Pl_team_shooting[[i]]) <- Pl_team_shooting[[i]][1,]
  
  Pl_team_shooting[[i]]$Team <- pl_teams$team[i]
  
  Pl_team_shooting[[i]] <- Pl_team_shooting[[i]][seq(2, nrow(Pl_team_shooting[[i]]) - 2, by = 2),] %>% select(Goals = GF, Team, Date, Round, Venue, Opponent, Sh, SoT, Dist, FK, PKatt)
}

Pl_team_shooting_df <- do.call(rbind, Pl_team_shooting)

############
## Keeper ##
############

Pl_team_keeper <- list()

for (i in 1:nrow(pl_teams)) {
  Pl_team_keeper[[i]] <- xml2::read_html(paste0('https://fbref.com/en/squads/', pl_teams$team_tag[i], '/', pl_teams$season[i], '/matchlogs/s', pl_teams$season_tag[i], '/', table_types[2], '/', pl_teams$team[i], '-Match-Logs-Premier-League')) %>% 
    rvest::html_table() %>%
    data.frame()
  
  Pl_team_keeper[[i]] <- Pl_team_keeper[[i]][seq(3, nrow(Pl_team_keeper[[i]]) - 2, by = 2),] %>% select(12, 21, 22, 24, 25, 27, 28, 30:32, 34, 35)
  
  names(Pl_team_keeper[[i]]) <- c('Opp_Saves', 'Opp_Launched_Cmp', 'Opp_Launched_Att', 'Opp_Passes_Att', 'Opp_Thr_Att', 'Opp_Passes_AvgLen', 'Opp_GK_Att', 'Opp_GK_AvgLen', 'Crosses_Att', 'Opp_Crosses_Stp', 'Opp_#OPA', 'Opp_AvgDist')
}

Pl_team_keeper_df <- do.call(rbind, Pl_team_keeper)

#############
## Passing ##
#############

Pl_team_passing <- list()

for (i in 1:nrow(pl_teams)) {
  Pl_team_passing[[i]] <- xml2::read_html(paste0('https://fbref.com/en/squads/', pl_teams$team_tag[i], '/', pl_teams$season[i], '/matchlogs/s', pl_teams$season_tag[i], '/', table_types[3], '/', pl_teams$team[i], '-Match-Logs-Premier-League')) %>% 
    rvest::html_table() %>%
    data.frame()
  
  Pl_team_passing[[i]] <- Pl_team_passing[[i]][seq(2, nrow(Pl_team_passing[[i]]) - 2, by = 2),] %>% select(13:16, 18, 19, 21, 22, 26:30)
  
  names(Pl_team_passing[[i]]) <- c('TotDist', 'ProgDist', 'Short_Cmp', 'Short_Att', 'Med_Cmp', 'Med_Att', 'Long_Cmp', 'Long_Att', 'KP', '1/3', 'PPA', 'CrsPA', 'Prog')
}

Pl_team_passing_df <- do.call(rbind, Pl_team_passing)

###################
## Passing Types ##
###################

Pl_team_passing_types <- list()

for (i in 1:nrow(pl_teams)) {
  Pl_team_passing_types[[i]] <- xml2::read_html(paste0('https://fbref.com/en/squads/', pl_teams$team_tag[i], '/', pl_teams$season[i], '/matchlogs/s', pl_teams$season_tag[i], '/', table_types[4], '/', pl_teams$team[i], '-Match-Logs-Premier-League')) %>% 
    rvest::html_table() %>%
    data.frame()
  
  names(Pl_team_passing_types[[i]]) <- c(Pl_team_passing_types[[i]][1,1:12], 'Pass_Att_FK', Pl_team_passing_types[[i]][1,14:18], 'CK_In', 'CK_Out', 'CK_Str', Pl_team_passing_types[[i]][1, 22:34])
  
  Pl_team_passing_types[[i]] <- Pl_team_passing_types[[i]][seq(2, nrow(Pl_team_passing_types[[i]]) - 2, by = 2),] %>% select(11:29, 31:32)
}

Pl_team_passing_types_df <- do.call(rbind, Pl_team_passing_types)

#################################
## GCA (Goal Creating Actions) ##
#################################

Pl_team_gca <- list()

for (i in 1:nrow(pl_teams)) {
  Pl_team_gca[[i]] <- xml2::read_html(paste0('https://fbref.com/en/squads/', pl_teams$team_tag[i], '/', pl_teams$season[i], '/matchlogs/s', pl_teams$season_tag[i], '/', table_types[5], '/', pl_teams$team[i], '-Match-Logs-Premier-League')) %>% 
    rvest::html_table() %>%
    data.frame()
  
  Pl_team_gca[[i]] <- Pl_team_gca[[i]][seq(2, nrow(Pl_team_gca[[i]]) - 2, by = 2),] %>% select(10:16)
  
  names(Pl_team_gca[[i]]) <- c('SCA_Total', 'SCA_PassLive', 'SCA_PassDead', 'SCA_Drib', 'SCA_Sh', 'SCA_Fld', 'SCA_Def')
}

Pl_team_gca_df <- do.call(rbind, Pl_team_gca)

#############
## Defense ##
#############

Pl_team_defense <- list()

for (i in 1:nrow(pl_teams)) {
  Pl_team_defense[[i]] <- xml2::read_html(paste0('https://fbref.com/en/squads/', pl_teams$team_tag[i], '/', pl_teams$season[i], '/matchlogs/s', pl_teams$season_tag[i], '/', table_types[6], '/', pl_teams$team[i], '-Match-Logs-Premier-League')) %>% 
    rvest::html_table() %>%
    data.frame()
  
  Pl_team_defense[[i]] <- Pl_team_defense[[i]][seq(3, nrow(Pl_team_defense[[i]]) - 2, by = 2),] %>% select(10:15, 18, 20, 22:24, 26:29, 31:32)
  
  names(Pl_team_defense[[i]]) <- c('Players_Tkl', 'TklW', 'Tkl_Def_3rd', 'Tkl_Mid_3rd', 'Tkl_Att_3rd',
                                   'Dribblers_Tkl', 'Dribblers_Past',
                                   'Press_Succ', 'Press_Def_3rd', 'Press_Mid_3rd', 'Press_Att_3rd',
                                   'Blocks_Sh', 'Blocks_ShSv', 'Blocks_Pass',
                                   'Int', 'Clr', 'Err')
}

Pl_team_defense_df <- do.call(rbind, Pl_team_defense)

################
## Possession ##
################

Pl_team_possession <- list()

for (i in 1:nrow(pl_teams)) {
  Pl_team_possession[[i]] <- xml2::read_html(paste0('https://fbref.com/en/squads/', pl_teams$team_tag[i], '/', pl_teams$season[i], '/matchlogs/s', pl_teams$season_tag[i], '/', table_types[7], '/', pl_teams$team[i], '-Match-Logs-Premier-League')) %>% 
    rvest::html_table() %>%
    data.frame()
  
  Pl_team_possession[[i]] <- Pl_team_possession[[i]][seq(2, nrow(Pl_team_possession[[i]]) - 2, by = 2),] %>% select(10:18, 20:31, 33)
  
  names(Pl_team_possession[[i]]) <- c('Touches_Total', 'Touches_Def_Pen', 'Touches_Def_3rd', 'Touches_Mid_3rd', 'Touches_Att_3rd', 'Touches_Att_Pen', 'Touches_Live',
                                      'Dribbles_Succ', 'Dribbles_Att', 'Dribbles_#Pl', 'Dribbles_Megs',
                                      'Carries', 'Carries_TotDist', 'Carries_ProgDist', 'Carries_Prog', 'Carries_1/3', 'Carries_CPA', 'Carries_Mis', 'Carries_Dis',
                                      'Rec_Target', 'Rec_Succ', 'Rec_Prog')
}

Pl_team_possession_df <- do.call(rbind, Pl_team_possession)

Pl_team_match_data <- cbind(Pl_team_shooting_df, 
                            Pl_team_keeper_df, 
                            Pl_team_passing_df,
                            Pl_team_passing_types_df,
                            Pl_team_gca_df,
                            Pl_team_defense_df,
                            Pl_team_possession_df)

write.csv(Pl_team_match_data, '~/Documents/Stat 495R/Pl_team_match_data.csv', row.names = FALSE)
