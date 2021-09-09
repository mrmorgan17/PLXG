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

# Remove any rows that don't have a year
# pl_season_href <- subset(pl_season_href, str_detect(pl_season_href[,1], '20|19'))

pl_season_href <- data.frame(pl_season_href[3:30,1])

# Extract the unique identifier for each season
season_tag <- str_extract_all(pl_season_href[,1], '[:graph:]+(?=\\-Premier)') %>% 
  str_split(., '\\/') %>% 
  unlist() %>% 
  .[seq(5, length(.), 6)]

# Extract the years for which the season ran (ex: 2019-2020)
season <- str_extract_all(pl_season_href[,1], '[:graph:]+(?=\\-Premier)') %>% 
  str_split(., '\\/') %>% 
  unlist() %>% 
  .[seq(6, length(.), 6)]

# Create a data frame with each season and its unique identifier
pl_season_href <- data.frame(season = season, season_tag = season_tag)

pl_season_href <- rbind(data.frame(season = "", season_tag = ""), pl_season_href)

pl_team_list <- list()
pl_team_df <- list()
pl_team_href <- list()
team_tag <- list()
team <- list()
pl_teams <- list()

# pl_team_list <- read_html(paste0('https://fbref.com/en/comps/9/', pl_season_href$season_tag[2], '/', pl_season_href$season[2], '-Premier-League-Stats'))
# 
# pl_team_df <- pl_team_list %>% 
#   html_table()
# 
# pl_team_href <- pl_team_list %>% 
#   html_nodes("td") %>%
#   html_nodes("a") %>%
#   html_attr("href") %>% 
#   data.frame() %>% 
#   filter(str_detect(., 'squads') == TRUE) %>% 
#   unique()
# 
# team_tag <- do.call(rbind, str_extract_all(pl_team_href[,1], '(?<=\\/)[:alnum:]{8}(?=\\/)'))
# 
# team <- str_extract_all(pl_team_href[,1], '[:graph:]+(?=\\-Stats)') %>% 
#   str_split(., '\\/') %>% 
#   unlist() %>% 
#   .[seq(6, length(.), 6)]
# 
# pl_teams <- data.frame(team = team, team_tag = team_tag, season = pl_season_href$season[2], season_tag = pl_season_href$season_tag[2])

for (i in 1:4) {
  pl_team_list[[i]] <- read_html(paste0('https://fbref.com/en/comps/9/', pl_season_href$season_tag[i], '/', pl_season_href$season[i], '-Premier-League-Stats'))
  
  pl_team_df[[i]] <- pl_team_list[[i]] %>% 
    html_table()
  
  pl_team_href[[i]] <- pl_team_list[[i]] %>% 
    html_nodes("td") %>%
    html_nodes("a") %>%
    html_attr("href") %>% 
    data.frame() %>% 
    filter(str_detect(., 'squads') == TRUE) %>% 
    unique()

  # Extract the unique identifier for each Premier League team
  team_tag[[i]] <- do.call(rbind, str_extract_all(pl_team_href[[i]][,1], '(?<=\\/)[:alnum:]{8}(?=\\/)'))
  
  # Extract the name of the team (ex: Manchester-United)
  if (i == 1) {
    team[[i]] <- str_extract_all(pl_team_href[[i]][,1], '[:graph:]+(?=\\-Stats)') %>% 
      str_split(., '\\/') %>% 
      unlist() %>% 
      .[seq(5, length(.), 5)]
  } else {
    team[[i]] <- str_extract_all(pl_team_href[[i]][,1], '[:graph:]+(?=\\-Stats)') %>% 
      str_split(., '\\/') %>% 
      unlist() %>% 
      .[seq(6, length(.), 6)]
  }
  
  # team[[i]] <- str_extract_all(pl_team_href[[i]][,1], '[:graph:]+(?=\\-Stats)') %>% 
  #   str_split(., '\\/') %>% 
  #   unlist() %>% 
  #   .[seq(5, length(.), 5)]
  
  # Create a one row data frame with the team name, their unique identifier, the season, and the season's unique identifier
  pl_teams[[i]] <- data.frame(team = team[[i]], team_tag = team_tag[[i]], season = pl_season_href$season[i], season_tag = pl_season_href$season_tag[i])
}

# Bind all the elements of the list together into a data frame
pl_teams <- do.call(rbind, pl_teams)

# Labeling the table types to reference in calling the right html addresses
table_types = c('shooting', 'keeper', 'passing', 'passing_types', 'gca', 'defense', 'possession')

##############
## Shooting ##
##############

# Date -- Date listed is local to the match
# Time -- Time listed is local to the match venue
# Round -- Round or Phase of Competition
# GF -- Goals For
# GA -- Goals Against
#
# Standard
# Gls -- Goals scored or allowed
# Sh -- Shots Total (Does not include penalty kicks)
# SoT -- Shots on target Note: Shots on target do not include penalty kicks
# SoT% -- Shots on target percentage Note: Shots on target do not include penalty kicks
# G/Sh -- Goals per shot
# G/SoT -- Goals per shot on target
# Dist -- Average distance, in yards, from goal of all shots taken (Does not include penalty kicks)
# FK -- Shots from free kicks
# PK -- Penalty Kicks Made
# PKatt -- Penalty Kicks Attempted

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

# Performance
# Saves -- Saves made
#
# Penalty Kicks
# PKatt -- Penalty Kicks Attempted
# PKA -- Penalty Kicks Allowed
# PKsv -- Penalty Kicks Saved
# PKm -- Penalty Kicks Missed
#
# Launched (Passes longer than 40 yards)
# Cmp -- Passes Completed
# Att -- Passes Attempted
# Cmp% -- Pass Completion Percentage
# 
# Passes (Not including goal kicks)
# Att -- Passes Attempted
# Thr -- Throws Attempted
# Launch% -- Percentage of Passes that were Launched (Passes longer than 40 yards)
# AvgLen -- Average length of passes, in yards
# 
# Goal Kicks
# Att -- Goal Kicks Attempted
# Launch% -- Percentage of Goal Kicks that were Launched (Passes longer than 40 yards)
# AvgLen -- Average length of goal kicks, in yards
#
# Crosses
# Opp -- Opponent's attempted crosses into penalty area
# Stp -- Number of crosses into penalty area which were successfully stopped by the goalkeeper
# Stp% -- Percentage of crosses into penalty area which were successfully stopped by the goalkeeper
#
# Sweeper
# #OPA -- # of defensive actions outside of penalty area
# AvgDist -- Average distance from goal (in yards) of all defensive actions

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

# Total
# Cmp -- Passes Completed
# Att -- Passes Attempted
# Cmp% -- Pass Completion Percentage (Minimum 30 minutes played per squad game to qualify as a leader)
# TotDist -- Total distance, in yards, that completed passes have traveled in any direction
# PrgDist -- Progressive Distance, or the total distance, in yards, that completed passes have traveled towards the opponent's goal. Note: Passes away from opponent's goal are counted as zero progressive yards.
#
# Short (Passes between 5 and 15 yards)
# Cmp -- Passes Completed
# Att -- Passes Attempted
# Cmp% -- Pass Completion Percentage (Minimum 30 minutes played per squad game to qualify as a leader)
#
# Medium (Passes between 15 and 30 yards)
# Cmp -- Passes Completed
# Att -- Passes Attempted
# Cmp% -- Pass Completion Percentage (Minimum 30 minutes played per squad game to qualify as a leader)
#
# Long (Passes longer than 30 yards)
# Cmp -- Passes Completed
# Att -- Passes Attempted
# Cmp% -- Pass Completion Percentage (Minimum 30 minutes played per squad game to qualify as a leader)
#
# Ast -- Assists
# xA -- xG Assisted
# xG which follows a pass that assists a shot
# Provided by StatsBomb.
# An underline indicates there is a match that is missing data, but will be updated when available.
# KP -- Passes that directly lead to a shot (assisted shots)
# 1/3 -- Completed passes that enter the 1/3 of the pitch closest to the goal (Not including set pieces)
# PPA -- Completed passes into the 18-yard box (Not including set pieces)
# CrsPA -- Completed crosses into the 18-yard box (Not including set pieces)
# Prog -- Progressive Passes, completed passes that move the ball towards the opponent's goal at least 10 yards from its furthest point in the last six passes, or any completed pass into the penalty area. (Excludes passes from the defending 40% of the pitch)

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

# Pass Types
# Live -- Live-ball passes
# Dead -- Dead-ball passes (Includes free kicks, corner kicks, kick offs, throw-ins and goal kicks)
# FK -- Passes attempted from free kicks
# TB -- Completed pass sent between back defenders into open space
# Press -- Passes made while under pressure from opponent
# Sw -- Passes that travel more than 40 yards of the width of the pitch
# Crs -- Crosses
# CK -- Corner Kicks
#
# Corner Kicks
# In -- Inswinging Corner Kicks
# Out -- Outswinging Corner Kicks
# Str -- Straight Corner Kicks
#
# Height
# Ground -- Ground passes
# Low -- Passes that leave the ground, but stay below shoulder-level
# High -- Passes that are above shoulder-level at the peak height
#
# Body Parts
# Left -- Passes attempted using left foot
# Right -- Passes attempted using right foot
# Head -- Passes attempted using head
# TI -- Throw-Ins taken
# Other -- Passes attempted using body parts other than the player's head or feet
#
# Outcomes
# Cmp -- Passes Completed
# Off -- Offsides
# Out -- Out of bounds
# Int -- Intercepted
# Blocks -- Blocked by the opponent who was standing it the path

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

# SCA Types
# SCA -- Shot-Creating Actions
# The two offensive actions directly leading to a shot, such as passes, dribbles and drawing fouls.
# Note: A single player can receive credit for multiple actions and the shot-taker can also receive credit.
# PassLive -- Completed live-ball passes that lead to a shot attempt
# PassDead -- Completed dead-ball passes that lead to a shot attempt.
# Includes free kicks, corner kicks, kick offs, throw-ins and goal kicks
# Drib -- Successful dribbles that lead to a shot attempt
# Sh -- Shots that lead to another shot attempt
# Fld -- Fouls drawn that lead to a shot attempt
# Def -- Defensive actions that lead to a shot attempt
#
# GCA Types 
# GCA -- Goal-Creating Actions
# The two offensive actions directly leading to a goal, such as passes, dribbles and drawing fouls. 
# Note: A single player can receive credit for multiple actions and the shot-taker can also receive credit.
# PassLive -- Completed live-ball passes that lead to a goal
# PassDead -- Completed dead-ball passes that lead to a goal. Includes free kicks, corner kicks, kick offs, throw-ins and goal kicks
# Drib -- Successful dribbles that lead to a goal
# Sh -- Shots that lead to another goal-scoring shot
# Fld -- Fouls drawn that lead to a goal
# Def -- Defensive actions that lead to a goal
# OG -- Actions that led directly to an opponent scoring on their own goal

# * GCA types won't be used to prevent leakage

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

# Tackles
# Tkl -- Number of players tackled
# TklW -- Tackles in which the tackler's team won possession of the ball
# Def 3rd -- Tackles in defensive 1/3
# Mid 3rd -- Tackles in middle 1/3
# Att 3rd -- Tackles in attacking 1/3
#
# Vs Dribbles
# Tkl -- Number of dribblers tackled
# Att -- Number of times dribbled past plus number of tackles
# Tkl% -- Percentage of dribblers tackled, Dribblers tackled divided by dribblers tackled plus times dribbled past
# Past -- Number of times dribbled past by an opposing player
#
# Pressures
# Press -- Number of times applying pressure to opposing player who is receiving, carrying or releasing the ball
# Succ -- Number of times the squad gained possession withing five seconds of applying pressure
# % -- Successful Pressure Percentage, Percentage of time the squad gained possession withing five seconds of applying pressure
# Def 3rd -- Number of times applying pressure to opposing player who is receiving, carrying or releasing the ball, in the defensive 1/3
# Mid 3rd -- Number of times applying pressure to opposing player who is receiving, carrying or releasing the ball, in the middle 1/3
# Att 3rd -- Number of times applying pressure to opposing player who is receiving, carrying or releasing the ball, in the attacking 1/3
#
# Blocks
# Blocks -- Number of times blocking the ball by standing in its path
# Sh -- Number of times blocking a shot by standing in its path
# ShSv -- Number of times blocking a shot that was on target, by standing in its path
# Pass -- Number of times blocking a pass by standing in its path
#
# Int -- Interceptions
# Tkl+Int -- Number of players tackled plus number of interceptions
# Clr -- Clearances
# Err -- Mistakes leading to an opponent's shot

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

# Touches
# Touches -- Number of times a player touched the ball. Note: Receiving a pass, then dribbling, then sending a pass counts as one touch
# Def Pen -- Touches in defensive penalty area
# Def 3rd -- Touches in defensive 1/3
# Mid 3rd -- Touches in middle 1/3
# Att 3rd -- Touches in attacking 1/3
# Att Pen -- Touches in attacking penalty area
# Live -- Live-ball touches. Does not include corner kicks, free kicks, throw-ins, kick-offs, goal kicks or penalty kicks
#
# Dribbles
# Succ -- Dribbles Completed Successfully
# Att -- Dribbles Attempted
# Succ% -- Percentage of Dribbles Completed Successfully
# Minimum .5 dribbles per squad game to qualify as a leader
# #Pl -- Number of Players Dribbled Past
# Megs -- Number of times a player dribbled the ball through an opposing player's legs
#
# Carries
# Carries -- Number of times the player controlled the ball with their feet
# TotDist -- Total distance, in yards, a player moved the ball while controlling it with their feet, in any direction
# PrgDist -- Progressive Distance, Total distance, in yards, a player moved the ball while controlling it with their feet towards the opponent's goal
# Prog -- Carries that move the ball towards the opponent's goal at least 5 yards, or any carry into the penalty area. Excludes carries from the defending 40% of the pitch
# 1/3 -- Carries that enter the 1/3 of the pitch closest to the goal
# CPA -- Carries into the 18-yard box
# Mis -- Number of times a player failed when attempting to gain control of a ball
# Dis -- Number of times a player loses control of the ball after being tackled by an opposing player. Does not include attempted dribbles
#
# Receiving
# Targ -- Number of times a player was the target of an attempted pass
# Rec -- Number of times a player successfully received a pass
# Rec% -- Passes Received Percentage
# Percentage of time a player successfully received a pass
# Minimum 30 minutes played per squad game to qualify as a leader
# Prog -- Progressive Passes Received, Completed passes that move the ball towards the opponent's goal at least 10 yards from its furthest point in the last six passes, or any completed pass into the penalty area. Excludes passes from the defending 40% of the pitch

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

Pl_team_match_data2 <- cbind(Pl_team_shooting_df, 
                             Pl_team_passing_df,
                             Pl_team_passing_types_df,
                             Pl_team_gca_df,
                             Pl_team_possession_df)