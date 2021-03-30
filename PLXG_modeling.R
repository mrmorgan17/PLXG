library(tidyverse)
library(vroom)
library(caret)
library(caTools)

pl_team_data <- vroom('Pl_team_match_data.csv') %>% mutate(Dist = ifelse(is.na(Dist), 0, Dist),
                                                           Opp_GK_AvgLen = ifelse(is.na(Opp_GK_AvgLen), 0, Opp_GK_AvgLen),
                                                           Opp_AvgDist = ifelse(is.na(Opp_AvgDist), 0, Opp_AvgDist))


# Dist = 2 NA's
# Opp_GK_AvgLen = 37 NA's
# Opp_AvgDist = 24 NA's

# NA's were imputed with 0s

# Splitting the data into a training set and a test set
split <- sample.split(pl_team_data$Goals, SplitRatio = 0.8)

pl_team_data.train <- subset(pl_team_data, split == TRUE)
pl_team_data.test <- subset(pl_team_data, split == FALSE)

test.Goals <- pl_team_data.test %>% pull(Goals)

# Removing the Goals variable from the test set
pl_team_data.test <- pl_team_data.test %>% select(-Goals)
 
# xgbTree model => best so far
xgbTree.model <- train(Goals ~ .,
                       data = pl_team_data.train %>% select(-Date, -Round, -Venue, -Opponent),
                       method = 'xgbTree',
                       trControl = trainControl(method = 'cv', number = 10),
                       tuneGrid = expand.grid(nrounds = c(400, 450, 500, 550),
                                              max_depth = 1,
                                              eta = c(.3, .31, .32, .325),
                                              gamma = 0,
                                              colsample_bytree = .8,
                                              min_child_weight = 1,
                                              subsample = 1),
                       maximize = FALSE)

xgbTree.model
# .3310949

xgbTree.preds <- predict(xgbTree.model, pl_team_data.test)

xgbTree.rounded.preds <- round(xgbTree.preds)
xgbTree.rounded.preds <- ifelse(xgbTree.rounded.preds < 0, 0, xgbTree.rounded.preds)

# Calculate RMSE for the test data with normal predictions from xgbTree.model
sqrt(mean((test.Goals - xgbTree.preds)^2))
# .3208364

# Calculate RMSE for the test data with rounded predictions from xgbTree.model
sqrt(mean((test.Goals - xgbTree.rounded.preds)^2))
# .3247992

# What are the most important varaibles for xgbTree.model?
varImp(xgbTree.model)

# 15 most important variables for xgbTree.model
# SoT             0.6082823
# Opp_Saves       0.2798113
# PKatt           0.0491460
# Short_Cmp       0.0164546
# TB              0.0095465
# Dist            0.0058872
# Clr             0.0056945
# Touches_Live    0.0045751
# TklW            0.0033724
# Err             0.0026964
# Crosses_Att     0.0023461
# Dead            0.0006286
# Blocks_Sh       0.0006169
# SCA_PassLive    0.0005878
# Other           0.0005757

# Building an xgbTree model with only the 10 most important variables from varImp(xgbTree.model)
xgbTree2.model <- train(Goals ~ .,
                        data = pl_team_data.train %>% select(Goals, Team, SoT, Opp_Saves, PKatt, SCA_PassLive, Short_Att, Dist, Clr, TklW, TB, Crosses_Att),
                        method = 'xgbTree',
                        trControl = trainControl(method = 'cv', number = 10),
                        tuneGrid = expand.grid(nrounds = c(400, 450, 500, 550),
                                               max_depth = 1,
                                               eta = c(.3, .31, .32, .325),
                                               gamma = 0,
                                               colsample_bytree = .8,
                                               min_child_weight = 1,
                                               subsample = 1),
                        maximize = FALSE)

xgbTree2.model
# .3310949

xgbTree2.preds <- predict(xgbTree2.model, pl_team_data.test)

xgbTree2.rounded.preds <- round(xgbTree2.preds)
xgbTree2.rounded.preds <- ifelse(xgbTree2.rounded.preds < 0, 0, xgbTree2.rounded.preds)

sqrt(mean((test.Goals - xgbTree2.preds)^2))
# .3208364

sqrt(mean((test.Goals - xgbTree2.rounded.preds)^2))

# Saving this model as an RDS object so it can be used by the Shiny app
saveRDS(xgbTree2.model, '/Users/matthewmorgan/Documents/Stat 495R/PLXG/PLXG App/PLXGModel.RData')

# Poisson Regression
summary(m1 <- glm(Goals ~ ., family = 'poisson', data = pl1920.train))

m1.preds <- predict(m1, pl_team_data.test)

m1.rounded.preds <- round(m1.preds)
m1.rounded.preds <- ifelse(m1.rounded.preds < 0, 0, m1.rounded.preds)

sqrt(mean((test.Goals - m1.preds)^2))

sqrt(mean((test.Goals - m1.rounded.preds)^2))

# ranger Model
ranger.model <- train(Goals ~ ., 
                      data = pl_team_data.train,
                      method = 'ranger',
                      trControl = trainControl(method = 'cv', number = 10),
                      preProcess = c('nzv'),
                      tuneGrid = expand.grid(mtry = 101,
                                             splitrule = 'variance',
                                             min.node.size = 5),
                      maximize = FALSE 
)

ranger.model

ranger.preds <- predict(ranger.model, pl_team_data.test)

ranger.rounded.preds <- round(ranger.preds)
ranger.rounded.preds <- ifelse(ranger.rounded.preds < 0, 0, ranger.rounded.preds)

sqrt(mean((test.Goals - ranger.preds)^2))

sqrt(mean((test.Goals - ranger.rounded.preds)^2))

# randomForest Model
rf.model <- train(Goals ~ .,
                  data = pl_team_data.train,
                  method = 'rf',
                  trControl = trainControl(method = 'cv', number = 10),
                  preProcess = c('nzv'),
                  tuneGrid = expand.grid(mtry = 101),
                  maximize = FALSE 
)

rf.model

rf.preds <- predict(rf.model, pl_team_data.test)

rf.rounded.preds <- round(rf.preds)
rf.rounded.preds <- ifelse(rf.rounded.preds < 0, 0, rf.rounded.preds)

sqrt(mean((test.Goals - rf.preds)^2))

sqrt(mean((test.Goals - rf.rounded.preds)^2))

# xgbLinear model
xgbLinear.model <- train(Goals ~ .,
                         data = pl_team_data.train,
                         method = 'xgbLinear',
                         trControl = trainControl(method = 'cv', number = 10),
                         tuneGrid = expand.grid(nrounds = 100,
                                                lambda = c(.001, .0015, .002),
                                                alpha = .015,
                                                eta = .001),
                         maximize = FALSE)

xgbLinear.model

xgbLinear.preds <- predict(xgbLinear.model, pl_team_data.test)

xgbLinear.rounded.preds <- round(xgbLinear.preds)
xgbLinear.rounded.preds <- ifelse(xgbLinear.rounded.preds < 0, 0, xgbLinear.rounded.preds)

sqrt(mean((test.Goals - xgbLinear.preds)^2))

sqrt(mean((test.Goals - xgbLinear.rounded.preds)^2))

# Getting Team Averages
Pl_team_avg <- pl_team_data %>% select(-Goals, -Date, -Round, -Venue, -Opponent) %>% group_split(Team)

Avg_Dat <- list()

for (i in 1:length(Pl_team_avg)) {
  Avg_Dat[[i]] <- as.data.frame(t(apply(Pl_team_avg[[i]] %>% select(-Team), 2, mean)))
  Avg_Dat[[i]]$Team <- Pl_team_avg[[i]] %>% pull(Team) %>% head(1)
}

Avg_Dat <- do.call(rbind, Avg_Dat)

# Putting the Team column first
Avg_Dat <- Avg_Dat[,c(98, 1:97)]

# Select the Team and the 10 most important variables
Avg_Dat_10 <- Avg_Dat %>% select(Team, SoT, Opp_Saves, PKatt, SCA_PassLive, Short_Att, Dist, Clr, TklW, TB, Crosses_Att)

# Writing out a .csv of each team's averages for the 10 most important variables for xgbTree3.model to be used in the Shint App
write.csv(Avg_Dat_10, '/Users/matthewmorgan/Documents/Stat 495R/PLXG/PLXG App/PL_10.csv', row.names = FALSE)
