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

split <- sample.split(pl_team_data$Goals, SplitRatio = 0.8)

pl_team_data.train <- subset(pl_team_data, split == TRUE)
pl_team_data.test <- subset(pl_team_data, split == FALSE)

test.Goals <- pl_team_data.test %>% pull(Goals)

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

sqrt(mean((test.Goals - xgbTree.preds)^2))
# .3208364

sqrt(mean((test.Goals - xgbTree.rounded.preds)^2))
# .3247992

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


xgbTree2.model <- train(Goals ~ .,
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

xgbTree2.model
# .32590965

xgbTree2.preds <- predict(xgbTree2.model, pl_team_data.test)

# Predict team-average rows...

xgbTree2.rounded.preds <- round(xgbTree2.preds)
xgbTree2.rounded.preds <- ifelse(xgbTree2.rounded.preds < 0, 0, xgbTree2.rounded.preds)

sqrt(mean((test.Goals - xgbTree2.preds)^2))
# .3275023

sqrt(mean((test.Goals - xgbTree2.rounded.preds)^2))
# .3281651

xgbTree3.model <- train(Goals ~ .,
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

xgbTree3.model
# .3310949

xgbTree3.preds <- predict(xgbTree3.model, pl_team_data.test)

xgbTree3.rounded.preds <- round(xgbTree3.preds)
xgbTree3.rounded.preds <- ifelse(xgbTree3.rounded.preds < 0, 0, xgbTree3.rounded.preds)

sqrt(mean((test.Goals - xgbTree3.preds)^2))
# .3208364

sqrt(mean((test.Goals - xgbTree3.rounded.preds)^2))

# Saving this model as an RDS object
saveRDS(xgbTree3.model, '/Users/matthewmorgan/Documents/Stat 495R/PLXG App/PLXGModel.RData')

# Poisson Regression
summary(m1 <- glm(Goals ~ ., family = 'poisson', data = pl1920.train))

m1.preds <- predict(m1, pl1920.test)

m1.rounded.preds <- round(m1.preds)
m1.rounded.preds <- ifelse(m1.rounded.preds < 0, 0, m1.rounded.preds)

sum(test.Goals - m1.rounded.preds)/length(test.Goals)

# Using Lasso regression for variable selection

# ranger Model
ranger.model <- train(Goals ~ ., 
                      data = pl1920.train,
                      method = 'ranger',
                      trControl = trainControl(method = 'cv', number = 10),
                      preProcess = c('nzv'),
                      tuneGrid = expand.grid(mtry = 101,
                                             splitrule = 'variance',
                                             min.node.size = 5),
                      maximize = FALSE 
)

ranger.model

ranger.preds <- predict(ranger.model, pl1920.test)

ranger.rounded.preds <- round(ranger.preds)
ranger.rounded.preds <- ifelse(ranger.rounded.preds < 0, 0, ranger.rounded.preds)

sum(sqrt((test.Goals - ranger.rounded.preds)^2))/length(test.Goals)

# randomForest Model
rf.model <- train(Goals ~ .,
                  data = pl1920.train,
                  method = 'rf',
                  trControl = trainControl(method = 'cv', number = 10),
                  preProcess = c('nzv'),
                  tuneGrid = expand.grid(mtry = 101),
                  maximize = FALSE 
)

rf.model

rf.preds <- predict(rf.model, pl1920.test)

rf.rounded.preds <- round(rf.preds)
rf.rounded.preds <- ifelse(rf.rounded.preds < 0, 0, rf.rounded.preds)

sum(sqrt((test.Goals - rf.rounded.preds)^2))/length(test.Goals)

# xgbLinear model
xgbLinear.model <- train(Goals ~ .,
                         data = pl1920.train,
                         method = 'xgbLinear',
                         trControl = trainControl(method = 'cv', number = 10),
                         tuneGrid = expand.grid(nrounds = 100,
                                                lambda = c(.001, .0015, .002),
                                                alpha = .015,
                                                eta = .001),
                         maximize = FALSE)

xgbLinear.model

# Getting Team Averages
Pl_team_avg <- pl_team_data %>% select(-Goals, -Date, -Round, -Venue, -Opponent) %>% group_split(Team)

Avg_Dat <- list()

for (i in 1:length(Pl_team_avg)) {
  Avg_Dat[[i]] <- as.data.frame(t(apply(Pl_team_avg[[i]] %>% select(-Team), 2, mean)))
  Avg_Dat[[i]]$Team <- Pl_team_avg[[i]] %>% pull(Team) %>% head(1)
}

Avg_Dat <- do.call(rbind, Avg_Dat)

Avg_Dat <- Avg_Dat[,c(98, 1:97)]

Avg_Dat_10 <- Avg_Dat %>% select(Team, SoT, Opp_Saves, PKatt, SCA_PassLive, Short_Att, Dist, Clr, TklW, TB, Crosses_Att)

write.csv(Avg_Dat_10, '/Users/matthewmorgan/Documents/Stat 495R/PLXG App/PL_10.csv', row.names = FALSE)

Arsenal3.preds <- predict(xgbTree3.model, Avg_Dat_10[1,])
Arsenal2.preds <- predict(xgbTree2.model, Avg_Dat[1,])
Arsenal.preds <- predict(xgbTree.model, Avg_Dat[1,])

varImp(xgbTree3.model)

saveRDS(xgbTree3.model, '/Users/matthewmorgan/Documents/Stat 495R/PLXG App')
