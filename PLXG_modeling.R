library(tidyverse)
library(vroom)
library(caret)
library(caTools)
library(xgboost)

# Read in data and impute 0s for NAs
pl_team_data <- vroom('Pl_team_match_data.csv') %>% mutate(Dist = ifelse(is.na(Dist), 0, Dist),
                                                           Opp_GK_AvgLen = ifelse(is.na(Opp_GK_AvgLen), 0, Opp_GK_AvgLen),
                                                           Opp_AvgDist = ifelse(is.na(Opp_AvgDist), 0, Opp_AvgDist))

pl_team_data2 <- pl_team_data %>% select(-Opp_Saves, -Opp_Launched_Cmp, -Opp_Launched_Att, 
                                         -Opp_Passes_Att, -Opp_Thr_Att, -Opp_Passes_AvgLen, 
                                         -Opp_GK_Att, -Opp_GK_AvgLen, -Crosses_Att, -Opp_Crosses_Stp, 
                                         -`Opp_#OPA`, -Opp_AvgDist, -Players_Tkl, -TklW, -Tkl_Def_3rd, 
                                         -Tkl_Mid_3rd, -Tkl_Att_3rd, -Dribblers_Tkl, -Dribblers_Past,
                                         -Press_Succ, -Press_Def_3rd, -Press_Mid_3rd, -Press_Att_3rd,
                                         -Blocks_Sh, -Blocks_ShSv, -Blocks_Pass, -Int, -Clr, -Err) 

# Splitting the data into a training set and a test set
split <- sample.split(pl_team_data2$Goals, SplitRatio = 0.8)

pl_team_data.train <- subset(pl_team_data2, split == TRUE)
pl_team_data.test <- subset(pl_team_data2, split == FALSE)

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
varImp(xgbTree.model, scale = FALSE)

# 15 most important variables for xgbTree.model
# SoT              0.5689924
# Opp_Saves        0.2856196
# PKatt            0.0438727
# SCA_Total        0.0370697
# Short_Cmp        0.0205521
# TB               0.0077533
# Dead             0.0057713
# Clr              0.0056416
# Dist             0.0031301
# TklW             0.0031152
# Err              0.0026727
# Ground           0.0017426
# Crosses_Att      0.0015149
# TI               0.0010820
# Carries_ProgDist 0.0008998

# Building an xgbTree model with only the 10 most important variables from varImp(xgbTree.model)
xgbTree2.model <- train(Goals ~ .,
                        data = pl_team_data.train %>% select(Goals, Team, SoT, Opp_Saves, PKatt, SCA_Total, Short_Cmp, TB, Dead, Clr, Dist, TklW),
                        method = 'xgbTree',
                        trControl = trainControl(method = 'cv', number = 10),
                        tuneGrid = expand.grid(nrounds = c(600, 650, 700, 750),
                                               max_depth = 1,
                                               eta = c(.3, .32, .325, .33, .333, .34),
                                               gamma = 0,
                                               colsample_bytree = c(.8, .85),
                                               min_child_weight = 1,
                                               subsample = c(.75, 1)),
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

# Building an xgbTree model with only the 10 most important variables from varImp(xgbTree.model)
xgbTree3.model <- train(Goals ~ .,
                        data = pl_team_data.train,
                        method = 'xgbTree',
                        trControl = trainControl(method = 'cv', number = 10),
                        # tuneLength = 4,
                        tuneGrid = expand.grid(nrounds = c(800, 900, 1000),
                                               max_depth = 1,
                                               eta = c(.03, .04, .045, .05),
                                               gamma = 0,
                                               colsample_bytree = .85,
                                               min_child_weight = 1,
                                               subsample = c(.35, .4, .45)),
                        maximize = FALSE)

xgbTree3.model
# .3310949

xgbTree3.preds <- predict(xgbTree3.model, pl_team_data.test)

xgbTree3.rounded.preds <- round(xgbTree3.preds)
xgbTree3.rounded.preds <- ifelse(xgbTree3.rounded.preds < 0, 0, xgbTree3.rounded.preds)

sqrt(mean((test.Goals - xgbTree3.preds)^2))
# .3208364

sqrt(mean((test.Goals - xgbTree3.rounded.preds)^2))

# Poisson Regression
summary(m1 <- glm(Goals ~ ., family = 'poisson', data = pl_team_data.train))

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
                      preProcess = c('nzv', 'center', 'scale'),
                      tuneGrid = expand.grid(mtry = 10, 60,
                                             splitrule = 'variance',
                                             min.node.size = 5),
                      maximize = FALSE)

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
                  tuneGrid = expand.grid(mtry = 73),
                  maximize = FALSE)

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
                         tuneLength = 4,
                         # tuneGrid = expand.grid(nrounds = 100,
                         #                        lambda = c(.001, .0015, .002),
                         #                        alpha = .015,
                         #                        eta = .001),
                         maximize = FALSE)

xgbLinear.model

xgbLinear.preds <- predict(xgbLinear.model, pl_team_data.test)

xgbLinear.rounded.preds <- round(xgbLinear.preds)
xgbLinear.rounded.preds <- ifelse(xgbLinear.rounded.preds < 0, 0, xgbLinear.rounded.preds)

sqrt(mean((test.Goals - xgbLinear.preds)^2))

sqrt(mean((test.Goals - xgbLinear.rounded.preds)^2))

# For app.R
Full_PL_10 <- pl_team_data %>% select(Goals, Team, Date, Opponent, SoT, Opp_Saves, PKatt, SCA_Total, Short_Cmp, TB, Dead, Clr, Dist, TklW)

# Reading in the model that is being used by app.R
PLXG.Model <- readRDS('/Users/matthewmorgan/Documents/Stat 495R/PLXG/PLXG App/PLXGModel.RData')

# Adding an XG prediction to each match
Full_PL_10 <- Full_PL_10 %>% mutate(XG = round(stats::predict(PLXG.Model, Full_PL_10 %>% select(-Goals, -Date, -Opponent)), digits = 2))

# Full_PL_10 <- merge(Full_PL_10, Avg_Dat_10 %>% select(Team, XG), by = 'Team')

# Write out Full_PL_10 to be used in app.R
write.csv(Full_PL_10, '/Users/matthewmorgan/Documents/Stat 495R/PLXG/PLXG App/Full_PL_10.csv', row.names = FALSE)

# Removing the XG column before modeling
Full_PL_10$XG <- NULL

# Using the xgboost package

# Splitting Full_PL_10 into a training and test set
split <- sample.split(Full_PL_10$Goals, SplitRatio = 0.8)

Full_PL_10.train <- subset(Full_PL_10, split == TRUE)
Full_PL_10.test <- subset(Full_PL_10, split == FALSE)

train.y <- Full_PL_10.train$Goals
test.y <- Full_PL_10.test$Goals

Full_PL_10.train <- Full_PL_10.train %>% select(-Date, -Opponent, -Goals)
Full_PL_10.test <- Full_PL_10.test %>% select(-Date, -Opponent, -Goals)

train.x <- data.matrix(Full_PL_10.train)
test.x <- data.matrix(Full_PL_10.test)

xgb.train = xgb.DMatrix(data = train.x, label = train.y)
xgb.test = xgb.DMatrix(data = test.x, label = test.y)

watchlist = list(train = xgb.train, test = xgb.test)

param <- list(objective = 'reg:squarederror', 
              booster = 'gbtree',
              eval_metric = 'rmse',
              eta = .3,
              max_depth = 1,
              subsample = .75,
              colsample_bytree = .85,
              nthread = 8)

clf <- xgb.cv(params = param, 
              data = xgb.train, 
              nrounds = 500,
              nfold = 10,
              watchlist = watchlist,
              verbose = 1,
              print_every_n = 10,
              early_stopping_rounds = 20,
              maximize = FALSE)

# bestRound <- clf$best_iteration
print(clf$evaluation_log[clf$best_iteration])

xgb.model <- xgb.train(params = param, 
                       data = xgb.train, 
                       nrounds = bestRound,
                       watchlist = watchlist,
                       verbose = 1,
                       maximize = FALSE)

# Calculate train RMSE ~ .293
sqrt(mean((test.y - stats::predict(xgb.model, test.x))^2))
