# Title     : Search a XGB model fit for aspect / planet activation counts.
# Created by: pablocc
# Created on: 07/10/2020

library(boot)
library(caret)
library(psych)
library(plyr)
source("./analysis.r")
source("./indicatorPlots.r")

aspectFilter <- c()
pxFilter <- c('MO', 'SU', 'ME', 'MA', 'JU', 'SA', 'UR', 'NE', 'PL', 'NN')
#dailyAspects <- dailyCombPlanetAspectsFactorsTable(orbLimit = 2, aspectFilter =  aspectFilter)
#dailyAspects <- dailyCombPlanetAspectsFactorsTableLE(
#  orbLimit = 2.5,
#  aspectFilter =  aspectFilter,
#  pxFilter = pxFilter
#)

dailyAspectsCount <- dailyAspectsGeneralizedCount(
  orbLimit = 2,
  pxFilter = pxFilter,
)

dailyAspectsPlanetYCount <- dailyAspectsPlanetYGeneralizedCount(
  orbLimit = 2,
  pxFilter = pxFilter,
)

dailyFastPlanetsSpeed <- dailyFastPlanetsRetrograde()
#dailySlowPlanetsSpeed <- dailySlowPlanetsRetrograde()
dailyAspects <- dailyAspectsCount
dailyAspects <- merge(dailyAspects, dailyAspectsPlanetYCount, by = c('Date'))
#dailyAspects <- merge(dailyAspects, dailyFastPlanetsSpeed, by = c('Date'))
#dailyAspects <- merge(dailyAspects, dailySlowPlanetsSpeed, by = c('Date'))

symbol <- "BAT-USD"
securityData <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2017-01-01", "2020-07-31"
)

aspectView <- merge(
  securityData[, c('Date', 'Eff', 'Actbin')],
  dailyAspects, by = "Date"
)

trainIndex <- createDataPartition(aspectView$Eff, p = 0.80, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

#  Reserved data for final test, skip a week to avoid timeserie memory.
securityDataTest <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2020-08-08"
)

aspectViewTest <- merge(
  securityDataTest,
  dailyAspects,
  by = "Date"
)

control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  returnResamp = "all",
  classProbs = T,
  allowParallel = T,
  verboseIter = T,
  trim = F
)

selectCols <- names(aspectViewTrain)[c(-1, -3)]
fitModel <- train(
  formula(Eff ~ .),
  data = aspectViewTrain[, ..selectCols],
  #method = "xgbDART", # 0.51
  method = "xgbLinear", # 0.52
  #method = "xgbTree", # 0.45
  metric = "Kappa",
  maximize = T,
  trControl = control,
  tuneLength = 2
  #tuneGrid = expand.grid(
  #  nrounds = 100,
  #  lambda = 0,
  #  alpha = 0,
  #  eta = 0.3
  #),
  #maxit = 100,
  #repeats = 200
)

fitModel$finalModel %>% summary()
fitModel %>% summary()
fitModel %>% print()
fitModel %>% varImp()

cat("--VALIDATE MODEL--\n\n")
# Validate test data accuracy.
validateActbinPred <- predict(fitModel, aspectViewValidate, type = "raw")
validateActbinPred <- mapvalues(validateActbinPred, from = c("up", "down"), to = c("buy", "sell"))
validateResult <- table(
  actualclass = as.character(aspectViewValidate$Actbin),
  predictedclass = as.character(validateActbinPred)
) %>%
  confusionMatrix(positive = "buy")
print(validateResult)

cat("--TEST MODEL--\n\n")
# Validate test data accuracy.
testActbinPred <- predict(fitModel, aspectViewTest, type = "raw")
testActbinPred <- mapvalues(testActbinPred, from = c("up", "down"), to = c("buy", "sell"))
testResult <- table(
  actualclass = as.character(aspectViewTest$Actbin),
  predictedclass = as.character(testActbinPred)
) %>%
  confusionMatrix(positive = "buy")
print(testResult)

finalActbinPred <- predict(fitModel, dailyAspects, type = "raw")
finalActbinPred <- mapvalues(finalActbinPred, from = c("up", "down"), to = c("buy", "sell"))
dailyAspects[, finalPred := finalActbinPred]

#saveRDS(fitModel, paste("./models/", symbol, "_xgb1", ".rds", sep=""))
#fwrite(dailyAspects, paste("~/Desktop/ml", symbol, "daily-xgb2.csv", sep = "-"))
