# Title     : Search a XGB model fit for aspect / planetY activation counts.
# Created by: pablocc
# Created on: 07/10/2020

library(boot)
library(caret)
library(psych)
library(plyr)
source("./analysis.r")
source("./indicatorPlots.r")

symbol <- "ETH-USD"
pxSelect <- c(
  'VE',
  'MA'
)

dailyAspectsCount <- dailyAspectsGeneralizedCount(
  orbLimit = 2,
  pxSelect = pxSelect,
)

dailyAspectsPlanetYCount <- dailyPlanetYActivationCount(
  orbLimit = 2,
  pxSelect = pxSelect,
)

dailyAspects <- dailyAspectsCount
dailyAspects <- merge(dailyAspects, dailyAspectsPlanetYCount, by = c('Date'))

securityData <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2017-01-01", "2020-07-31"
)

aspectView <- merge(
  securityData[, c('Date', 'Eff', 'Actbin')],
  dailyAspects, by = "Date"
)

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

trainIndex <- createDataPartition(aspectView$Eff, p = 0.80, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

trainXgbLinearModel <- function() {
  control <- trainControl(
    method = "cv",
    number = 20,
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

  return(fitModel)
}

fitModel1 <- trainXgbLinearModel()
fitModel2 <- trainXgbLinearModel()
fitModel3 <- trainXgbLinearModel()
fitModel4 <- trainXgbLinearModel()

# Predict outcomes for all weak learner models.
aspectView$EffUpP1 <- predict(fitModel1, aspectView, type = "prob")$up
aspectView$EffUpP2 <- predict(fitModel2, aspectView, type = "prob")$up
aspectView$EffUpP3 <- predict(fitModel3, aspectView, type = "prob")$up
aspectView$EffUpP4 <- predict(fitModel4, aspectView, type = "prob")$up

ensambleControl <- trainControl(
  method = "boot",
  savePredictions = "final",
  returnResamp = "all",
  classProbs = T,
  allowParallel = T,
  verboseIter = T,
  trim = F
)

# Train ensamble model.
probCols <- c('EffUpP1', 'EffUpP2', 'EffUpP3', 'EffUpP4')
ensambleModel <- train(
  x = aspectView[, ..probCols],
  y = aspectView$Actbin,
  method = "gbm",
  trControl = ensambleControl,
  tuneLength = 2
)

# Predict outcomes for all weak learner models.
aspectViewTest$EffUpP1 <- predict(fitModel1, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP2 <- predict(fitModel2, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP3 <- predict(fitModel3, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP4 <- predict(fitModel4, aspectViewTest, type = "prob")$up

# Final ensamble prediction.
aspectViewTest$ActionPred <- predict(ensambleModel, aspectViewTest, type = "raw")

table(
  actualclass = as.character(aspectViewTest$Actbin),
  predictedclass = as.character(aspectViewTest$ActionPred)
) %>%
  confusionMatrix(positive = "buy") %>%
  print()

# Full data set prediction.
dailyAspects$EffUpP1 <- predict(fitModel1, dailyAspects, type = "prob")$up
dailyAspects$EffUpP2 <- predict(fitModel2, dailyAspects, type = "prob")$up
dailyAspects$EffUpP3 <- predict(fitModel3, dailyAspects, type = "prob")$up
dailyAspects$EffUpP4 <- predict(fitModel4, dailyAspects, type = "prob")$up
dailyAspects$EffPred <- predict(ensambleModel, dailyAspects, type = "raw")

# Round probabilities.
dailyAspects[, EffUpP1 := format(EffUpP1, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP2 := format(EffUpP2, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP3 := format(EffUpP3, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP4 := format(EffUpP4, format = "f", big.mark = ",", digits = 3)]

fwrite(dailyAspects, paste("~/Desktop/", symbol, "-predict-xgblinearLE-ensamble", ".csv", sep = ""))

#saveRDS(fitModel, paste("./models/", symbol, "_xgb1", ".rds", sep=""))
#fwrite(dailyAspects, paste("~/Desktop/ml", symbol, "daily-xgb3.csv", sep = "-"))
