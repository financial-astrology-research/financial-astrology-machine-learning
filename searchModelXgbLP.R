# Title     : Search XGB linear model fit with aspects factors table to predict trending and flat days.
# Created by: pablocc
# Created on: 07/10/2020

# CONCLUSION:

library(boot)
library(caret)
library(psych)
library(plyr)
source("./analysis.r")
source("./indicatorPlots.r")

symbol <- "BNB-USD"
pxSelect <- c(
  'MO',
  'ME',
  'VE',
  'SU',
  'MA'
)

pySelect <- c(
  'ME',
  'VE',
  'SU',
  'MA',
  'JU',
  'SA',
  'NN',
  'UR',
  'NE',
  'PL'
)

aspectFilter <- c(
  #0
  #30,
  #45,
  #60,
  #90,
  #103
  #120
  #135
  #150
  #180
)

dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)

dailyAspects <- dailyCombPlanetAspectsFactorsTableLI(
  orbLimit = 5,
  aspectFilter =  aspectFilter,
  pxSelect = pxSelect,
  pySelect = pySelect
)

dailyFastPlanetsSpeed <- dailyFastPlanetsRetrograde()
dailyAspects <- merge(dailyAspects, dailyFastPlanetsSpeed, by = c('Date'))

securityData <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2017-01-01", "2020-08-30"
)

zscoreTrendCut <- 0.5
securityData[, Actbin := cut(abs(zdiffPercent), c(0, zscoreTrendCut, 50), c('neutral', 'trend'))]
plot(securityData$Actbin)

aspectView <- merge(
  securityData[, c('Date', 'Eff', 'Actbin')],
  dailyAspects, by = "Date"
)

#  Reserved data for final test, skip a week to avoid timeserie memory.
securityDataTest <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2020-09-15"
)

securityDataTest[, Actbin := cut(abs(zdiffPercent), c(0, zscoreTrendCut, 50), c('neutral', 'trend'))]
#plot(securityDataTest$Actbin)

aspectViewTest <- merge(
  securityDataTest,
  dailyAspects,
  by = "Date"
)

trainIndex <- createDataPartition(aspectView$Actbin, p = 0.90, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

customSummary <- function (data, lev = levels(data$obs), model = NULL) {
  c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
}

trainXgbLinearModel <- function() {
  control <- trainControl(
    method = "cv",
    number = 10,
    savePredictions = "final",
    returnResamp = "all",
    classProbs = T,
    allowParallel = T,
    verboseIter = T,
    #summaryFunction = customSummary,
    trim = F
  )

  selectCols <- names(aspectViewTrain)[c(-1, -2)]
  fitModel <- train(
    formula(Actbin ~ .),
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

  fitModel$finalModel %>% summary() %>% print()
  fitModel %>% summary() %>% print()
  fitModel %>% print()
  fitModel %>% varImp() %>% print()

  cat("--VALIDATE MODEL--\n\n")
  # Validate test data accuracy.
  validateActbinPred <- predict(fitModel, aspectViewValidate, type = "raw")
  #validateActbinPred <- mapvalues(validateActbinPred, from = c("up", "down"), to = c("buy", "sell"))
  validateResult <- table(
    actualclass = as.character(aspectViewValidate$Actbin),
    predictedclass = as.character(validateActbinPred)
  ) %>%
    confusionMatrix(positive = "trend")
  print(validateResult)

  cat("--TEST MODEL--\n\n")
  # Validate test data accuracy.
  testActbinPred <- predict(fitModel, aspectViewTest, type = "raw")
  #testActbinPred <- mapvalues(testActbinPred, from = c("up", "down"), to = c("buy", "sell"))
  testResult <- table(
    actualclass = as.character(aspectViewTest$Actbin),
    predictedclass = as.character(testActbinPred)
  ) %>%
    confusionMatrix(positive = "trend")
  print(testResult)

  return(fitModel)
}

fitModel1 <- trainXgbLinearModel()
fitModel2 <- trainXgbLinearModel()
fitModel3 <- trainXgbLinearModel()

# Predict outcomes for all weak learner models.
aspectView$EffNeuP1 <- predict(fitModel1, aspectView, type = "prob")$neutral
aspectView$EffNeuP2 <- predict(fitModel2, aspectView, type = "prob")$neutral
aspectView$EffNeuP3 <- predict(fitModel3, aspectView, type = "prob")$neutral

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
probCols <- c('EffNeuP1', 'EffNeuP2', 'EffNeuP3')
ensambleModel <- train(
  x = aspectView[, ..probCols],
  y = aspectView$Actbin,
  method = "gbm",
  trControl = ensambleControl,
  tuneLength = 2
)

# Predict outcomes for all weak learner models.
aspectViewTest$EffNeuP1 <- predict(fitModel1, aspectViewTest, type = "prob")$neutral
aspectViewTest$EffNeuP2 <- predict(fitModel2, aspectViewTest, type = "prob")$neutral
aspectViewTest$EffNeuP3 <- predict(fitModel3, aspectViewTest, type = "prob")$neutral

# Final ensamble prediction.
aspectViewTest$ActionPred <- predict(ensambleModel, aspectViewTest, type = "raw")

table(
  actualclass = as.character(aspectViewTest$Actbin),
  predictedclass = as.character(aspectViewTest$ActionPred)
) %>%
  confusionMatrix(positive = "trend") %>%
  print()

# Full data set prediction.
dailyAspects$EffNeuP1 <- predict(fitModel1, dailyAspects, type = "prob")$neutral
dailyAspects$EffNeuP2 <- predict(fitModel2, dailyAspects, type = "prob")$neutral
dailyAspects$EffNeuP3 <- predict(fitModel3, dailyAspects, type = "prob")$neutral
dailyAspects$EffPred <- predict(ensambleModel, dailyAspects, type = "raw")

# Round probabilities.
dailyAspects[, EffNeuP1 := format(EffNeuP1, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffNeuP2 := format(EffNeuP2, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffNeuP3 := format(EffNeuP3, format = "f", big.mark = ",", digits = 3)]

fwrite(dailyAspects, paste("~/Desktop/", symbol, "-predict-xgblinearLP-ensamble", ".csv", sep = ""))

#saveRDS(fitModel, paste("./models/", symbol, "_xgb1", ".rds", sep=""))
#fwrite(dailyAspects, paste("~/Desktop/ml", symbol, "daily-xgb3.csv", sep = "-"))
