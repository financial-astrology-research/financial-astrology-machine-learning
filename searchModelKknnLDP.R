# Title     : Daily aspects factors table KNN regression model with CV control that estimate daily price percent change.
# Purpose   : Based on ModelLD this model has some variations:
#             1) Planets MO, ME, VE, SU fast planets applying to all slow planets except NN.
#             2) CV folds to 5 with 5 repeats.
#             3) Validate fit using Actbin daily price change (buy / sell) instead of Effect
#             4) Fit is based on MA(2, 3) effect to smooth price variations.
#             5) Fit weak learners for MA trend and ensamble for Actbin to generalize for daily change.
#             6) Split to 80/20 proportion.
#             7) Optimize weak learners and ensamble for Kappa.
#             8) 9 weak learners that combine multiples fast aspects cycles.

library(boot)
library(caret)
library(psych)
library(gbm)
library(ModelMetrics)
source("./analysis.r")
source("./indicatorPlots.r")

symbol <- "BTC-USD"
zdiffPercentCut <- 2
maPriceFsPeriod <- 2
maPriceSlPeriod <- 3
orbLimit <- 4

pxSelect <- c(
  'MO',
  'ME',
  'VE',
  'SU'
)

pySelect <- c(
  'ME',
  'VE',
  'SU',
  'MA',
  'JU',
  'SA',
  #'NN',
  'UR',
  'NE',
  'PL'
)

aspectFilter <- c(
  30,
  60,
  120
)

dailyAspects <- dailyCombPlanetAspectsFactorsTableLI(
  orbLimit = orbLimit,
  pxSelect = pxSelect,
  pySelect = pySelect,
  aspectFilter = aspectFilter
)

control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  savePredictions = "all",
  verboseIter = T,
  allowParallel = T,
  trim = F
)

# Reserved data for validation.
securityDataTest <- mainOpenSecurity(
  symbol, maPriceFsPeriod, maPriceSlPeriod,
  "%Y-%m-%d", "2020-09-25"
)

aspectViewTest <- merge(
  securityDataTest,
  dailyAspects, by = "Date"
)

modelTrain <- function(useFeatures, maPriceFsPeriod, maPriceSlPeriod, modelId) {
  securityData <- mainOpenSecurity(
    symbol, maPriceFsPeriod, maPriceSlPeriod,
    "%Y-%m-%d", "2010-01-01", "2020-06-30"
  )

  # Filter the extreme outliers.
  cat(paste("Original days rows: ", nrow(securityData)), "\n")
  #minDiff <- securityData[zdiffPercent > -1.7 & zdiffPercent < 0, min(diffPercent)]
  #maxDiff <- securityData[zdiffPercent < 1.7 & zdiffPercent > 0, max(diffPercent)]
  securityData <- securityData[abs(zdiffPercent) <= zdiffPercentCut]
  hist(securityData$diffPercent)
  cat(paste("Total days rows: ", nrow(securityData)), "\n")

  aspectView <- merge(
    securityData[, c('Date', 'diffPercent', 'Actbin', 'Eff')],
    dailyAspects, by = "Date"
  )

  selectCols <- c('diffPercent', useFeatures)
  cat("Using features: ", selectCols, "\n")
  trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.80, list = FALSE)
  aspectViewTrain <- aspectView[trainIndex,]
  aspectViewValidate <- aspectView[-trainIndex,]
  fitModel <- train(
    formula(diffPercent ~ .),
    data = aspectViewTrain[, ..selectCols],
    method = "kknn",
    metric = "RMSE",
    trControl = control,
    tuneGrid = expand.grid(
      kmax = 7,
      distance = 2,
      kernel = "optimal"
    )
  )

  # Validate data predictions.
  validateDiffPercentPred <- predict(fitModel, aspectViewValidate, type = "raw")
  validatePredictionMSE <- mse(aspectViewValidate$diffPercent, validateDiffPercentPred)
  validatePredictionCorrelation <- cor(aspectViewValidate$diffPercent, validateDiffPercentPred)
  plot(aspectViewValidate$diffPercent, validateDiffPercentPred)
  cat("Validate MSE: ", validatePredictionMSE, "\n")
  cat("Validate Actual/Predicted correlation: ", validatePredictionCorrelation, "\n\n")

  testDiffPercentPred <- predict(fitModel, aspectViewTest, type = "raw")
  testPredictionMSE <- mse(aspectViewTest$diffPercent, testDiffPercentPred)
  testPredictionCorrelation <- cor(aspectViewTest$diffPercent, testDiffPercentPred)
  plot(aspectViewTest$diffPercent, testDiffPercentPred)
  cat("Test MSE: ", testPredictionMSE, "\n")
  cat("Test Actual/Predicted correlation: ", testPredictionCorrelation, "\n\n")

  #saveRDS(logisticModel, paste("./models/", symbol, "_logistic_", modelId, ".rds", sep = ""))
  return(fitModel)
}

allFeatures <- names(dailyAspects)
useFeatures1 <- allFeatures[grep('MO|ME|VE', allFeatures)]
fitModel1 <- modelTrain(
  useFeatures1, maPriceFsPeriod, maPriceSlPeriod, "1"
)

useFeatures2 <- allFeatures[grep('MO|ME|VE', allFeatures)]
fitModel2 <- modelTrain(
  useFeatures2, maPriceFsPeriod, maPriceSlPeriod, "2"
)

useFeatures3 <- allFeatures[grep('MO|ME|VE', allFeatures)]
fitModel3 <- modelTrain(
  useFeatures3, maPriceFsPeriod, maPriceSlPeriod, "3"
)

useFeatures4 <- allFeatures[grep('VE', allFeatures)]
fitModel4 <- modelTrain(
  useFeatures4, maPriceFsPeriod, maPriceSlPeriod, "4"
)

useFeatures5 <- allFeatures[grep('SU', allFeatures)]
fitModel5 <- modelTrain(
  useFeatures5, maPriceFsPeriod, maPriceSlPeriod, "5"
)

fitModel1 %>% print()
fitModel1 %>% varImp()

fitModel2 %>% print()
fitModel2 %>% varImp()

fitModel3 %>% print()
fitModel3 %>% varImp()

fitModel4 %>% print()
fitModel4 %>% varImp()

fitModel5 %>% print()
fitModel5 %>% varImp()

securityData <- mainOpenSecurity(
  symbol, maPriceFsPeriod, maPriceSlPeriod,
  "%Y-%m-%d", "2010-01-01", "2020-09-30"
)

# Filter the extreme outliers.
cat(paste("Original days rows: ", nrow(securityData)), "\n")
securityData <- securityData[abs(zdiffPercent) < zdiffPercentCut]
hist(securityData$zdiffPercent)
cat(paste("Total days rows: ", nrow(securityData)), "\n")

aspectView <- merge(
  securityData[, c('Date', 'diffPercent', 'Actbin', 'Eff')],
  dailyAspects, by = "Date"
)

selectCols <- names(aspectView)[c(-1, -2, -3)]
# Predict outcomes for all weak learner models.
aspectView$diffPred1 <- predict(fitModel1, aspectView, type = "raw")
aspectView$diffPred2 <- predict(fitModel2, aspectView, type = "raw")
aspectView$diffPred3 <- predict(fitModel3, aspectView, type = "raw")
aspectView$diffPred4 <- predict(fitModel4, aspectView, type = "raw")
aspectView$diffPred5 <- predict(fitModel5, aspectView, type = "raw")

# Train ensamble model.
trainIndex <- createDataPartition(aspectView$Actbin, p = 0.80, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

probCols <- c("diffPred1", "diffPred2", "diffPred3", "diffPred4", "diffPred5")
topModel <- train(
  x = aspectViewTrain[, ..probCols],
  y = aspectViewTrain$Actbin,
  method = "gbm",
  metric = "Kappa",
  trControl = control,
  tuneLength = 3
)

topModel %>% summary()

# Validate data predictions.
aspectViewValidate$EffPred <- predict(topModel, aspectViewValidate, type = "raw")

table(
  actual = aspectViewValidate$Actbin,
  predicted = aspectViewValidate$EffPred
) %>% caret::confusionMatrix()

# Predict outcomes for all weak learner models.
aspectViewTest$diffPred1 <- predict(fitModel1, aspectViewTest, type = "raw")
aspectViewTest$diffPred2 <- predict(fitModel2, aspectViewTest, type = "raw")
aspectViewTest$diffPred3 <- predict(fitModel3, aspectViewTest, type = "raw")
aspectViewTest$diffPred4 <- predict(fitModel4, aspectViewTest, type = "raw")
aspectViewTest$diffPred5 <- predict(fitModel5, aspectViewTest, type = "raw")
# Final ensamble prediction.
aspectViewTest$EffPred <- predict(topModel, aspectViewTest, type = "raw")

table(
  actualclass = aspectViewTest$Actbin,
  predictedclass = aspectViewTest$EffPred
) %>% caret::confusionMatrix()
#saveRDS(topModel, paste("./models/", symbol, "_logistic_ensamble", ".rds", sep = ""))

# Full data set prediction.
dailyAspects$diffPred1 <- predict(fitModel1, dailyAspects, type = "raw")
dailyAspects$diffPred2 <- predict(fitModel2, dailyAspects, type = "raw")
dailyAspects$diffPred3 <- predict(fitModel3, dailyAspects, type = "raw")
dailyAspects$diffPred4 <- predict(fitModel4, dailyAspects, type = "raw")
dailyAspects$diffPred5 <- predict(fitModel5, dailyAspects, type = "raw")
dailyAspects$EffPred <- predict(topModel, dailyAspects, type = "raw")

# Round probabilities.
dailyAspects[, diffPred1 := format(diffPred1, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, diffPred2 := format(diffPred2, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, diffPred3 := format(diffPred3, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, diffPred4 := format(diffPred4, format = "f", big.mark = ",", digits = 4)]
dailyAspects[, diffPred5 := format(diffPred5, format = "f", big.mark = ",", digits = 5)]

exportCols <- c('Date', selectCols[-1], probCols, "EffPred")
fwrite(dailyAspects[, ..exportCols], paste("~/Desktop/", symbol, "-predict-kknnLDP-ensamble", ".csv", sep = ""))
