# Title     : Daily aspects factors table KNN model with CV control.
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
source("./analysis.r")
source("./indicatorPlots.r")

symbol <- "ADA-USD"
maPriceFsPeriod <- 2
maPriceSlPeriod <- 3
orbLimit <- 2

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

dailyAspects <- dailyCombPlanetAspectsFactorsTableLI(
  orbLimit = orbLimit,
  pxSelect = pxSelect,
  pySelect = pySelect
)

control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  savePredictions = "all",
  classProbs = T,
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

modelTrain <- function(method, useFeatures, maPriceFsPeriod, maPriceSlPeriod, modelId) {
  securityData <- mainOpenSecurity(
    symbol, maPriceFsPeriod, maPriceSlPeriod,
    "%Y-%m-%d", "2010-01-01", "2020-06-30"
  )

  # Filter the extreme outliers.
  cat(paste("Original days rows: ", nrow(securityData)), "\n")
  securityData <- securityData[abs(zdiffPercent) < 3]
  hist(securityData$zdiffPercent)
  cat(paste("Total days rows: ", nrow(securityData)), "\n")

  aspectView <- merge(
    securityData[, c('Date', 'diffPercent', 'Actbin', 'Eff')],
    dailyAspects, by = "Date"
  )

  selectCols <- c('Eff', useFeatures)
  cat("Using features: ", selectCols)
  trainIndex <- createDataPartition(aspectView$Eff, p = 0.80, list = FALSE)
  aspectViewTrain <- aspectView[trainIndex,]
  aspectViewValidate <- aspectView[-trainIndex,]
  fitModel <- train(
    formula(Eff ~ .),
    data = aspectViewTrain[, ..selectCols],
    method = method,
    metric = "Kappa",
    trControl = control,
    tuneGrid = expand.grid(
      kmax = 7,
      distance = 2,
      kernel = "optimal"
    )
  )

  # Validate data predictions.
  validateEffPred <- predict(fitModel, aspectViewValidate, type = "raw")
  aspectViewValidate$EffPred <- mapvalues(validateEffPred, from = c("up", "down"), to = c("buy", "sell"))

  validateResult <- table(
    actualclass = as.character(aspectViewValidate$Actbin),
    predictedclass = as.character(aspectViewValidate$EffPred)
  ) %>%
    confusionMatrix()

  validateAccuracy <- validateResult$overall['Accuracy']
  validatePrevalence <- validateResult$byClass['Prevalence']
  cat("Accuracy: ", validateAccuracy, "Prevalence: ", validatePrevalence, "\n")

  testEffPred <- predict(fitModel, aspectViewTest, type = "raw")
  aspectViewTest$EffPred <- mapvalues(testEffPred, from = c("up", "down"), to = c("buy", "sell"))

  testResult <- table(
    actualclass = as.character(aspectViewTest$Actbin),
    predictedclass = as.character(aspectViewTest$EffPred)
  ) %>%
    confusionMatrix()

  testAccuracy <- testResult$overall['Accuracy']
  testPrevalence <- testResult$byClass['Prevalence']
  cat("Accuracy: ", testAccuracy, "Prevalence: ", testPrevalence, "\n")

  #saveRDS(logisticModel, paste("./models/", symbol, "_logistic_", modelId, ".rds", sep = ""))
  return(fitModel)
}

allFeatures <- names(dailyAspects)
useFeatures1 <- allFeatures[grep('ME|VE', allFeatures)]
fitModel1 <- modelTrain(
  "kknn", useFeatures1, maPriceFsPeriod, maPriceSlPeriod, "1"
)

useFeatures2 <- allFeatures[grep('ME|VE', allFeatures)]
fitModel2 <- modelTrain(
  "kknn", useFeatures2, maPriceFsPeriod, maPriceSlPeriod, "2"
)

useFeatures3 <- allFeatures[grep('MO|ME|VE', allFeatures)]
fitModel3 <- modelTrain(
  "kknn", useFeatures3, maPriceFsPeriod, maPriceSlPeriod, "3"
)

useFeatures4 <- allFeatures[grep('MO|ME|VE', allFeatures)]
fitModel4 <- modelTrain(
  "kknn", useFeatures4, maPriceFsPeriod, maPriceSlPeriod, "4"
)

useFeatures5 <- allFeatures[grep('ME|VE|SU', allFeatures)]
fitModel5 <- modelTrain(
  "kknn", useFeatures5, maPriceFsPeriod, maPriceSlPeriod, "5"
)

useFeatures6 <- allFeatures[grep('ME|VE|SU', allFeatures)]
fitModel6 <- modelTrain(
  "kknn", useFeatures6, maPriceFsPeriod, maPriceSlPeriod, "6"
)

useFeatures7 <- allFeatures[grep('MO|ME|VE|SU', allFeatures)]
fitModel7 <- modelTrain(
  "kknn", useFeatures7, maPriceFsPeriod, maPriceSlPeriod, "7"
)

useFeatures8 <- allFeatures[grep('MO|ME|VE|SU', allFeatures)]
fitModel8 <- modelTrain(
  "kknn", useFeatures8, maPriceFsPeriod, maPriceSlPeriod, "8"
)

useFeatures9 <- allFeatures[grep('VE|SU', allFeatures)]
fitModel9 <- modelTrain(
  "kknn", useFeatures9, maPriceFsPeriod, maPriceSlPeriod, "9"
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

fitModel6 %>% print()
fitModel6 %>% varImp()

fitModel7 %>% print()
fitModel7 %>% varImp()

fitModel8 %>% print()
fitModel8 %>% varImp()

fitModel9 %>% print()
fitModel9 %>% varImp()

securityData <- mainOpenSecurity(
  symbol, maPriceFsPeriod, maPriceSlPeriod,
  "%Y-%m-%d", "2010-01-01", "2020-09-30"
)

# Filter the extreme outliers.
cat(paste("Original days rows: ", nrow(securityData)), "\n")
securityData <- securityData[abs(zdiffPercent) < 3]
hist(securityData$zdiffPercent)
cat(paste("Total days rows: ", nrow(securityData)), "\n")

aspectView <- merge(
  securityData[, c('Date', 'diffPercent', 'Actbin', 'Eff')],
  dailyAspects, by = "Date"
)

selectCols <- names(aspectView)[c(-1, -2, -3)]
# Predict outcomes for all weak learner models.
aspectView$EffUpP1 <- predict(fitModel1, aspectView, type = "prob")$up
aspectView$EffUpP2 <- predict(fitModel2, aspectView, type = "prob")$up
aspectView$EffUpP3 <- predict(fitModel3, aspectView, type = "prob")$up
aspectView$EffUpP4 <- predict(fitModel4, aspectView, type = "prob")$up
aspectView$EffUpP5 <- predict(fitModel5, aspectView, type = "prob")$up
aspectView$EffUpP6 <- predict(fitModel6, aspectView, type = "prob")$up
aspectView$EffUpP7 <- predict(fitModel7, aspectView, type = "prob")$up
aspectView$EffUpP8 <- predict(fitModel8, aspectView, type = "prob")$up
aspectView$EffUpP9 <- predict(fitModel9, aspectView, type = "prob")$up

# Train ensamble model.
trainIndex <- createDataPartition(aspectView$Actbin, p = 0.80, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

probCols <- c("EffUpP1", "EffUpP2", "EffUpP3", "EffUpP4", "EffUpP5", "EffUpP6", "EffUpP7", "EffUpP8", "EffUpP9")
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
  actualclass = as.character(aspectViewValidate$Actbin),
  predictedclass = as.character(aspectViewValidate$EffPred)
) %>%
  confusionMatrix() %>%
  print()

# Predict outcomes for all weak learner models.
aspectViewTest$EffUpP1 <- predict(fitModel1, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP2 <- predict(fitModel2, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP3 <- predict(fitModel3, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP4 <- predict(fitModel4, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP5 <- predict(fitModel5, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP6 <- predict(fitModel6, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP7 <- predict(fitModel7, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP8 <- predict(fitModel8, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP9 <- predict(fitModel9, aspectViewTest, type = "prob")$up
# Final ensamble prediction.
aspectViewTest$EffPred <- predict(topModel, aspectViewTest, type = "raw")

table(
  actualclass = as.character(aspectViewTest$Actbin),
  predictedclass = as.character(aspectViewTest$EffPred)
) %>%
  confusionMatrix() %>%
  print()

#saveRDS(topModel, paste("./models/", symbol, "_logistic_ensamble", ".rds", sep = ""))

# Full data set prediction.
dailyAspects$EffUpP1 <- predict(fitModel1, dailyAspects, type = "prob")$up
dailyAspects$EffUpP2 <- predict(fitModel2, dailyAspects, type = "prob")$up
dailyAspects$EffUpP3 <- predict(fitModel3, dailyAspects, type = "prob")$up
dailyAspects$EffUpP4 <- predict(fitModel4, dailyAspects, type = "prob")$up
dailyAspects$EffUpP5 <- predict(fitModel5, dailyAspects, type = "prob")$up
dailyAspects$EffUpP6 <- predict(fitModel6, dailyAspects, type = "prob")$up
dailyAspects$EffUpP7 <- predict(fitModel7, dailyAspects, type = "prob")$up
dailyAspects$EffUpP8 <- predict(fitModel8, dailyAspects, type = "prob")$up
dailyAspects$EffUpP9 <- predict(fitModel9, dailyAspects, type = "prob")$up
dailyAspects$EffPred <- predict(topModel, dailyAspects, type = "raw")

# Round probabilities.
dailyAspects[, EffUpP1 := format(EffUpP1, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP2 := format(EffUpP2, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP3 := format(EffUpP3, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP4 := format(EffUpP4, format = "f", big.mark = ",", digits = 4)]
dailyAspects[, EffUpP5 := format(EffUpP5, format = "f", big.mark = ",", digits = 5)]
dailyAspects[, EffUpP6 := format(EffUpP6, format = "f", big.mark = ",", digits = 6)]
dailyAspects[, EffUpP7 := format(EffUpP7, format = "f", big.mark = ",", digits = 7)]
dailyAspects[, EffUpP8 := format(EffUpP8, format = "f", big.mark = ",", digits = 8)]
dailyAspects[, EffUpP9 := format(EffUpP9, format = "f", big.mark = ",", digits = 9)]

exportCols <- c('Date', selectCols[-1], probCols, "EffPred")
fwrite(dailyAspects[, ..exportCols], paste("./predictions/", symbol, "-predict-kknnLDO-ensamble", ".csv", sep = ""))
