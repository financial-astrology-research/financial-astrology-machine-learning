# Title     : Daily planets longitude harmonics.
#             1) Planets MO, ME, VE, SU, MA, CE, VS, JU, SA, UR, NE, PL harmonic 3, 4, 6, 8.
#             2) CV folds to 10 with 5 repeats.
#             3) Validate fit using Actbin daily price change (buy / sell) instead of Effect
#             4) Fit based on MA(2, 4) effect to smooth price variations.
#             5) Data split 80/20 proportion.
#             6) Optimize weak learners and ensamble for Kappa.
#             7) Fit weak learners for MA trend and ensamble for Actbin to generalize for daily change.
#             8) Use harmonics with NN/SN and asteroids CE.

# CONCLUSION:
# - Longitude harmonics features alone don't have predictive power due the fact that only contain the energy quality.

library(boot)
library(caret)
library(gbm)
library(magrittr)
library(memoise)
library(plyr)
library(psych)

source("./analysis.r")
source("./indicatorPlots.r")

modelId <- "kknnLDDBFD"
maPriceFsPeriod <- 2
maPriceSlPeriod <- 4

pLonSelect <- c(
  'MO',
  'ME',
  'VE',
  'SU',
  'MA',
  'CE',
  'VS',
  'JU',
  'SA',
  'UR',
  'NE',
  'PL'
)

harmonicsSelect <- c(3, 4, 6, 8)

idCols <- c('Date', 'Hour')
setModernAspectsSet3()
setPlanetsAll()
memoOpenHourlyPlanets <- memoise(openHourlyPlanets)
hourlyPlanets <- memoOpenHourlyPlanets('planets_12')

for (harmonic in harmonicsSelect) {
  planetsLonH <- paste0(planetsLonCols, 'H', harmonic)
  hourlyPlanets[,
    c(planetsLonH) :=
      lapply(.SD, function(x) distanceHarmonic(x, harmonic)), .SDcols = planetsLonCols
  ]
}

columnNames <- colnames(hourlyPlanets)
longitudeHarmonicsColumns <- columnNames[grep(str_flatten(paste0('^', pLonSelect, 'LONH[0-9]', '$'), "|"), columnNames)]
selectColumns <- longitudeHarmonicsColumns
dailyLongitudes <- hourlyPlanets[, lapply(.SD, mean), .SDcols = selectColumns, by = 'Date']

control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  savePredictions = "all",
  classProbs = T,
  verboseIter = F,
  allowParallel = T,
  trim = F
)

searchModel <- function(symbol) {
  # Reserved data for validation.
  securityDataTest <- mainOpenSecurity(
    symbol, maPriceFsPeriod, maPriceSlPeriod,
    "%Y-%m-%d", "2020-09-25"
  )

  aspectViewTest <- merge(
    securityDataTest,
    dailyLongitudes, by = "Date"
  )

  modelTrain <- function(method, useFeatures, maPriceFsPeriod, maPriceSlPeriod, modelId) {
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
      dailyLongitudes, by = "Date"
    )

    selectCols <- c('Eff', useFeatures)
    cat("Using features: ", selectCols, "\n")
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
        kmax = 11,
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

  allFeatures <- names(dailyLongitudes)[-1]
  fitModel1 <- modelTrain(
    "kknn", allFeatures, maPriceFsPeriod, maPriceSlPeriod, "1"
  )

  fitModel2 <- modelTrain(
    "kknn", allFeatures, maPriceFsPeriod, maPriceSlPeriod, "2"
  )

  fitModel3 <- modelTrain(
    "kknn", allFeatures, maPriceFsPeriod, maPriceSlPeriod, "3"
  )

  fitModel4 <- modelTrain(
    "kknn", allFeatures, maPriceFsPeriod, maPriceSlPeriod, "4"
  )

  fitModel5 <- modelTrain(
    "kknn", allFeatures, maPriceFsPeriod, maPriceSlPeriod, "5"
  )

  fitModel1 %>% print()
  fitModel1 %>% varImp()
  fitModel2 %>% print()
  fitModel2 %>% varImp()
  fitModel3 %>% print()
  fitModel3 %>% varImp()
  fitModel4 %>% print()
  fitModel5 %>% print()

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
    dailyLongitudes, by = "Date"
  )

  selectCols <- names(aspectView)[c(-1, -2, -3)]
  # Predict outcomes for all weak learner models.
  aspectView$EffUpP1 <- predict(fitModel1, aspectView, type = "prob")$up
  aspectView$EffUpP2 <- predict(fitModel2, aspectView, type = "prob")$up
  aspectView$EffUpP3 <- predict(fitModel3, aspectView, type = "prob")$up
  aspectView$EffUpP4 <- predict(fitModel4, aspectView, type = "prob")$up
  aspectView$EffUpP5 <- predict(fitModel5, aspectView, type = "prob")$up

  # Train ensamble model.
  trainIndex <- createDataPartition(aspectView$Actbin, p = 0.80, list = FALSE)
  aspectViewTrain <- aspectView[trainIndex,]
  aspectViewValidate <- aspectView[-trainIndex,]

  probCols <- c('EffUpP1', 'EffUpP2', 'EffUpP3', 'EffUpP4', 'EffUpP5')
  topModel <- train(
    x = aspectViewTrain[, ..probCols],
    y = aspectViewTrain$Actbin,
    method = "gbm",
    metric = "Kappa",
    trControl = control,
    tuneLength = 3,
    verbose = F
  )

  topModel %>% summary()

  # Validate data predictions.
  aspectViewValidate$EffPred <- predict(topModel, aspectViewValidate, type = "raw")

  cat("\n", symbol, "MODEL VALIDATE RESULT:\n")
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
  # Final ensamble prediction.
  aspectViewTest$EffPred <- predict(topModel, aspectViewTest, type = "raw")

  cat("\n", symbol, "MODEL TEST RESULT:\n")
  testResult <- table(
    actualclass = as.character(aspectViewTest$Actbin),
    predictedclass = as.character(aspectViewTest$EffPred)
  ) %>% confusionMatrix()
  print(testResult)

  #saveRDS(topModel, paste("./models/", symbol, "_logistic_ensamble", ".rds", sep = ""))

  # Full data set prediction.
  dailyLongitudes$EffUpP1 <- predict(fitModel1, dailyLongitudes, type = "prob")$up
  dailyLongitudes$EffUpP2 <- predict(fitModel2, dailyLongitudes, type = "prob")$up
  dailyLongitudes$EffUpP3 <- predict(fitModel3, dailyLongitudes, type = "prob")$up
  dailyLongitudes$EffUpP4 <- predict(fitModel4, dailyLongitudes, type = "prob")$up
  dailyLongitudes$EffUpP5 <- predict(fitModel5, dailyLongitudes, type = "prob")$up
  dailyLongitudes$EffPred <- predict(topModel, dailyLongitudes, type = "raw")

  # Round probabilities.
  dailyLongitudes[, EffUpP1 := format(EffUpP1, format = "f", big.mark = ",", digits = 3)]
  dailyLongitudes[, EffUpP2 := format(EffUpP2, format = "f", big.mark = ",", digits = 3)]
  dailyLongitudes[, EffUpP3 := format(EffUpP3, format = "f", big.mark = ",", digits = 3)]
  dailyLongitudes[, EffUpP4 := format(EffUpP4, format = "f", big.mark = ",", digits = 4)]
  dailyLongitudes[, EffUpP5 := format(EffUpP5, format = "f", big.mark = ",", digits = 5)]

  exportCols <- c('Date', selectCols[-1], probCols, "EffPred")
  fwrite(dailyLongitudes[, ..exportCols], paste("~/Desktop/ModelsPred/", symbol, "-predict-", modelId, "-ensamble", ".csv", sep = ""))

  return(
    list(symbol = symbol, results = testResult)
  )
}

listFilePath <- npath(paste("~/Sites/own/astro-trading/hisdata/symbols/working.csv", sep = ""))
symbolsList <- read.csv(listFilePath, header = F, stringsAsFactors = F)
testResults <- lapply(symbolsList$V1, searchModel)

cat("\nMODEL SEARCH SUMMARY:\n\n")
for (idx in 1:count(testResults)) {
  cat(testResults[[idx]]$symbol, "TEST RESULT:\n")
  print(testResults[[idx]]$results)
}
