# Title     : Daily planets longitude distance harmonic 2.
#             1) Planets MO, ME, VE, SU fast planets applying to all slow planets, NN, SN and asteroids.
#             2) CV folds to 5 with 5 repeats.
#             3) Validate fit using Actbin daily price change (buy / sell) instead of Effect
#             4) Fit based on MA(2, 3) effect to smooth price variations.
#             5) Data split 80/20 proportion.
#             6) Optimize weak learners and ensamble for Kappa.
#             7) Fit weak learners for MA trend and ensamble for Actbin to generalize for daily change.
#             8) Use setModernAspectsSet3 aspects that include Kepler angles.
#             9) Use harmonics with NN/SN and asteroids CE, JN, CH, PH.
#            10) Use SULON to allow model detect Sun zodiac position.

# CONCLUSION:

library(boot)
library(caret)
library(gbm)
library(magrittr)
library(memoise)
library(plyr)
library(psych)

source("./analysis.r")
source("./indicatorPlots.r")

modelId <- "kknnLDDBF"
symbol <- "BTC-USD"
maPriceFsPeriod <- 2
maPriceSlPeriod <- 3

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
  #'VS',
  #'CE',
  #'JN',
  #'PA',
  'JU',
  'SA',
  'NN',
  'SN',
  'UR',
  #'CH',
  #'PH',
  'NE',
  'PL'
)

idCols <- c('Date', 'Hour')
setModernAspectsSet3()
setPlanetsAll()
memoOpenHourlyPlanets <- memoise(openHourlyPlanets)
hourlyPlanets <- memoOpenHourlyPlanets('planets_12')

# Angular harmonics.
planetsCombLonH2 <- paste0(planetsCombLon, 'H2')
planetsCombLonDis <- paste0(planetsCombLon, 'DIS')
hourlyPlanets[,
  c(planetsCombLonH2) :=
    lapply(.SD, function(x) distanceHarmonic(x,  2)), .SDcols = planetsCombLonDis
]

planetsCombLonH3 <- paste0(planetsCombLon, 'H3')
planetsCombLonDis <- paste0(planetsCombLon, 'DIS')
hourlyPlanets[,
  c(planetsCombLonH3) :=
    lapply(.SD, function(x) distanceHarmonicAbs(x,  3)), .SDcols = planetsCombLonDis
]

planetsCombLonH4 <- paste0(planetsCombLon, 'H4')
planetsCombLonDis <- paste0(planetsCombLon, 'DIS')
hourlyPlanets[,
  c(planetsCombLonH4) :=
    lapply(.SD, function(x) distanceHarmonicAbs(x,  4)), .SDcols = planetsCombLonDis
]

planetsCombLonH5 <- paste0(planetsCombLon, 'H5')
hourlyPlanets[,
  c(planetsCombLonH5) :=
    lapply(.SD, function(x) distanceHarmonicAbs(x,  5)), .SDcols = planetsCombLonDis
]

# Keep pos/neg range for better result.
planetsCombLonH6 <- paste0(planetsCombLon, 'H6')
hourlyPlanets[,
  c(planetsCombLonH6) :=
    lapply(.SD, function(x) distanceHarmonic(x,  6)), .SDcols = planetsCombLonDis
]

planetsCombLonH7 <- paste0(planetsCombLon, 'H7')
hourlyPlanets[,
  c(planetsCombLonH7) :=
    lapply(.SD, function(x) distanceHarmonicAbs(x,  7)), .SDcols = planetsCombLonDis
]

planetsCombLonH8 <- paste0(planetsCombLon, 'H8')
hourlyPlanets[,
  c(planetsCombLonH8) :=
    lapply(.SD, function(x) distanceHarmonicAbs(x,  8)), .SDcols = planetsCombLonDis
]

planetsCombLonH9 <- paste0(planetsCombLon, 'H9')
hourlyPlanets[,
  c(planetsCombLonH9) :=
    lapply(.SD, function(x) distanceHarmonicAbs(x,  9)), .SDcols = planetsCombLonDis
]

# Keep pos/neg range for better result.
planetsCombLonH10 <- paste0(planetsCombLon, 'H10')
hourlyPlanets[,
  c(planetsCombLonH10) :=
    lapply(.SD, function(x) distanceHarmonic(x,  10)), .SDcols = planetsCombLonDis
]

# Keep pos/neg range for better result.
planetsCombLonH11 <- paste0(planetsCombLon, 'H11')
hourlyPlanets[,
  c(planetsCombLonH11) :=
    lapply(.SD, function(x) distanceHarmonic(x,  11)), .SDcols = planetsCombLonDis
]

planetsCombLonH12 <- paste0(planetsCombLon, 'H12')
hourlyPlanets[,
  c(planetsCombLonH12) :=
    lapply(.SD, function(x) distanceHarmonicAbs(x,  12)), .SDcols = planetsCombLonDis
]

# Longitude harmonics.
planetsLonH2 <- paste0(planetsLonCols, 'H2')
hourlyPlanets[,
  c(planetsLonH2) :=
    lapply(.SD, function(x) distanceHarmonic(x,  2)), .SDcols = planetsLonCols
]

planetsLonH3 <- paste0(planetsLonCols, 'H3')
hourlyPlanets[,
  c(planetsLonH3) :=
    lapply(.SD, function(x) distanceHarmonic(x,  3)), .SDcols = planetsLonCols
]

columnNames <- colnames(hourlyPlanets)
planetPairsPattern <- expand.grid(pxSelect, pySelect) %>%
  with(paste0('^', Var1, Var2, 'LONH11$')) %>%
  str_flatten(collapse = "|")
selectColumns <- c(
  columnNames[grep(planetPairsPattern, columnNames)]
)
dailyAspects <- hourlyPlanets[, lapply(.SD, mean), .SDcols = selectColumns, by = 'Date']

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
      kmax = 12,
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

allFeatures <- names(dailyAspects)[-1]
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
  dailyAspects, by = "Date"
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
dailyAspects$EffPred <- predict(topModel, dailyAspects, type = "raw")

# Round probabilities.
dailyAspects[, EffUpP1 := format(EffUpP1, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP2 := format(EffUpP2, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP3 := format(EffUpP3, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP4 := format(EffUpP4, format = "f", big.mark = ",", digits = 4)]
dailyAspects[, EffUpP5 := format(EffUpP5, format = "f", big.mark = ",", digits = 5)]

#exportCols <- c('Date', selectCols[-1], probCols, "EffPred")
#fwrite(dailyAspects[, ..exportCols], paste("~/Desktop/", symbol, "-predict-", modelId, "-ensamble", ".csv", sep = ""))
