# Title     : Daily generalized aspects and planets activation count KKNN ensamble model.
#             1) Planets MO, ME, VE, SU fast planets applying to all slow planets and CE, CH asteroids.
#             2) CV folds to 5 with 5 repeats.
#             3) Validate fit using Actbin daily price change (buy / sell) instead of Effect
#             4) Fit based on MA(2, 3) effect to smooth price variations.
#             5) Data split 80/20 proportion.
#             6) Optimize weak learners and ensamble for Kappa.
#             7) Fit weak learners for MA trend and ensamble for Actbin to generalize for daily change.
#             8) Orb to 1 degrees.
#             9) Moon phase cycle features.
#             10) Add MO to pxSelect.
#             11) Use setModernAspectsSet3 aspects that include Kepler angles.
#             12) Use aspects with asteroids CE, CH.

# CONCLUSION:
# 1) Adding Kepler aspects and reducing orb to 1 degree maintain same accuracy
#    as major/minor aspects aspect set with 4 degrees orb. Is possible that major and minor could have
#    different orb effect so when reducing orb the predictive power comes from minor and when increasing
#    from major aspects.
# 2) Adding CE/CH increase accuracy 3%.

library(boot)
library(caret)
library(gbm)
library(plyr)
library(psych)

source("./analysis.r")
source("./indicatorPlots.r")

modelId <- "kknnLDDBC"
symbol <- "DASH-USD"
maPriceFsPeriod <- 2
maPriceSlPeriod <- 3
orbLimit <- 1

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
  'CE',
  'JU',
  'SA',
  'NN',
  'UR',
  'CH',
  'NE',
  'PL'
)

idCols <- c('Date', 'Hour')
setModernAspectsSet3()
setPlanetsAll()
hourlyPlanets <- openHourlyPlanets('planets_12', clear = F)
dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)

dailyAspectsCount <- dailyAspectsGeneralizedCount(
  dailyAspects = dailyAspects,
  orbLimit = orbLimit,
  pxSelect = pxSelect,
  pySelect = pySelect
)

dailyAspectsPlanetYCount <- dailyPlanetYActivationCount(
  dailyAspects = dailyAspects,
  orbLimit = orbLimit,
  pxSelect = pxSelect,
  pySelect = pySelect
)

dailyAspects <- merge(dailyAspectsCount, dailyAspectsPlanetYCount, by = "Date")
# Month day number don't seems to have predictive power.
# dailyAspects[, DayNo := as.numeric(format(Date, "%d"))]
dailyMoonPositions <- fread("./dplanets/daily_moon_phase_positions.csv")
dailyMoonPositions[, Date := as.Date(Date)]
dailyMoonPositions[, MoonSunDis := (DISMAX + DISMIN) / 2]
dailyMoonPhase <- dailyMoonPositions[, c('Date', 'MoonSunDis', 'MoonPhaseID')]
dailyAspects <- merge(dailyAspects, dailyMoonPhase, by = "Date")

control <- trainControl(
  method = "repeatedcv",
  number = 10,
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
fitModel2 %>% print()
fitModel3 %>% print()
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

exportCols <- c('Date', selectCols[-1], probCols, "EffPred")
fwrite(dailyAspects[, ..exportCols], paste("~/Desktop/", symbol, "-predict-", modelId, "-ensamble", ".csv", sep = ""))
