# Title     : Daily generalized aspects / planet Y activation count KNN regression model
#             with GA feature selection that maximize Rsquared on train to fit for
#             daily price percent change estimation.
# Purpose   : Based on ModelLD this model has some variations:
#             1) Planets MO, ME, VE, SU fast planets applying to all slow planets and asteroids except NN.
#             2) CV folds to 5 with 5 repeats.
#             3) Validate fit using Actbin daily price change (buy / sell) instead of Effect
#             4) Fit weak learners for diff percent change and ensamble for Actbin
#                to generalize for daily change (buy / sell) signal.
#             5) Split to 80/20 proportion.
#             6) Optimize weak learners for RMSE.

library(boot)
library(caret)
library(psych)
library(gbm)
library(ModelMetrics)
source("./analysis.r")
source("./indicatorPlots.r")

symbol <- "ADA-USD"
zdiffPercentCut <- 2
maPriceFsPeriod <- 2
maPriceSlPeriod <- 3
orbLimit <- 4
kMax <- 7

pxSelectAll <- c(
  'MO',
  'ME',
  'VE',
  'SU'
)

pySelectAll <- c(
  'ME',
  'VE',
  'SU',
  'MA',
  'CE',
  'VS',
  'JU',
  'SA',
  'NN',
  'CH',
  'UR',
  'NE',
  'PL'
)

idCols <- c('Date', 'Hour')
setModernMixAspectsSet1()
setPlanetsMOMEVESUMACEVSJUNNSAURCHNEPL()
hourlyPlanets <<- openHourlyPlanets('planets_12', clear = F)
dailyAspectsRows <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)

securityData <- mainOpenSecurity(
  symbol, maPriceFsPeriod, maPriceSlPeriod,
  "%Y-%m-%d", "2010-01-01", "2020-06-30"
)

# Filter the extreme outliers.
cat(paste("Original days rows:", nrow(securityData)), "\n")
securityData <- securityData[abs(zdiffPercent) <= zdiffPercentCut]
hist(securityData$diffPercent)
cat(paste("Total days rows:", nrow(securityData)), "\n")

control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5,
  savePredictions = "all",
  verboseIter = F,
  allowParallel = T,
  trim = F
)

modelTrain <- function(pxSelect, pySelect) {
  if (count(pxSelect) == 0) {
    pxSelect <- pxSelectAll
  }

  dailyAspectsGeneralizedCount <- dailyAspectsGeneralizedCount(
    dailyAspects = dailyAspectsRows,
    orbLimit = orbLimit,
    pxSelect = pxSelect,
    pySelect = pySelect
  )

  dailyPlanetYActivationCount <- dailyPlanetYActivationCount(
    dailyAspects = dailyAspectsRows,
    orbLimit = orbLimit,
    pxSelect = pxSelect,
    pySelect = pySelect
  )

  dailyAspects <- merge(dailyAspectsGeneralizedCount, dailyPlanetYActivationCount, date = "Date")
  aspectView <- merge(
    securityData[, c('Date', 'diffPercent', 'Actbin', 'Eff')],
    dailyAspects, by = "Date"
  )

  useFeatures <- names(dailyAspects)[-1]
  selectCols <- c('diffPercent', useFeatures)
  cat("Using features:", selectCols, "\n")

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
      kmax = kMax,
      distance = 2,
      kernel = "optimal"
    )
  )

  # Validate data predictions.
  validateDiffPercentPred <- predict(fitModel, aspectViewValidate, type = "raw")
  validatePredictionMSE <- rmse(aspectViewValidate$diffPercent, validateDiffPercentPred)
  validatePredictionCorrelation <- cor(aspectViewValidate$diffPercent, validateDiffPercentPred)
  #plot(aspectViewValidate$diffPercent, validateDiffPercentPred)
  cat("Validate RMSE:", validatePredictionMSE, "\n")
  cat("Validate Actual/Predicted correlation:", validatePredictionCorrelation, "\n")

  #saveRDS(logisticModel, paste("./models/", symbol, "_logistic_", modelId, ".rds", sep = ""))
  return(fitModel)
}

findRelevantFeatures <- function(params) {
  pxSelect <- pxSelectAll[params[1:4] == 1]
  pySelect <- pySelectAll[params[5:17] == 1]
  cat("Using PX:", pxSelect, "- PY: ", pySelect, "\n")
  fitModel <- modelTrain(pxSelect, pySelect)
  cat("Fit Rsquared:", fitModel$results$Rsquared, "\n\n")

  return(fitModel$results$Rsquared)
}

gar <- ga(
  "binary",
  fitness = findRelevantFeatures,
  nBits = 17,
  names = c(
    'MOX',
    'MEX',
    'VEX',
    'SUX',
    'MEY',
    'VEY',
    'SUY',
    'MAY',
    'CEY',
    'VSY',
    'JUY',
    'SAY',
    'NNY',
    'CHY',
    'URY',
    'NEY',
    'PLY'
  ),
  popSize = 50, maxiter = 50, run = 10,
  selection = gabin_rwSelection, mutation = gabin_raMutation,
  crossover = gabin_spCrossover, population = gabin_Population,
  parallel = F, monitor = gaMonitor, keepBest = T
)

summary(gar)
print(gar@solution)
plot(gar)

# ADA Best features:
# Using PX:  ME VE - PY:  SU MA CE VS JU SA NN CH UR NE PL / R2=0.10 to 0.15
#MOX MEX VEX SUX MEY VEY SUY MAY CEY VSY JUY SAY NNY CHY URY NEY PLY
#[1,]   0   1   1   0   0   0   1   1   1   1   1   1   1   1   1   1   1

#fitModel1 <- modelTrain( "1" )
#fitModel2 <- modelTrain( "2" )
#fitModel3 <- modelTrain( "3" )
#fitModel4 <- modelTrain( "4" )
#fitModel5 <- modelTrain( "5" )
#
#securityData <- mainOpenSecurity(
#  symbol, maPriceFsPeriod, maPriceSlPeriod,
#  "%Y-%m-%d", "2010-01-01", "2020-09-30"
#)
#
## Reserved data for validation.
#securityDataTest <- mainOpenSecurity(
#  symbol, maPriceFsPeriod, maPriceSlPeriod,
#  "%Y-%m-%d", "2020-09-25"
#)
#
#aspectViewTest <- merge(
#  securityDataTest,
#  dailyAspects, by = "Date"
#)
#
## Filter the extreme outliers.
#cat(paste("Original days rows: ", nrow(securityData)), "\n")
#securityData <- securityData[abs(zdiffPercent) < zdiffPercentCut]
#hist(securityData$diffPercent)
#cat(paste("Total days rows: ", nrow(securityData)), "\n")
#
#aspectView <- merge(
#  securityData[, c('Date', 'diffPercent', 'Actbin', 'Eff')],
#  dailyAspects, by = "Date"
#)
#
#selectCols <- names(aspectView)[c(-1, -2, -3)]
## Predict outcomes for all weak learner models.
#aspectView$diffPred1 <- predict(fitModel1, aspectView, type = "raw")
#aspectView$diffPred2 <- predict(fitModel2, aspectView, type = "raw")
#aspectView$diffPred3 <- predict(fitModel3, aspectView, type = "raw")
#aspectView$diffPred4 <- predict(fitModel4, aspectView, type = "raw")
#aspectView$diffPred5 <- predict(fitModel5, aspectView, type = "raw")
#
## Train ensamble model.
#trainIndex <- createDataPartition(aspectView$Actbin, p = 0.80, list = FALSE)
#aspectViewTrain <- aspectView[trainIndex,]
#aspectViewValidate <- aspectView[-trainIndex,]
#
#probCols <- c("diffPred1", "diffPred2", "diffPred3", "diffPred4", "diffPred5")
#topModel <- train(
#  x = aspectViewTrain[, ..probCols],
#  y = aspectViewTrain$Actbin,
#  method = "gbm",
#  metric = "Kappa",
#)
#
#topModel %>% summary()
#
## Validate data predictions.
#aspectViewValidate$EffPred <- predict(topModel, aspectViewValidate, type = "raw")
#
#table(
#  actual = aspectViewValidate$Actbin,
#  predicted = aspectViewValidate$EffPred
#) %>% caret::confusionMatrix()
#
## Predict outcomes for all weak learner models.
#aspectViewTest$diffPred1 <- predict(fitModel1, aspectViewTest, type = "raw")
#aspectViewTest$diffPred2 <- predict(fitModel2, aspectViewTest, type = "raw")
#aspectViewTest$diffPred3 <- predict(fitModel3, aspectViewTest, type = "raw")
#aspectViewTest$diffPred4 <- predict(fitModel4, aspectViewTest, type = "raw")
#aspectViewTest$diffPred5 <- predict(fitModel5, aspectViewTest, type = "raw")
## Final ensamble prediction.
#aspectViewTest$EffPred <- predict(topModel, aspectViewTest, type = "raw")
#
#table(
#  actualclass = aspectViewTest$Actbin,
#  predictedclass = aspectViewTest$EffPred
#) %>% caret::confusionMatrix()
##saveRDS(topModel, paste("./models/", symbol, "_logistic_ensamble", ".rds", sep = ""))
#
## Full data set prediction.
#dailyAspects$diffPred1 <- predict(fitModel1, dailyAspects, type = "raw")
#dailyAspects$diffPred2 <- predict(fitModel2, dailyAspects, type = "raw")
#dailyAspects$diffPred3 <- predict(fitModel3, dailyAspects, type = "raw")
#dailyAspects$diffPred4 <- predict(fitModel4, dailyAspects, type = "raw")
#dailyAspects$diffPred5 <- predict(fitModel5, dailyAspects, type = "raw")
#dailyAspects$EffPred <- predict(topModel, dailyAspects, type = "raw")
#
## Round probabilities.
#dailyAspects[, diffPred1 := format(diffPred1, format = "f", big.mark = ",", digits = 3)]
#dailyAspects[, diffPred2 := format(diffPred2, format = "f", big.mark = ",", digits = 3)]
#dailyAspects[, diffPred3 := format(diffPred3, format = "f", big.mark = ",", digits = 3)]
#dailyAspects[, diffPred4 := format(diffPred4, format = "f", big.mark = ",", digits = 4)]
#dailyAspects[, diffPred5 := format(diffPred5, format = "f", big.mark = ",", digits = 5)]
#
#exportCols <- c('Date', selectCols[-1], probCols, "EffPred")
#fwrite(dailyAspects[, ..exportCols], paste("~/Desktop/", symbol, "-predict-kknnLDS-ensamble", ".csv", sep = ""))
