# Title     : Daily generalized aspects / planet Y activation count KNN regression model
#             with GA feature selection that maximize Rsquared on train data to fit for
#             daily price percent change estimation.
# Purpose   : Based on ModelLD this model has some variations:
#             1) Planets MO, ME, VE, SU fast planets applying to all slow planets and asteroids except NN.
#             2) CV folds to 5 with 3 repeats.
#             3) Split to 80/20 proportion.
#             4) Validate fit using Actbin daily price change (buy / sell) instead of Effect
#             5) GA feature detection that fit to maximize Rsquared train data.
#             6) Fit 5 weak learners for diff percent change.
#             7) Ensamble weak learnets to fit for Actbin to predict categorical (buy / sell) signal.
#             8) Optimize weak learners for RMSE.
#             9) GA feature selection popSize = 100 and iter = 20.
#            10) KKNN K param set to 7.

library(boot)
library(caret)
library(psych)
library(gbm)
library(ModelMetrics)
source("./analysis.r")
source("./indicatorPlots.r")

zdiffPercentCut <- 2
maPriceFsPeriod <- 2
maPriceSlPeriod <- 3
trainDataStartDate <- as.Date("2010-01-01")
trainDataEndDate <- as.Date("2020-08-15")
testDataStartDate <- as.Date("2020-09-01")
orbLimit <- 4
kMax <- 7
gaMaxIter <- 20
nBits <- 16
wlCVFolds <- 5
wlCVRepeats <- 5
enCVFolds <- 10
enCVRepeats <- 10

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
  #'NN',
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

control <- trainControl(
  method = "repeatedcv",
  number = wlCVFolds,
  repeats = wlCVRepeats,
  savePredictions = "all",
  verboseIter = F,
  allowParallel = T,
  trim = F
)

searchModel <- function(symbol) {
  cat("\nProcessing", symbol, "model search...\n")
  securityData <- mainOpenSecurity(
    symbol, maPriceFsPeriod, maPriceSlPeriod,
    "%Y-%m-%d", trainDataStartDate, trainDataEndDate
  )

  # Filter the extreme outliers.
  cat(paste("Original days observations rows:", nrow(securityData)), "\n")
  securityData <- securityData[abs(zdiffPercent) <= zdiffPercentCut]
  hist(securityData$diffPercent)
  cat(paste("Post filter days observations rows:", nrow(securityData)), "\n\n")

  prepareDailyAspects <- function(pxSelect, pySelect) {
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

    return(dailyAspects)
  }

  modelTrain <- function(pxSelect, pySelect) {
    cat("Using PX:", pxSelect, "- PY:", pySelect, "\n")

    if (count(pxSelect) == 0) {
      pxSelect <- pxSelectAll
    }

    if (count(pySelect) < 3) {
      pySelect <- pySelectAll
    }

    dailyAspects <- prepareDailyAspects(pxSelect, pySelect)
    aspectView <- merge(
      securityData[, c('Date', 'diffPercent', 'Actbin', 'Eff')],
      dailyAspects, by = "Date"
    )

    useFeatures <- names(dailyAspects)[-1]
    selectCols <- c('diffPercent', useFeatures)
    #cat("Selected cols:", selectCols, "\n")
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
    validateMAE <- mae(aspectViewValidate$diffPercent, validateDiffPercentPred)
    validateRMSE <- rmse(aspectViewValidate$diffPercent, validateDiffPercentPred)
    validateR2 <- cor(aspectViewValidate$diffPercent, validateDiffPercentPred)^2
    cat("Validate MAE:", validateMAE, "RMSE:", validateRMSE, "R2:", validateR2, "\n")

    trainMAE <- fitModel$results$MAE
    trainRMSE <- fitModel$results$RMSE
    trainR2 <- fitModel$results$Rsquared
    cat("Train MAE:", trainMAE, "RMSE:", trainRMSE, "R2:", trainR2, "\n\n")

    return(fitModel)
  }

  parseSolutionParameters <- function(solution) {
    pxSelect <- pxSelectAll[solution[1:4] == 1]
    pySelect <- pySelectAll[solution[5:nBits] == 1]

    return(list(
      pxSelect = pxSelect,
      pySelect = pySelect
    ))
  }

  findRelevantFeatures <- function(solution) {
    params <- parseSolutionParameters(solution)
    fitModel <- modelTrain(params$pxSelect, params$pySelect)

    #return(fitModel$results$RMSE)
    #return(fitModel$results$MAE)
    return(fitModel$results$Rsquared)
  }

  solutionModelTrain <- function(params) {
    fitModel <- modelTrain(params$pxSelect, params$pySelect)

    return(fitModel)
  }

  gar <- ga(
    "binary",
    fitness = findRelevantFeatures,
    nBits = nBits,
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
      'CHY',
      'URY',
      'NEY',
      'PLY'
    ),
    popSize = 100, maxiter = gaMaxIter, run = 20,
    selection = gabin_rwSelection, mutation = gabin_raMutation,
    crossover = gabin_spCrossover, population = gabin_Population,
    parallel = F, monitor = gaMonitor, keepBest = T
  )

  cat("\n")
  summary(gar) %>% print()

  params <- parseSolutionParameters(gar@solution)
  fitModel1 <- solutionModelTrain(params)
  fitModel1 %>% varImp() %>% print()

  fitModel2 <- solutionModelTrain(params)
  fitModel2 %>% varImp() %>% print()

  fitModel3 <- solutionModelTrain(params)
  fitModel3 %>% varImp() %>% print()

  fitModel4 <- solutionModelTrain(params)
  fitModel4 %>% varImp() %>% print()

  fitModel5 <- solutionModelTrain(params)
  fitModel5 %>% varImp() %>% print()

  dailyAspects <- prepareDailyAspects(params$pxSelect, params$pySelect)
  aspectView <- merge(
    securityData[, c('Date', 'diffPercent', 'Actbin', 'Eff')],
    dailyAspects, by = "Date"
  )

  # Predict outcomes for all weak learner models.
  aspectView$DiffPred1 <- predict(fitModel1, aspectView, type = "raw")
  aspectView$DiffPred2 <- predict(fitModel2, aspectView, type = "raw")
  aspectView$DiffPred3 <- predict(fitModel3, aspectView, type = "raw")
  aspectView$DiffPred4 <- predict(fitModel4, aspectView, type = "raw")
  aspectView$DiffPred5 <- predict(fitModel5, aspectView, type = "raw")

  # Train ensamble model.
  trainIndex <- createDataPartition(aspectView$Actbin, p = 0.80, list = FALSE)
  aspectViewTrain <- aspectView[trainIndex,]
  aspectViewValidate <- aspectView[-trainIndex,]

  probCols <- c("DiffPred1", "DiffPred2", "DiffPred3", "DiffPred4", "DiffPred5")
  topModel <- train(
    x = aspectViewTrain[, ..probCols],
    y = aspectViewTrain$Actbin,
    method = "gbm",
    metric = "Kappa",
    trControl = trainControl(
      method = "repeatedcv",
      number = enCVFolds,
      repeats = enCVRepeats,
      savePredictions = "all",
      classProbs = T,
      verboseIter = F,
      allowParallel = T,
      trim = F
    ),
    tuneLength = 3,
    verbose = F
  )

  topModel %>% summary() %>% print()

  # Validate data predictions.
  aspectViewValidate$EffPred <- predict(topModel, aspectViewValidate, type = "raw")

  table(
    actual = aspectViewValidate$Actbin,
    predicted = aspectViewValidate$EffPred
  ) %>% caret::confusionMatrix() %>% print()

  # Reserved data for validation.
  securityDataTest <- mainOpenSecurity(
    symbol, maPriceFsPeriod, maPriceSlPeriod,
    "%Y-%m-%d", testDataStartDate
  )

  aspectViewTest <- merge(
    securityDataTest[, c('Date', 'Actbin', 'Eff')],
    dailyAspects,
    by = "Date"
  )

  # Predict outcomes for all weak learner models.
  aspectViewTest$DiffPred1 <- predict(fitModel1, aspectViewTest, type = "raw")
  aspectViewTest$DiffPred2 <- predict(fitModel2, aspectViewTest, type = "raw")
  aspectViewTest$DiffPred3 <- predict(fitModel3, aspectViewTest, type = "raw")
  aspectViewTest$DiffPred4 <- predict(fitModel4, aspectViewTest, type = "raw")
  aspectViewTest$DiffPred5 <- predict(fitModel5, aspectViewTest, type = "raw")
  # Final ensamble prediction.
  aspectViewTest$EffPred <- predict(topModel, aspectViewTest, type = "raw")

  cat(symbol, "MODEL RESULTS:\n")
  testResult <- table(
    actualclass = aspectViewTest$Actbin,
    predictedclass = aspectViewTest$EffPred
  ) %>% caret::confusionMatrix()
  print(testResult)
  #saveRDS(topModel, paste("./models/", symbol, "_logistic_ensamble", ".rds", sep = ""))

  # Full data set prediction.
  dailyAspects$DiffPred1 <- predict(fitModel1, dailyAspects, type = "raw")
  dailyAspects$DiffPred2 <- predict(fitModel2, dailyAspects, type = "raw")
  dailyAspects$DiffPred3 <- predict(fitModel3, dailyAspects, type = "raw")
  dailyAspects$DiffPred4 <- predict(fitModel4, dailyAspects, type = "raw")
  dailyAspects$DiffPred5 <- predict(fitModel5, dailyAspects, type = "raw")
  # Ensamble buy probability and class prediction.
  dailyAspects$BuyProb <- predict(topModel, dailyAspects, type = "prob")$buy
  dailyAspects$EffPred <- predict(topModel, dailyAspects, type = "raw")

  # Round probabilities.
  dailyAspects[, DiffPred1 := format(DiffPred1, format = "f", big.mark = ",", digits = 2)]
  dailyAspects[, DiffPred2 := format(DiffPred2, format = "f", big.mark = ",", digits = 2)]
  dailyAspects[, DiffPred3 := format(DiffPred3, format = "f", big.mark = ",", digits = 2)]
  dailyAspects[, DiffPred4 := format(DiffPred4, format = "f", big.mark = ",", digits = 2)]
  dailyAspects[, DiffPred5 := format(DiffPred5, format = "f", big.mark = ",", digits = 2)]
  dailyAspects[, BuyProb := format(BuyProb, format = "f", big.mark = ",", digits = 2)]

  aspectsCols <- names(aspectView)[-seq(2, 4)]
  exportCols <- c(aspectsCols, "EffPred")
  fwrite(dailyAspects[, ..exportCols], paste("./predictions/", symbol, "-predict-ensamble-gakknn-MA", ".csv", sep = ""))

  return(
    list(symbol=symbol, results=testResult)
  )
}

listFilePath <- npath(paste("./symbols/working.csv", sep=""))
symbolsList <- read.csv(listFilePath, header=F, stringsAsFactors=F)
testResults <- lapply(symbolsList$V1, searchModel)
cat("\nMODEL TEST RESULTS:\n")

for(idx in 1:count(testResults)) {
  cat(testResults[[idx]]$symbol, "TEST RESULT:\n")
  print(testResults[[idx]]$results)
}