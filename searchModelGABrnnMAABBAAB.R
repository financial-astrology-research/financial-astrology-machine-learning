# Title     : Daily generalized aspects / planet Y activation count brnn regression model
#             with GA feature selection that maximize Rsquared on train data to fit for
#             daily price percent change estimation.
# Purpose   : Based on ModelLD this model has some variations:
#             1) Planets MO, ME, VE, SU fast planets applying to all slow planets.
#             2) CV folds to 5 with 1 repeats for weak learners, folds to 5 with 10 repeats for ensamble.
#             3) Split to 80/20 proportion.
#             4) Validate fit using Actbin daily price change (buy / sell) instead of Effect
#             5) GA feature detection that fit to maximize Rsquared train data.
#             6) Fit 5 weak learners for diff percent change.
#             7) Ensamble weak learnets to fit for Actbin to predict categorical (buy / sell) signal.
#             8) Optimize weak learners for RMSE.
#             9) GA feature selection popSize = 100 and iter = 10.
#            10) Fit using multi train sample mean metric penalized by standard deviation.
#            11) GA search orb limit 2-4 degrees.
#            12) Z diff percent cut to 3.
#            13) Remove pxSelect from GA search.

library(boot)
library(caret)
library(psych)
library(gbm)
library(ModelMetrics)
library(zeallot)
source("./analysis.r")
source("./indicatorPlots.r")

modelId <- "ensamble-ga-brnn-MAABBAAB"
zdiffPercentCut <- 3
maPriceFsPeriod <- 2
maPriceSlPeriod <- 3
trainDataStartDate <- as.Date("2010-01-01")
trainDataEndDate <- as.Date("2020-08-15")
testDataStartDate <- as.Date("2020-09-01")
kMax <- 7
gaPopSize <- 100
gaMaxIter <- 10
gaParamsNum <- 10
wlCVFolds <- 5
wlCVRepeats <- 1
enCVFolds <- 5
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
  #'CE',
  #'VS',
  'JU',
  'SA',
  #'NN',
  #'CH',
  'UR',
  'NE',
  'PL'
)

aspectsSelectAll <- c(
  0,
  30,
  45,
  60,
  90,
  103,
  120,
  135,
  150,
  180
)

idCols <- c('Date', 'Hour')
setModernMixAspectsSet1()
setPlanetsMOMEVESUMACEVSJUNNSAURCHNEPL()
hourlyPlanets <<- openHourlyPlanets('planets_12', clear = F)
dailyAspectsRows <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, 4)

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

  prepareModelData <- function(params) {
    c(pxSelect, pySelect, orbLimit) %<-% params
    dailyAspects <- prepareDailyAspectsCountPlanetYActivationCount(
      dailyAspectsRows, orbLimit, pxSelect, pySelect, aspectsSelectAll
    )

    if (is.null(dailyAspects)) {
      return(NULL)
    }

    aspectView <- merge(
      securityData[, c('Date', 'diffPercent', 'Actbin', 'Eff')],
      dailyAspects, by = "Date"
    )

    trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.80, list = FALSE)
    aspectViewTrain <- aspectView[trainIndex,]
    aspectViewValidate <- aspectView[-trainIndex,]
    useFeatures <- names(dailyAspects)[-1]
    selectCols <- c('diffPercent', useFeatures)

    return(
      list(
        train = aspectViewTrain[, ..selectCols],
        validate = aspectViewValidate[, ..selectCols]
      )
    )
  }

  modelTrain <- function(params) {
    c(pxSelect, pySelect, orbLimit) %<-% params
    cat("Using PX:", pxSelect, "- PY:", pySelect, "- AORB: ", orbLimit, "\n")
    modelData <- prepareModelData(params)

    if (is.null(modelData)) {
      cat("Skip solution, invalid GA solution params\n\n")
      return(NULL)
    }

    fitModel <- train(
      formula(diffPercent ~ .),
      data = modelData$train,
      method = "brnn",
      metric = "RMSE",
      trControl = control,
      tuneLength = 2
    )

    return(fitModel)
  }

  parseSolutionParameters <- function(solution) {
    pyLength <- length(pySelectAll)
    pxSelect <- pxSelectAll
    orbLimit <- solution[1]
    pySelect <- pySelectAll[solution[2:pyLength] == 1]

    return(list(
      pxSelect = pxSelect,
      pySelect = pySelect,
      orbLimit = orbLimit
    ))
  }

  testModelFit <- function(fitModel, params) {
    allFitMetrics <- c()
    for (i in 1:5) {
      # Validate data predictions with different train partitions.
      modelData <- prepareModelData(params)
      validateDiffPercentPred <- predict(fitModel, modelData$validate, type = "raw")
      validateMAE <- mae(modelData$validate$diffPercent, validateDiffPercentPred)
      validateRMSE <- rmse(modelData$validate$diffPercent, validateDiffPercentPred)
      validateR2 <- cor(modelData$validate$diffPercent, validateDiffPercentPred)^2
      cat("Validate", i, "MAE:", validateMAE, "RMSE:", validateRMSE, "R2:", validateR2, "\n")
      allFitMetrics <- c(allFitMetrics, validateR2)
    }

    fitMetric <- mean(allFitMetrics) / (1 + sd(allFitMetrics))
    cat("Final fit metric:", fitMetric, "\n\n")
    return(fitMetric)
  }

  findRelevantFeatures <- function(solution) {
    params <- parseSolutionParameters(solution)
    fitModel <- modelTrain(params)

    # Invalid GA parameters that failed fit, penalize with high negative value.
    if (is.null(fitModel)) {
      return(-1)
    }

    fitMetric <- testModelFit(fitModel, params)

    return(fitMetric)
  }

  solutionModelTrain <- function(params) {
    # Different partition for each weak learner train
    trainIndex <- createDataPartition(securityData$diffPercent, p = 0.80, list = FALSE)
    securityDataTrain <- securityData[trainIndex,]
    securityDataValidate <- securityData[-trainIndex,]

    fitModel <- modelTrain(params)

    return(fitModel)
  }

  cat("\nProcessing GA features selection\n")
  gar <- ga(
    "real-valued",
    fitness = findRelevantFeatures,
    lower = c(2, rep(0, gaParamsNum-1)),
    upper = c(4, rep(1, gaParamsNum-1)),
    popSize = gaPopSize, maxiter = gaMaxIter, run = gaMaxIter,
    selection = gaint_rwSelection, mutation = gaint_raMutation,
    crossover = gaint_spCrossover, population = gaint_Population,
    elitism = base::max(1, round(gaPopSize * 0.3)),
    pmutation = 0.4, pcrossover = 0.3,
    parallel = F, monitor = gaMonitor, keepBest = T
  )

  cat("\n")
  summary(gar) %>% print()
  cat("\nProcessing weak learners train\n")

  params <- parseSolutionParameters(gar@solution)
  fitModel1 <- solutionModelTrain(params)
  fitModel2 <- solutionModelTrain(params)
  fitModel3 <- solutionModelTrain(params)
  fitModel4 <- solutionModelTrain(params)
  fitModel5 <- solutionModelTrain(params)

  c(pxSelect, pySelect, orbLimit) %<-% params
  dailyAspects <- prepareDailyAspectsCountPlanetYActivationCount(
    dailyAspectsRows, orbLimit, pxSelect, pySelect, aspectsSelectAll
  )

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

  # Ensamble model data partition.
  trainIndex <- createDataPartition(aspectView$Actbin, p = 0.80, list = FALSE)
  aspectViewTrain <- aspectView[trainIndex,]
  aspectViewValidate <- aspectView[-trainIndex,]

  cat("\nProcessing ensamble train\n")
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

  cat("\n", symbol, "MODEL VALIDATE RESULT:\n")
  table(
    actual = aspectViewValidate$Actbin,
    predicted = aspectViewValidate$EffPred
  ) %>%
    caret::confusionMatrix() %>%
    print()

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

  cat("\n", symbol, "MODEL TEST RESULT:\n")
  testResult <- table(
    actualclass = aspectViewTest$Actbin,
    predictedclass = aspectViewTest$EffPred
  ) %>% caret::confusionMatrix()
  print(testResult)

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
  fwrite(dailyAspects[, ..exportCols], paste("~/Desktop/ModelsPred/", symbol, "-predict-", modelId, ".csv", sep = ""))

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