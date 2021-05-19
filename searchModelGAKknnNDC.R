# Title     : Daily combined planets aspects factors data KNN regression model
#             with GA feature selection that maximize Rsquared on train data to fit for
#             zdifflogHxL price change estimation.
# Purpose   : Predict daily traiding signal action category (buy / sell) from estimated daily price change.
#             1) Planets ME, VE, SU fast planets applying to all planets.
#             2) CV folds to 5 with 1 repeats for weak learners, folds to 10 with 10 repeats for ensamble.
#             3) Split to 80/20 proportion.
#             4) Validate fit using HxLEff daily price change (buy / sell) instead of Effect
#             5) GA feature detection that fit to maximize Rsquared train data.
#             6) Fit 5 weak learners for zdifflogHxL change.
#             7) Ensamble weak learnets to fit for HxLEff to predict categorical (buy / sell) signal.
#             8) Optimize weak learners for RMSE.
#             9) GA feature selection popSize = 50 and iter = 10.
#            10) Fit using multi train sample mean metric penalized by standard deviation.
#            11) Perform zdifflogHxL zscore outliers drop.

library(boot)
library(zeallot)
library(caret)
library(psych)
library(gbm)
library(ModelMetrics)
source("./analysis.r")
source("./indicatorPlots.r")

modelId <- "ensamble-gakknn-NDC"
zscoreCut <- 2
maPriceFsPeriod <- 2
maPriceSlPeriod <- 3
trainDataStartDate <- as.Date("2010-01-01")
trainDataEndDate <- as.Date("2020-08-15")
testDataStartDate <- as.Date("2020-09-01")
orbLimit <- 2
kMax <- 7
gaPopSize <- 50
gaMaxIter <- 10
gaParamsNum <- 9
wlCVFolds <- 5
wlCVRepeats <- 1
enCVFolds <- 10
enCVRepeats <- 10

pxSelectAll <- c(
  'ME',
  'VE',
  'SU'
)

pySelectAll <- c(
  'ME',
  'VE',
  'SU',
  'MA',
  'JU',
  'SA',
  'UR',
  'NE',
  'PL'
)

aspectSelectAll <- c(
  0,
  30,
  45,
  60,
  90,
  120,
  135,
  150,
  180
)

idCols <- c('Date', 'Hour')
setClassicAspectsSet8()
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
  securityData <- securityData[abs(zdifflogHxL) <= zscoreCut]
  hist(securityData$zdifflogHxL)
  cat(paste("Post filter days observations rows:", nrow(securityData)), "\n\n")

  prepareDailyAspects <- function(pxSelect, pySelect, aspectSelect) {
    dailyAspects <- dailyCombPlanetAspectsFactorsTable(
      dailyAspects = dailyAspectsRows,
      orbLimit = orbLimit,
      pxSelect = pxSelect,
      pySelect = pySelect,
      aspectSelect = aspectSelect
    )

    if (is.null(dailyAspects)) {
      return(NULL)
    }

    return(dailyAspects)
  }

  prepareModelData <- function(params) {
    c(pxSelect, pySelect, aspectSelect) %<-% params
    dailyAspects <- prepareDailyAspects(pxSelect, pySelect, aspectSelect)

    if (is.null(dailyAspects)) {
      return(NULL)
    }

    aspectView <- merge(
      securityData[, c('Date', 'zdifflogHxL', 'HxLEff')],
      dailyAspects, by = "Date"
    )

    trainIndex <- createDataPartition(aspectView$zdifflogHxL, p = 0.80, list = FALSE)
    aspectViewTrain <- aspectView[trainIndex,]
    aspectViewValidate <- aspectView[-trainIndex,]
    useFeatures <- names(dailyAspects)[-1]
    selectCols <- c('zdifflogHxL', useFeatures)
    #cat("Selected cols:", selectCols, "\n")

    return(
      list(
        train = aspectViewTrain[, ..selectCols],
        validate = aspectViewValidate[, ..selectCols]
      )
    )
  }

  modelTrain <- function(params) {
    c(pxSelect, pySelect, aspectSelect) %<-% params
    cat("Using PX:", pxSelect, "- PY:", pySelect, "- ASP:", aspectSelect, "\n")
    modelData <- prepareModelData(params)

    if (is.null(modelData)) {
      cat("Skip solution, invalid GA solution params\n\n")
      return(NULL)
    }

    fitModel <- train(
      formula(zdifflogHxL ~ .),
      data = modelData$train,
      method = "kknn",
      metric = "RMSE",
      trControl = control,
      tuneGrid = expand.grid(
        kmax = kMax,
        distance = 2,
        kernel = "optimal"
      )
    )

    return(fitModel)
  }

  parseSolutionParameters <- function(solution) {
    pxSelect <- pxSelectAll
    pySelect <- pySelectAll[solution[1:gaParamsNum] == 1]
    aspectSelect <- aspectSelectAll

    return(list(
      pxSelect = pxSelect,
      pySelect = pySelect,
      aspectSelect = aspectSelect
    ))
  }

  findRelevantFeatures <- function(solution) {
    params <- parseSolutionParameters(solution)
    fitModel <- modelTrain(params)

    # Invalid GA parameters that failed fit, penalize with high negative value.
    if (is.null(fitModel)) {
      return(-1)
    }

    allFitMetrics <- c()
    for (i in 1:5) {
      # Validate data predictions with different train partitions.
      modelData <- prepareModelData(params)
      validateDiffPercentPred <- predict(fitModel, modelData$validate, type = "raw")
      validateMAE <- mae(modelData$validate$zdifflogHxL, validateDiffPercentPred)
      validateRMSE <- rmse(modelData$validate$zdifflogHxL, validateDiffPercentPred)
      validateR2 <- cor(modelData$validate$zdifflogHxL, validateDiffPercentPred)^2
      cat("Validate", i, "MAE:", validateMAE, "RMSE:", validateRMSE, "R2:", validateR2, "\n")
      allFitMetrics <- c(allFitMetrics, validateR2)
    }

    fitMetric <- mean(allFitMetrics) / (1 + sd(allFitMetrics))
    cat("Final fit metric:", fitMetric, "\n\n")
    return(fitMetric)
  }

  solutionModelTrain <- function(params) {
    fitModel <- modelTrain(params)

    if (is.null(fitModel)) {
      stop("Model train result error")
    }

    return(fitModel)
  }

  cat("\nProcessing GA features selection\n")
  gar <- ga(
    "real-valued",
    fitness = findRelevantFeatures,
    lower = rep(0, gaParamsNum),
    upper = rep(1, gaParamsNum),
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
  #fitModel1 %>% varImp() %>% print()
  fitModel2 <- solutionModelTrain(params)
  #fitModel2 %>% varImp() %>% print()
  fitModel3 <- solutionModelTrain(params)
  #fitModel3 %>% varImp() %>% print()
  fitModel4 <- solutionModelTrain(params)
  #fitModel4 %>% varImp() %>% print()
  fitModel5 <- solutionModelTrain(params)
  #fitModel5 %>% varImp() %>% print()

  c(pxSelect, pySelect, aspectSelect) %<-% params
  dailyAspects <- prepareDailyAspects(pxSelect, pySelect, aspectSelect)

  aspectView <- merge(
    securityData[, c('Date', 'zdifflogHxL', 'HxLEff')],
    dailyAspects, by = "Date"
  )

  # Predict outcomes for all weak learner models.
  aspectView$DiffPred1 <- predict(fitModel1, aspectView, type = "raw")
  aspectView$DiffPred2 <- predict(fitModel2, aspectView, type = "raw")
  aspectView$DiffPred3 <- predict(fitModel3, aspectView, type = "raw")
  aspectView$DiffPred4 <- predict(fitModel4, aspectView, type = "raw")
  aspectView$DiffPred5 <- predict(fitModel5, aspectView, type = "raw")

  # Ensamble model data partition.
  trainIndex <- createDataPartition(aspectView$HxLEff, p = 0.80, list = FALSE)
  aspectViewTrain <- aspectView[trainIndex,]
  aspectViewValidate <- aspectView[-trainIndex,]

  cat("\nProcessing ensamble train\n")
  probCols <- c("DiffPred1", "DiffPred2", "DiffPred3", "DiffPred4", "DiffPred5")
  topModel <- train(
    x = aspectViewTrain[, ..probCols],
    y = aspectViewTrain$HxLEff,
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
    actual = aspectViewValidate$HxLEff,
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
    securityDataTest[, c('Date', 'HxLEff')],
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
    actualclass = aspectViewTest$HxLEff,
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
  fwrite(dailyAspects[, ..exportCols], paste("./predictions/", symbol, "-predict-", modelId, ".csv", sep = ""))

  return(
    list(symbol = symbol, results = testResult)
  )
}

listFilePath <- npath(paste("./symbols/working.csv", sep = ""))
symbolsList <- read.csv(listFilePath, header = F, stringsAsFactors = F)
testResults <- lapply(symbolsList$V1, searchModel)

cat("\nMODEL SEARCH SUMMARY:\n\n")
for (idx in 1:count(testResults)) {
  cat(testResults[[idx]]$symbol, "TEST RESULT:\n")
  print(testResults[[idx]]$results)
}