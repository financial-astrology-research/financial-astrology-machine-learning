# Title     : Search candidate ModelLD parameters for different securities.
# Created by: pablocc
# Created on: 04/10/2020

library(boot)
library(caret)
library(psych)
library(gbm)
library(glmulti)
library(metafor)
source("./analysis.r")
source("./indicatorPlots.r")

dailyAspects <- dailyCombPlanetAspectsFactorsTable(orbLimit = 1)

symbol <- "LINK-USD"
securityData <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2010-01-01", "2020-07-31"
)

# Filter the extreme outliers.
cat(paste("Original days rows: ", nrow(securityData)), "\n")
securityData <- securityData[zdiffPercent < 3 & zdiffPercent > -3,]
hist(securityData$zdiffPercent)
cat(paste("Total days rows: ", nrow(securityData)), "\n")

aspectView <- merge(
  securityData,
  dailyAspects, by = "Date"
)

trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.70, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

selectCols <- c(
  'Actbin'
  #, 'MOME', 'MOVE', 'MOSU', 'MOMA', 'MOJU', 'MOSA', 'MOUR', 'MONE', 'MOPL', 'MONN'
  , 'MEVE', 'MESU', 'MEMA', 'MEJU', 'MESA', 'MEUR', 'MENE', 'MEPL', 'MENN'
  #, 'VESU', 'VEMA', 'VEJU', 'VESA', 'VEUR', 'VENE', 'VEPL', 'VENN'
  , 'SUMA', 'SUJU', 'SUSA', 'SUUR', 'SUNE', 'SUPL', 'SUNN'
  #,'MAJU', 'MASA', 'MAUR', 'MANE', 'MAPL'
  #,'JUSA'
)

modelSearch <- glmulti(
  y = "Actbin",
  xr = selectCols[c(-1)],
  data = aspectViewTrain[, ..selectCols],
  fitfunction = glm,
  family = binomial,
  level = 1,
  marginality = F,
  intercept = T,
  crit = "aic",
  confsetsize = 100,
  method = "g",
  plotty = F,
  popsize = 200,
  conseq = 2
  #mutrate = 0.01, sexrate = 0.1, imm = 0.1,
)

plot(modelSearch, type = "s")

#plsFitTime <- train(unemploy ~ pce + pop + psavert,
#                    data = economics,
#                    method = "pls",
#                    preProc = c("center", "scale"),
#                    trControl = myTimeControl)

control <- trainControl(
#method = "boot", # 2 - slow
#method = "boot632", # 2 - slow
#method = "optimism_boot", # 2 - very slow
#method = "boot_all", # 2 - slow
method = "cv", # 2 - fast
number = 10,
#method = "LOOCV", # 2 - very slow
#method = "LGOCV", # 2 - fast
#method = "none", # 2 - very fast
#method = "timeslice", # 2 - very slow
#initialWindow = 30,
#horizon = 10,
savePredictions = "final",
classProbs = T,
allowParallel = T
)

testLogisticModelFormula <- function(useFormula) {
  cat("\nEvaluating model formula: ", as.character(useFormula), "\n")
  logisticModel <- train(
    useFormula,
    data = aspectViewTrain,
    method = "glm",
    trControl = control,
    tuneLength = 3
  )

  #logisticModel1 %>% summary()

  aspectViewTrain$ActbinPred <- predict(logisticModel, aspectViewTrain, type = "raw")
  trainResult <- table(
    actualclass = as.character(aspectViewTrain$Actbin),
    predictedclass = as.character(aspectViewTrain$ActbinPred)
  ) %>%
    confusionMatrix(positive = "buy")

  # Validate data predictions.
  aspectViewValidate$ActbinPred <- predict(logisticModel, aspectViewValidate, type = "raw")

  validateResult <- table(
    actualclass = as.character(aspectViewValidate$Actbin),
    predictedclass = as.character(aspectViewValidate$ActbinPred)
  ) %>%
    confusionMatrix(positive = "buy")
  #print(testResult)

  validateAccuracy <- validateResult$overall['Accuracy']
  validatePrevalence <- validateResult$byClass['Prevalence']
  cat("Accuracy: ", validateAccuracy, "Prevalence: ", validatePrevalence, "\n")

  if (validateAccuracy > 0.6 &
    validatePrevalence > 0.47 &
    validatePrevalence < 0.53) {
    cat("\nCANDIDATE MODEL\n")
    logisticModel %>% print()
    print(trainResult)
    print(validateResult)

    cat("\nTEST SAMPLE VALIDATION\n")
    # Validate test data accuracy.
    testActbinPred <- predict(logisticModel, aspectViewTest, type = "raw")

    table(
      actualclass = as.character(aspectViewTest$Actbin),
      predictedclass = as.character(testActbinPred)
    ) %>%
      confusionMatrix(positive = "buy") %>%
      print()

    return(logisticModel)
  }

  return(FALSE)
}

#  Reserved data for final test, skip one week to avoid any timeseries memory.
securityDataTest <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2020-08-08"
)

aspectViewTest <- merge(
  securityDataTest,
  dailyAspects,
  by = "Date"
)

cat("--TRAIN BEST FORMULAS WITH TIME SLICES CONTROL--\n\n")
bestModels <- list()
for (j in 1:length(modelSearch@formulas)) {
  selectModel <- testLogisticModelFormula(modelSearch@formulas[[j]])
  if (is.object(selectModel)) {
    cat("\nSelected model with formula: ", as.character(modelSearch@formulas[[j]]))
    bestModels[[length(bestModels) + 1]] <- selectModel
  }
}

selectModelsCount <- length(bestModels)
cat("\nSELECTED #", selectModelsCount, " models that passed criteria.\n\n")

trainBestModelsEnsamble <- function(bestModels) {
  for (idx in 1:length(bestModels)) {
    # Outcome field name.
    fieldName <- paste('pup', idx, sep = "")

    # Train data.
    trainActbinUpProb <- predict(bestModels[[idx]], aspectViewTrain, type = "prob")$up
    aspectViewTrain[, c(fieldName) := trainActbinUpProb]

    # Validate data.
    validateActbinUpProb <- predict(bestModels[[idx]], aspectViewValidate, type = "prob")$up
    aspectViewValidate[, c(fieldName) := validateActbinUpProb]

    # Test data.
    testActbinUpProb <- predict(bestModels[[idx]], aspectViewTest, type = "prob")$up
    aspectViewTest[, c(fieldName) := testActbinUpProb]
  }

  probCols <- paste('pup', seq(1, length(bestModels)), sep = "")
  ensambleModel <- train(
    x = aspectViewTrain[, ..probCols],
    y = aspectViewTrain$Actbin,
    method = "gbm",
    trControl = control,
    tuneLength = 3
  )

  ensambleModel %>% summary()

  # Validate data predictions.
  aspectViewValidate$ActbinPred <- predict(ensambleModel, aspectViewValidate, type = "raw")

  table(
    actualclass = as.character(aspectViewValidate$Actbin),
    predictedclass = as.character(aspectViewValidate$ActbinPred)
  ) %>%
    confusionMatrix(positive = "buy") %>%
    print()

  # Final ensamble prediction.
  aspectViewTest$ActbinPred <- predict(ensambleModel, aspectViewTest, type = "raw")

  table(
    actualclass = as.character(aspectViewTest$Actbin),
    predictedclass = as.character(aspectViewTest$ActbinPred)
  ) %>%
    confusionMatrix(positive = "buy") %>%
    print()

  #useFormula <- modelSearch@formulas[[1]]
  #rfModel = train(
  #  useFormula,
  #  data = aspectViewTrain,
  #  method = "rf",
  #  metric = "Accuracy",
  #  tuneLength = 3,
  #  ntree = 50,
  #  trControl = control,
  #  importance = F
  #)
  #
  #rfModel %>% print()
  #
  #aspectViewTrain$ActbinPred <- predict(rfModel, aspectViewTrain, type = "raw")
  #testResult <- table(
  #  actualclass = as.character(aspectViewTrain$Actbin),
  #  predictedclass = as.character(aspectViewTrain$ActbinPred)
  #) %>%
  #  confusionMatrix(positive = "up")
  #print(cbind(
  #  testResult$overall['Accuracy'], testResult$overall['AccuracyLower'], testResult$overall['AccuracyUpper']
  #))
  #
  ## Validate data predictions.
  #aspectViewValidate$ActbinPred <- predict(rfModel, aspectViewValidate, type = "raw")
  #
  #table(
  #  actualclass = as.character(aspectViewValidate$Actbin),
  #  predictedclass = as.character(aspectViewValidate$ActbinPred)
  #) %>%
  #  confusionMatrix(positive = "up") %>%
  #  print()
  #
  #knnModel <- train(
  #  useFormula,
  #  data = aspectViewTrain,
  #  method = "knn",
  #  trControl = control,
  #  tuneLength = 3
  #)
  #
  #knnModel %>% print()
  ##logisticModel1 %>% summary()
  #
  #aspectViewTrain$ActbinPred <- predict(knnModel, aspectViewTrain, type = "raw")
  #table(
  #  actualclass = as.character(aspectViewTrain$Actbin),
  #  predictedclass = as.character(aspectViewTrain$ActbinPred)
  #) %>%
  #  confusionMatrix(positive = "up") %>%
  #  print()
  #
  ## Validate data predictions.
  #aspectViewValidate$ActbinPred <- predict(knnModel, aspectViewValidate, type = "raw")
  #
  #table(
  #  actualclass = as.character(aspectViewValidate$Actbin),
  #  predictedclass = as.character(aspectViewValidate$ActbinPred)
  #) %>%
  #  confusionMatrix(positive = "up") %>%
  #  print()

  # Experiment bestglm model search.
  #Xy <- aspectViewTrain[, ..selectCols]
  #bestglm(Xy, family=binomial, IC = "BICq")
}

#trainBestModelsEnsamble(bestModels)