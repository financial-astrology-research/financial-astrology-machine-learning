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

dailyAspects <- dailyCombPlanetAspectsFactorsTable()

symbol <- "BAT-USD"
securityData <- mainOpenSecurity(
  symbol, 2, 4, "%Y-%m-%d",
  "2010-01-01", "2020-06-30"
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

trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.80, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

selectCols <- c(
  'Eff'
  #, 'MOME', 'MOVE', 'MOSU', 'MOMA', 'MOJU'
  #, 'MOSA', 'MOUR', 'MONE', 'MOPL', 'MONN'
  , 'MEVE', 'MESU', 'MEMA', 'MEJU', 'MESA', 'MEUR', 'MENE', 'MEPL', 'MENN'
  , 'VESU', 'VEMA', 'VEJU', 'VESA', 'VEUR', 'VENE', 'VEPL', 'VENN'
  , 'SUMA', 'SUJU', 'SUSA', 'SUUR', 'SUNE', 'SUPL', 'SUNN'
  #,'MAJU', 'MASA', 'MAUR', 'MANE', 'MAPL'
  #,'JUSA'
)

modelSearch <- glmulti(
  y = "Eff",
  xr = selectCols[c(-1)],
  #exclude=c("sp.y", "sp.x", "dc.x", "dc.y"),
  data = aspectViewTrain[, ..selectCols],
  fitfunction = glm,
  family = binomial,
  maxit = 30,
  level = 1,
  marginality = F,
  intercept = T,
  crit = "aic",
  confsetsize = 50,
  method = "g",
  plotty = F,
  popsize = 200,
  conseq = 2
  #mutrate = 0.01, sexrate = 0.1, imm = 0.1,
)

plot(modelSearch, type = "s")

control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  search = "random",
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

  aspectViewTrain$EffPred <- predict(logisticModel, aspectViewTrain, type = "raw")
  trainResult <- table(
    actualclass = as.character(aspectViewTrain$Eff),
    predictedclass = as.character(aspectViewTrain$EffPred)
  ) %>%
    confusionMatrix(positive = "up")

  # Validate data predictions.
  aspectViewValidate$EffPred <- predict(logisticModel, aspectViewValidate, type = "raw")

  testResult <- table(
    actualclass = as.character(aspectViewValidate$Eff),
    predictedclass = as.character(aspectViewValidate$EffPred)
  ) %>%
    confusionMatrix(positive = "up")
  #print(testResult)

  testAccuracy <- testResult$overall['Accuracy']
  imbalance <- abs(testResult$byClass['Pos Pred Value'] - testResult$byClass['Neg Pred Value'])

  cat("Accuracy=", testAccuracy, "\tAccuracy Diff=", imbalance, "\n", sep="")
  if (testAccuracy >= 0.6 & imbalance <= 0.1) {
    logisticModel %>% print()
    print(trainResult)
    print(testResult)

    return(logisticModel)
  }

  return(FALSE)
}

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

if (selectModelsCount >= 2) {
  # Validate with reserved data.
  securityDataTest <- mainOpenSecurity(symbol, 2, 4, "%Y-%m-%d", "2020-08-01")
  aspectViewTest <- merge(
    securityDataTest,
    dailyAspects,
    by = "Date"
  )

  for (idx in 1:selectModelsCount) {
    # Outcome field name.
    fieldName <- paste('pup', idx, sep = "")

    # Train data.
    trainEffUpProb <- predict(bestModels[[idx]], aspectViewTrain, type = "prob")$up
    aspectViewTrain[, c(fieldName) := trainEffUpProb]

    # Validate data.
    validateEffUpProb <- predict(bestModels[[idx]], aspectViewValidate, type = "prob")$up
    aspectViewValidate[, c(fieldName) := validateEffUpProb]

    # Test data.
    testEffUpProb <- predict(bestModels[[idx]], aspectViewTest, type = "prob")$up
    aspectViewTest[, c(fieldName) := testEffUpProb]

    # Validate test data accuracy.
    testEffPred <- predict(bestModels[[idx]], aspectViewTest, type = "raw")

    table(
      actualclass = as.character(aspectViewTest$Eff),
      predictedclass = as.character(testEffPred)
    ) %>%
      confusionMatrix(positive = "up") %>%
      print()
  }

  probCols <- paste('pup', seq(1, length(bestModels)), sep = "")
  ensambleModel <- train(
    x = aspectViewTrain[, ..probCols],
    y = aspectViewTrain$Eff,
    method = "gbm",
    trControl = control,
    tuneLength = 3
  )

  ensambleModel %>% summary()

  # Validate data predictions.
  aspectViewValidate$EffPred <- predict(ensambleModel, aspectViewValidate, type = "raw")

  table(
    actualclass = as.character(aspectViewValidate$Eff),
    predictedclass = as.character(aspectViewValidate$EffPred)
  ) %>%
    confusionMatrix(positive = "up") %>%
    print()

  # Final ensamble prediction.
  aspectViewTest$EffPred <- predict(ensambleModel, aspectViewTest, type = "raw")

  table(
    actualclass = as.character(aspectViewTest$Eff),
    predictedclass = as.character(aspectViewTest$EffPred)
  ) %>%
    confusionMatrix(positive = "up") %>%
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
  #aspectViewTrain$EffPred <- predict(rfModel, aspectViewTrain, type = "raw")
  #testResult <- table(
  #  actualclass = as.character(aspectViewTrain$Eff),
  #  predictedclass = as.character(aspectViewTrain$EffPred)
  #) %>%
  #  confusionMatrix(positive = "up")
  #print(cbind(
  #  testResult$overall['Accuracy'], testResult$overall['AccuracyLower'], testResult$overall['AccuracyUpper']
  #))
  #
  ## Validate data predictions.
  #aspectViewValidate$EffPred <- predict(rfModel, aspectViewValidate, type = "raw")
  #
  #table(
  #  actualclass = as.character(aspectViewValidate$Eff),
  #  predictedclass = as.character(aspectViewValidate$EffPred)
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
  #aspectViewTrain$EffPred <- predict(knnModel, aspectViewTrain, type = "raw")
  #table(
  #  actualclass = as.character(aspectViewTrain$Eff),
  #  predictedclass = as.character(aspectViewTrain$EffPred)
  #) %>%
  #  confusionMatrix(positive = "up") %>%
  #  print()
  #
  ## Validate data predictions.
  #aspectViewValidate$EffPred <- predict(knnModel, aspectViewValidate, type = "raw")
  #
  #table(
  #  actualclass = as.character(aspectViewValidate$Eff),
  #  predictedclass = as.character(aspectViewValidate$EffPred)
  #) %>%
  #  confusionMatrix(positive = "up") %>%
  #  print()

  # Experiment bestglm model search.
  #Xy <- aspectViewTrain[, ..selectCols]
  #bestglm(Xy, family=binomial, IC = "BICq")
}
