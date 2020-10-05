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
  , 'MOME', 'MOVE', 'MOSU', 'MOMA', 'MOJU'
  , 'MOSA', 'MOUR', 'MONE', 'MOPL', 'MONN'
  , 'MEVE', 'MESU', 'MEMA', 'MEJU', 'MESA', 'MEUR', 'MENE', 'MEPL', 'MENN'
  , 'VESU', 'VEMA', 'VEJU', 'VESA', 'VEUR', 'VENE', 'VEPL'
  , 'SUMA', 'SUJU', 'SUSA'
  # ,'SUUR', 'SUNE', 'SUPL', 'SUNN'
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

  trainAccuracy <- as.numeric(trainResult$overall['Accuracy'])
  testAccuracy <- as.numeric(testResult$overall['Accuracy'])

  if (trainAccuracy >= 0.6 & testAccuracy >= 0.6) {
    cat("Test Formula: ", as.character(useFormula), "\n")
    logisticModel %>% print()

    print(cbind(
      trainResult$overall['Accuracy'], trainResult$overall['AccuracyLower'], trainResult$overall['AccuracyUpper']
    ))

    print(cbind(
      testResult$overall['Accuracy'], testResult$overall['AccuracyLower'], testResult$overall['AccuracyUpper']
    ))

    return(TRUE)
  }

  return(FALSE)
}

bestFormulas <- c()
for (j in 1:count(modelSearch@formulas)) {
  selectFormula <- testLogisticModelFormula(modelSearch@formulas[[j]])
  if (selectFormula) {
    bestFormulas <- c(bestFormulas, modelSearch@formulas[[j]])
  }
}

cat("\n\nBest ", count(bestFormulas), " model formulas: \n")
print(bestFormulas)
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
