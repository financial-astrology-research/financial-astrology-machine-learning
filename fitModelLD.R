# Title     : Daily aspects factors orb2 logistic regression model with CV control.
# Created by: pablocc
# Created on: 02/10/2020

library(boot)
library(caret)
library(psych)
source("./analysis.r")
source("./indicatorPlots.r")

dailyAspects <- dailyCombPlanetAspectsFactorsTable()

symbol <- "LINK-USD"
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

logisticModelTrain <- function(aspectView, modelId) {
  trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.90, list = FALSE)
  aspectViewTrain <- aspectView[trainIndex,]
  aspectViewValidate <- aspectView[-trainIndex,]

  control <- trainControl(
    method = "cv",
    number = 10,
    savePredictions = "final",
    classProbs = T
  )

  #SUNE + JUSA
  predictorCols <- c('MOME', 'MOSA', 'MOUR', 'MEVE', 'MEMA', 'MESA', 'MEUR', 'MENE', 'MEPL', 'VESU', 'VEMA', 'VENE', 'SUMA')
  logisticModel <- train(
    x = aspectViewTrain[, ..predictorCols],
    y = aspectViewTrain$Eff,
    method = "glm",
    trControl = control,
    tuneLength = 3
  )

  logisticModel %>% print()
  #logisticModel1 %>% summary()

  #trainPredictProb <- predict(logisticModel1, aspectViewTrain, type = "prob")
  #aspectViewTrain$EffPred <- ifelse(trainPredictProb$up > trainPredictProb$down, "up", "down")
  #table(
  #  actualclass = as.character(aspectViewTrain$Eff),
  #  predictedclass = as.character(aspectViewTrain$EffPred)
  #) %>%
  #  confusionMatrix(positive = "up") %>%
  #  print()

  # Validate data predictions.
  validatePredictProb <- predict(logisticModel, aspectViewValidate, type = "prob")
  aspectViewValidate$EffPred <- ifelse(validatePredictProb$up > validatePredictProb$down, "up", "down")

  table(
    actualclass = as.character(aspectViewValidate$Eff),
    predictedclass = as.character(aspectViewValidate$EffPred)
  ) %>%
    confusionMatrix(positive = "up") %>%
    print()

  # Validate with reserved data.
  securityDataTest <- mainOpenSecurity(symbol, 2, 4, "%Y-%m-%d", "2020-08-01")
  aspectViewTest <- merge(
    securityDataTest,
    dailyAspects, by = "Date"
  )
  testPredictProb <- predict(logisticModel, aspectViewTest, type = "prob")
  aspectViewTest$EffPred <- ifelse(testPredictProb$up > testPredictProb$down, "up", "down")

  table(
    actualclass = as.character(aspectViewTest$Eff),
    predictedclass = as.character(aspectViewTest$EffPred)
  ) %>%
    confusionMatrix(positive = "up") %>%
    print()

  finalPredictProb <- predict(logisticModel, dailyAspects, type = "prob")
  dailyAspects$EffPred <- ifelse(finalPredictProb$up > finalPredictProb$down, "up", "down")

  #saveRDS(logisticModel1, "./models/LINK_logistic1_60acc.rds")
  exportCols <- c('Date', predictorCols, "EffPred")
  fwrite(dailyAspects[, ..exportCols], paste("~/Desktop/", symbol, "-predict-", modelId, ".csv", sep = ""))

  return(logisticModel)
}

logisticModel1 <- logisticModelTrain(aspectView, "1")
logisticModel2 <- logisticModelTrain(aspectView, "2")
logisticModel3 <- logisticModelTrain(aspectView, "3")

logisticModel1 %>% print()
logisticModel2 %>% print()
logisticModel3 %>% print()
