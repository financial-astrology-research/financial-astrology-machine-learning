# Title     : Daily aspects factors orb2 logistic regression model with CV control.
# Created by: pablocc
# Created on: 02/10/2020

library(boot)
library(caret)
library(psych)
library(gbm)
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

control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = T
)

predictorCols <- c(
  'MOME', 'MOSA', 'MOUR', 'MEVE', 'MEMA', 'MESA', 'MEUR', 'MENE', 'MEPL', 'VESU', 'VEMA', 'VENE', 'SUMA'
)

logisticModelTrain <- function(aspectView, modelId) {
  trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.90, list = FALSE)
  aspectViewTrain <- aspectView[trainIndex,]
  aspectViewValidate <- aspectView[-trainIndex,]

  #SUNE + JUSA
  logisticModel <- train(
    x = aspectViewTrain[, ..predictorCols],
    y = aspectViewTrain$Eff,
    method = "glm",
    trControl = control,
    tuneLength = 3
  )

  logisticModel %>% print()
  #logisticModel1 %>% summary()

  aspectViewTrain$EffPred <- predict(logisticModel, aspectViewTrain, type = "raw")
  table(
    actualclass = as.character(aspectViewTrain$Eff),
    predictedclass = as.character(aspectViewTrain$EffPred)
  ) %>%
    confusionMatrix(positive = "up") %>%
    print()

  # Validate data predictions.
  aspectViewValidate$EffPred <- predict(logisticModel, aspectViewValidate, type = "raw")

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
  aspectViewTest$EffPred <- predict(logisticModel, aspectViewTest, type = "raw")

  table(
    actualclass = as.character(aspectViewTest$Eff),
    predictedclass = as.character(aspectViewTest$EffPred)
  ) %>%
    confusionMatrix(positive = "up") %>%
    print()

  saveRDS(topModel, paste("./models/LINK_logistic_", modelId, ".rds", sep = ""))

  return(logisticModel)
}

logisticModel1 <- logisticModelTrain(aspectView, "1")
logisticModel2 <- logisticModelTrain(aspectView, "2")
logisticModel3 <- logisticModelTrain(aspectView, "3")

logisticModel1 %>% print()
logisticModel2 %>% print()
logisticModel3 %>% print()

# Predict outcomes for all weak learner models.
aspectView$EffUpP1 <- predict(logisticModel1, aspectView, type = "prob")$up
aspectView$EffUpP2 <- predict(logisticModel2, aspectView, type = "prob")$up
aspectView$EffUpP3 <- predict(logisticModel3, aspectView, type = "prob")$up

# Train ensamble model.
trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.90, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

probCols <- c('EffUpP1', 'EffUpP2', 'EffUpP3')
topModel <- train(
  x = aspectViewTrain[, ..probCols],
  y = aspectViewTrain$Eff,
  method = "gbm",
  trControl = control,
  tuneLength = 3
)

topModel %>% summary()

# Validate data predictions.
aspectViewValidate$EffPred <- predict(topModel, aspectViewValidate, type = "raw")

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

# Predict outcomes for all weak learner models.
aspectViewTest$EffUpP1 <- predict(logisticModel1, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP2 <- predict(logisticModel2, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP3 <- predict(logisticModel3, aspectViewTest, type = "prob")$up
# Final ensamble prediction.
aspectViewTest$EffPred <- predict(topModel, aspectViewTest, type = "raw")

table(
  actualclass = as.character(aspectViewTest$Eff),
  predictedclass = as.character(aspectViewTest$EffPred)
) %>%
  confusionMatrix(positive = "up") %>%
  print()

saveRDS(topModel, paste("./models/LINK_logistic_ensamble", ".rds", sep = ""))

# Full data set prediction.
dailyAspects$EffUpP1 <- predict(logisticModel1, dailyAspects, type = "prob")$up
dailyAspects$EffUpP2 <- predict(logisticModel2, dailyAspects, type = "prob")$up
dailyAspects$EffUpP3 <- predict(logisticModel3, dailyAspects, type = "prob")$up
dailyAspects$EffPred <- predict(topModel, dailyAspects, type = "raw")

exportCols <- c('Date', predictorCols, probCols, "EffPred")
fwrite(dailyAspects[, ..exportCols], paste("~/Desktop/", symbol, "-predict-ensamble", ".csv", sep = ""))
