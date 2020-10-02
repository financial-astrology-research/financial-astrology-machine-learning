# Title     : Prepare daily aspects for exploratory analysis.
# Created by: pablocc
# Created on: 30/09/2020
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

trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.80, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = T
)

logisticModel1 <- train(
  formula(Eff ~ MOME + MOSA + MOUR + MEVE + MEMA + MESA + MEUR + MENE + MEPL + VESU + VEMA + VENE),
  data = aspectViewTrain,
  method = "glm",
  trControl = control,
  tuneLength = 3
)

logisticModel1 %>% print()
logisticModel1 %>% summary()

trainPredictProb <- predict(logisticModel1, aspectViewTrain, type = "prob")
aspectViewTrain$EffPred <- ifelse(trainPredictProb$up > trainPredictProb$down, "up", "down")

table(
  actualclass = as.character(aspectViewTrain$Eff),
  predictedclass = as.character(aspectViewTrain$EffPred)
) %>%
  confusionMatrix(positive = "up") %>%
  print()

# Validate data predictions.
validatePredictProb <- predict(logisticModel1, aspectViewValidate, type = "prob")
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
testPredictProb <- predict(logisticModel1, aspectViewTest, type = "prob")
aspectViewTest$EffPred <- ifelse(testPredictProb$up > testPredictProb$down, "up", "down")

table(
  actualclass = as.character(aspectViewTest$Eff),
  predictedclass = as.character(aspectViewTest$EffPred)
) %>%
  confusionMatrix(positive = "up") %>%
  print()

finalPredictProb <- predict(logisticModel1, dailyAspects, type = "prob")
dailyAspects$EffPred <- ifelse(finalPredictProb$up > finalPredictProb$down, "up", "down")

#saveRDS(logisticModel1, "./models/LINK_logistic1_60acc.rds")

fwrite(
  aspectView,
  paste("~/Desktop/", symbol, "-planets-comb-aspects-factors-ma3-6-2orb.csv", sep = "")
)

#fwrite(dailyAspects, paste("~/Desktop/", symbol, "-predict.csv", sep = ""))
