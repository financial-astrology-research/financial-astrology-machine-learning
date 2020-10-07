# Title     : Experiment multiples models with caret train.
# Created by: pablocc
# Created on: 07/10/2020

library(boot)
library(caret)
library(psych)
library(gbm)
source("./analysis.r")
source("./indicatorPlots.r")

dailyAspects <- dailyCombPlanetAspectsFactorsTable()

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
  'Eff'
  #, 'MOME', 'MOVE', 'MOSU', 'MOMA', 'MOJU'
  #, 'MOSA', 'MOUR', 'MONE', 'MOPL', 'MONN'
  , 'MEVE', 'MESU', 'MEMA', 'MEJU', 'MESA', 'MEUR', 'MENE', 'MEPL', 'MENN'
  , 'VESU', 'VEMA', 'VEJU', 'VESA', 'VEUR', 'VENE', 'VEPL', 'VENN'
  #, 'SUMA', 'SUJU', 'SUSA', 'SUUR', 'SUNE', 'SUPL', 'SUNN'
  #,'MAJU', 'MASA', 'MAUR', 'MANE', 'MAPL'
  #,'JUSA'
)

control <- trainControl(
# method = "boot",
#method = "boot632",
#method = "optimism_boot",
#method = "boot_all", # none
#method = "cv", # none
method = "LOOCV",
# method = "LGOCV",
# method = "none",
# method = "timeslice",
# method = "adaptive_cv",
# method = "adaptive_boot",
# method = "adaptive_LGOCV",
#initialWindow = 30,
#horizon = 10,
savePredictions = "final",
classProbs = T,
allowParallel = T
)

fitModel <- train(
  formula(Eff ~ .),
  data = aspectViewTrain[, ..selectCols],
  method = "glm",
  trControl = control,
  tuneLength = 3
)

#  Reserved data for final test.
securityDataTest <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2020-08-01"
)

aspectViewTest <- merge(
  securityDataTest,
  dailyAspects,
  by = "Date"
)

cat("--VALIDATE MODEL--\n\n")
# Validate test data accuracy.
validateEffPred <- predict(fitModel, aspectViewValidate, type = "raw")
validateResult <- table(
  actualclass = as.character(aspectViewValidate$Eff),
  predictedclass = as.character(validateEffPred)
) %>%
  confusionMatrix(positive = "up")
print(validateResult)

cat("--TEST MODEL--\n\n")
# Validate test data accuracy.
testEffPred <- predict(fitModel, aspectViewTest, type = "raw")
testResult <- table(
  actualclass = as.character(aspectViewTest$Eff),
  predictedclass = as.character(testEffPred)
) %>%
  confusionMatrix(positive = "up")
print(testResult)
