# Title     : Experiment multiples models with caret train.
# Created by: pablocc
# Created on: 07/10/2020

library(boot)
library(caret)
library(psych)
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

# TODO: Experiment partition by price diff and increase to 0.80.
trainIndex <- createDataPartition(aspectView$Actbin, p = 0.70, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

#  Reserved data for final test, skip a week to avoid timeserie memory.
securityDataTest <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2020-08-08"
)

aspectViewTest <- merge(
  securityDataTest,
  dailyAspects,
  by = "Date"
)

selectCols <- c(
  'Actbin'
  , 'MOME', 'MOVE', 'MOSU', 'MOMA', 'MOJU', 'MOSA', 'MOUR', 'MONE', 'MOPL', 'MONN'
  , 'MEVE', 'MESU', 'MEMA', 'MEJU', 'MESA', 'MEUR', 'MENE', 'MEPL', 'MENN'
  , 'VESU', 'VEMA', 'VEJU', 'VESA', 'VEUR', 'VENE', 'VEPL', 'VENN'
  , 'SUMA', 'SUJU', 'SUSA', 'SUUR', 'SUNE', 'SUPL', 'SUNN'
)

control <- trainControl(
  #method = "cv", # 2 - fast
  #method = "boot", # 2 - slow
  #method = "boot632", # 2 - slow
  #method = "optimism_boot", # 2 - very slow
  #method = "boot_all", # 2 - slow
  #method = "LOOCV", # 2 - very slow
  method = "LGOCV", # 2 - fast
  #method = "none", # 2 - very fast
  #method = "timeslice", # 2 - very slow
  #initialWindow = 30,
  #horizon = 10,
  number = 10,
  savePredictions = "final",
  classProbs = T,
  allowParallel = T,
  verboseIter = T
)

fitModel <- train(
  formula(Actbin ~ .),
  data = aspectViewTrain[, ..selectCols],
  # method = "logreg", # 0.54
  # method = "adaboost", # 0.55
  method = "cforest",
  #mtry = 10,
  trControl = control,
  tuneLength = 3
)

fitModel %>% summary()
fitModel %>% print()
fitModel %>% plot()
fitModel %>% varImp()

cat("--VALIDATE MODEL--\n\n")
# Validate test data accuracy.
validateActbinPred <- predict(fitModel, aspectViewValidate, type = "raw")
validateResult <- table(
  actualclass = as.character(aspectViewValidate$Actbin),
  predictedclass = as.character(validateActbinPred)
) %>%
  confusionMatrix(positive = "buy")
print(validateResult)

cat("--TEST MODEL--\n\n")
# Validate test data accuracy.
testActbinPred <- predict(fitModel, aspectViewTest, type = "raw")
testResult <- table(
  actualclass = as.character(aspectViewTest$Actbin),
  predictedclass = as.character(testActbinPred)
) %>%
  confusionMatrix(positive = "buy")
print(testResult)
