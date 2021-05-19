# Title     : Daily aspects factors GLM logistic model with CV control with aspects factors.
# Purpose   : Based on ModelLD this model has some variations:
#             1) Planets MO, ME, VE, SU fast planets applying to all slow planets except JU and NN.
#             2) Not include absense of planet combination aspect "none".
#             3) Increase CV folds to 20.
#             4) Validate fit using Actbin daily price change (buy / sell) instead of Effect
#                The fit is based on MA(2, 4) effect to smooth price variations.
#             5) Use kknn in favor of glm as weak learners algorithm.
#             6) Reduce CV folds to 10.
#             7) Change data split to 80/20 proportion.
#             8) Include SU in fast planet.
#             9) Optimize weak learners and ensamble for Kappa.

library(boot)
library(caret)
library(psych)
library(gbm)
source("./analysis.r")
source("./indicatorPlots.r")

symbol <- "ADA-USD"
maPriceFsPeriod <- 2
maPriceSlPeriod <- 4

pxSelect <- c(
  'MO',
  'ME',
  'VE',
  'SU'
  #'MA'
)

pySelect <- c(
  'ME',
  'VE',
  'SU',
  'MA',
  #'JU',
  'SA',
  #'NN',
  'UR',
  'NE',
  'PL'
)

dailyAspects <- dailyCombPlanetAspectsFactorsTableLI(
  orbLimit = 2,
  pxSelect = pxSelect,
  pySelect = pySelect
)

securityData <- mainOpenSecurity(
  symbol, maPriceFsPeriod, maPriceSlPeriod,
  "%Y-%m-%d", "2010-01-01", "2020-06-30"
)

# Filter the extreme outliers.
cat(paste("Original days rows: ", nrow(securityData)), "\n")
securityData <- securityData[abs(zdiffPercent) < 3]
hist(securityData$zdiffPercent)
cat(paste("Total days rows: ", nrow(securityData)), "\n")

aspectView <- merge(
  securityData[, c('Date', 'diffPercent', 'Actbin', 'Eff')],
  dailyAspects, by = "Date"
)

control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "final",
  classProbs = T,
  verboseIter = T,
  allowParallel = T,
  trim = F
)

# Reserved data for validation.
securityDataTest <- mainOpenSecurity(
  symbol, maPriceFsPeriod, maPriceSlPeriod,
  "%Y-%m-%d", "2020-08-01"
)

aspectViewTest <- merge(
  securityDataTest,
  dailyAspects, by = "Date"
)

selectCols <- names(aspectView)[c(-1, -2, -3)]
modelTrain <- function(method, tuneLength, modelId) {
  trainIndex <- createDataPartition(aspectView$Eff, p = 0.80, list = FALSE)
  aspectViewTrain <- aspectView[trainIndex,]
  aspectViewValidate <- aspectView[-trainIndex,]
  fitModel <- train(
    formula(Eff ~ .),
    data = aspectViewTrain[, ..selectCols],
    method = method,
    metric = "Kappa",
    trControl = control,
    tuneLength = tuneLength,
  )

  # Validate data predictions.
  validateEffPred <- predict(fitModel, aspectViewValidate, type = "raw")
  aspectViewValidate$EffPred <- mapvalues(validateEffPred, from = c("up", "down"), to = c("buy", "sell"))

  table(
    actualclass = as.character(aspectViewValidate$Actbin),
    predictedclass = as.character(aspectViewValidate$EffPred)
  ) %>%
    confusionMatrix() %>%
    print()

  testEffPred <- predict(fitModel, aspectViewTest, type = "raw")
  aspectViewTest$EffPred <- mapvalues(testEffPred, from = c("up", "down"), to = c("buy", "sell"))

  table(
    actualclass = as.character(aspectViewTest$Actbin),
    predictedclass = as.character(aspectViewTest$EffPred)
  ) %>%
    confusionMatrix() %>%
    print()

  #saveRDS(logisticModel, paste("./models/", symbol, "_logistic_", modelId, ".rds", sep = ""))
  return(fitModel)
}

logisticModel1 <- modelTrain("kknn", 3, "1")
logisticModel2 <- modelTrain("kknn", 3, "2")
logisticModel3 <- modelTrain("kknn", 3, "3")

logisticModel1 %>% print()
logisticModel1 %>% varImp()
logisticModel1 %>% summary() %>% print()

logisticModel2 %>% print()
logisticModel2 %>% varImp()
logisticModel2 %>% summary() %>% print()

logisticModel3 %>% print()
logisticModel3 %>% varImp()
logisticModel3 %>% summary() %>% print()

# Predict outcomes for all weak learner models.
aspectView$EffUpP1 <- predict(logisticModel1, aspectView, type = "prob")$up
aspectView$EffUpP2 <- predict(logisticModel2, aspectView, type = "prob")$up
aspectView$EffUpP3 <- predict(logisticModel3, aspectView, type = "prob")$up

# Train ensamble model.
trainIndex <- createDataPartition(aspectView$Eff, p = 0.80, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

probCols <- c('EffUpP1', 'EffUpP2', 'EffUpP3')
topModel <- train(
  x = aspectViewTrain[, ..probCols],
  y = aspectViewTrain$Eff,
  method = "gbm",
  metric = "Kappa",
  trControl = control,
  tuneLength = 3
)

topModel %>% summary()

# Validate data predictions.
validateEffPred <- predict(topModel, aspectViewValidate, type = "raw")
aspectViewValidate$EffPred <- mapvalues(validateEffPred, from = c("up", "down"), to = c("buy", "sell"))

table(
  actualclass = as.character(aspectViewValidate$Actbin),
  predictedclass = as.character(aspectViewValidate$EffPred)
) %>%
  confusionMatrix() %>%
  print()

# Validate with reserved data.
securityDataTest <- mainOpenSecurity(
  symbol, maPriceFsPeriod, maPriceSlPeriod,
  "%Y-%m-%d", "2020-08-01"
)
aspectViewTest <- merge(
  securityDataTest,
  dailyAspects, by = "Date"
)

# Predict outcomes for all weak learner models.
aspectViewTest$EffUpP1 <- predict(logisticModel1, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP2 <- predict(logisticModel2, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP3 <- predict(logisticModel3, aspectViewTest, type = "prob")$up
# Final ensamble prediction.
testEffPred <- predict(topModel, aspectViewTest, type = "raw")
aspectViewTest$EffPred <- mapvalues(testEffPred, from = c("up", "down"), to = c("buy", "sell"))

table(
  actualclass = as.character(aspectViewTest$Actbin),
  predictedclass = as.character(aspectViewTest$EffPred)
) %>%
  confusionMatrix() %>%
  print()

#saveRDS(topModel, paste("./models/", symbol, "_logistic_ensamble", ".rds", sep = ""))

# Full data set prediction.
dailyAspects$EffUpP1 <- predict(logisticModel1, dailyAspects, type = "prob")$up
dailyAspects$EffUpP2 <- predict(logisticModel2, dailyAspects, type = "prob")$up
dailyAspects$EffUpP3 <- predict(logisticModel3, dailyAspects, type = "prob")$up
EffPred <- predict(topModel, dailyAspects, type = "raw")
dailyAspects$EffPred <- mapvalues(EffPred, from = c("up", "down"), to = c("buy", "sell"))

# Round probabilities.
dailyAspects[, EffUpP1 := format(EffUpP1, format="f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP2 := format(EffUpP2, format="f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP3 := format(EffUpP3, format="f", big.mark = ",", digits = 3)]

exportCols <- c('Date', selectCols[-1], probCols, "EffPred")
fwrite(dailyAspects[, ..exportCols], paste("./predictions/", symbol, "-predict-kknnLDA-ensamble", ".csv", sep = ""))
