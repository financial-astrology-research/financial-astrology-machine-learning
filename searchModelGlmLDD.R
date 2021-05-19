# Title     : Daily aspects factors GLM logistic model with CV control with aspects factors.
# Purpose   : Based on ModelLD this model has some variations:
#             1) Add planets from fast planets applying to all planets except JU and NN.
#             2) Not include absense of planet combination aspect "none"
#             3) Increase CV folds to 20
#             4) Validate fit using Actbin daily price change (buy / sell) instead of Actect
#                The fit is based on MA effect to smooth price variations.
#             5) Train using Actbin instead of MAs for price change categories.

library(boot)
library(caret)
library(psych)
library(gbm)
source("./analysis.r")
source("./indicatorPlots.r")

symbol <- "ADA-USD"
maPriceFsPeriod <- 1
maPriceSlPeriod <- 2

pxSelect <- c(
  'MO',
  'ME',
  'VE'
  #'SU',
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
securityData <- securityData[zdiffPercent < 3 & zdiffPercent > -3,]
hist(securityData$zdiffPercent)
cat(paste("Total days rows: ", nrow(securityData)), "\n")

aspectView <- merge(
  securityData[, c('Date', 'diffPercent', 'Actbin')],
  dailyAspects, by = "Date"
)

control <- trainControl(
  method = "cv",
  number = 20,
  savePredictions = "final",
  classProbs = T,
  verboseIter = T
)

selectCols <- names(aspectView)[c(-1, -2)]

logisticModelTrain <- function(aspectView, modelId) {
  trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.90, list = FALSE)
  aspectViewTrain <- aspectView[trainIndex,]
  aspectViewValidate <- aspectView[-trainIndex,]
  logisticModel <- train(
    formula(Actbin ~ .),
    data = aspectViewTrain[, ..selectCols],
    method = "glm",
    trControl = control,
    tuneLength = 5,
  )

  # Validate data predictions.
  validateActPred <- predict(logisticModel, aspectViewValidate, type = "raw")
  aspectViewValidate$ActPred <- validateActPred

  table(
    actualclass = as.character(aspectViewValidate$Actbin),
    predictedclass = as.character(aspectViewValidate$ActPred)
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
  testActPred <- predict(logisticModel, aspectViewTest, type = "raw")
  aspectViewTest$ActPred <- testActPred

  table(
    actualclass = as.character(aspectViewTest$Actbin),
    predictedclass = as.character(aspectViewTest$ActPred)
  ) %>%
    confusionMatrix() %>%
    print()

  #saveRDS(logisticModel, paste("./models/", symbol, "_logistic_", modelId, ".rds", sep = ""))

  return(logisticModel)
}

logisticModel1 <- logisticModelTrain(aspectView, "1")
logisticModel2 <- logisticModelTrain(aspectView, "2")
logisticModel3 <- logisticModelTrain(aspectView, "3")

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
aspectView$ActUpP1 <- predict(logisticModel1, aspectView, type = "prob")$buy
aspectView$ActUpP2 <- predict(logisticModel2, aspectView, type = "prob")$buy
aspectView$ActUpP3 <- predict(logisticModel3, aspectView, type = "prob")$buy

# Train ensamble model.
trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.90, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

probCols <- c('ActUpP1', 'ActUpP2', 'ActUpP3')
topModel <- train(
  x = aspectViewTrain[, ..probCols],
  y = aspectViewTrain$Actbin,
  method = "gbm",
  trControl = control,
  tuneLength = 3
)

topModel %>% summary()

# Validate data predictions.
validateActPred <- predict(topModel, aspectViewValidate, type = "raw")
aspectViewValidate$ActPred <- validateActPred

table(
  actualclass = as.character(aspectViewValidate$Actbin),
  predictedclass = as.character(aspectViewValidate$ActPred)
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
aspectViewTest$ActUpP1 <- predict(logisticModel1, aspectViewTest, type = "prob")$buy
aspectViewTest$ActUpP2 <- predict(logisticModel2, aspectViewTest, type = "prob")$buy
aspectViewTest$ActUpP3 <- predict(logisticModel3, aspectViewTest, type = "prob")$buy
# Final ensamble prediction.
testActPred <- predict(topModel, aspectViewTest, type = "raw")
aspectViewTest$ActPred <- testActPred

table(
  actualclass = as.character(aspectViewTest$Actbin),
  predictedclass = as.character(aspectViewTest$ActPred)
) %>%
  confusionMatrix() %>%
  print()

#saveRDS(topModel, paste("./models/", symbol, "_logistic_ensamble", ".rds", sep = ""))

# Full data set prediction.
dailyAspects$ActUpP1 <- predict(logisticModel1, dailyAspects, type = "prob")$buy
dailyAspects$ActUpP2 <- predict(logisticModel2, dailyAspects, type = "prob")$buy
dailyAspects$ActUpP3 <- predict(logisticModel3, dailyAspects, type = "prob")$buy
allActPred <- predict(topModel, dailyAspects, type = "raw")
dailyAspects$ActPred <- allActPred

# Round probabilities.
dailyAspects[, ActUpP1 := format(ActUpP1, format="f", big.mark = ",", digits = 3)]
dailyAspects[, ActUpP2 := format(ActUpP2, format="f", big.mark = ",", digits = 3)]
dailyAspects[, ActUpP3 := format(ActUpP3, format="f", big.mark = ",", digits = 3)]

exportCols <- c('Date', selectCols[-1], probCols, "ActPred")
fwrite(dailyAspects[, ..exportCols], paste("./predictions/", symbol, "-predict-glmLDD-ensamble", ".csv", sep = ""))
