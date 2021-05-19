# Title     : Search XGB linear model using daily aspects (classic) factors table and SL / FS retrograde.
# Created by: pablocc
# Created on: 25/10/2020

library(boot)
library(caret)
library(psych)
library(plyr)
source("./analysis.r")
source("./indicatorPlots.r")

symbol <- "EOS-USD"
aspectFilter <- c()
pxSelect <- c(
  #'MO',
  #'ME',
  'VE',
  #'SU',
  'MA'
)

pySelect <- c(
  #'SU',
  #'MA',
  'JU',
  'SA',
  'NN',
  'UR'
  #'NE'
  #'PL'
)

dailyAspects <- dailyCombPlanetAspectsFactorsTableLI(
  orbLimit = 4,
  aspectFilter =  aspectFilter,
  pxSelect = pxSelect,
  pySelect = pySelect
)

dailyFastPlanetsSpeed <- dailyFastPlanetsRetrograde()
#dailySlowPlanetsSpeed <- dailySlowPlanetsRetrograde()
dailyAspects <- merge(dailyAspects, dailyFastPlanetsSpeed, by = c('Date'))
#dailyAspects <- merge(dailyAspects, dailySlowPlanetsSpeed, by = c('Date'))

securityData <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2017-01-01", "2020-07-31"
)

#idCols <- c('Date', 'Hour')
#dailyAspectsRaw <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, 6)
#dailyAspectsRaw <- dailyAspectsRaw[p.y %in% c("MA", "SA", "NE", "UR", "PL"),]
##dailyAspectsRaw <- dailyAspectsRaw[p.x %in% c("SU", "VE", "MA", "JU", "SA", "NE", "UR", "PL"),]
##dailyAspectsRaw <- dailyAspectsRaw[aspect %ni% c(30, 45, 103),]
#dailyAspectsStats <- dailyAspectsRaw[, data.table(mean(orb), count(origin)), by=Date]
#dailyAspectsStatsPrice <- merge(
#  securityData[, data.table(Date, abs(zdiffPercent))],
#  dailyAspectsStats, by = "Date"
#)
#setnames(dailyAspectsStatsPrice, c('Date', 'Change', 'MeanOrb', 'AspCount'))
#dailyAspectsStatsPrice[, Weight := AspCount / MeanOrb]
##dailyAspectsStatsPrice[Date > as.Date("2020-09-01") & Date < as.Date("2020-09-22"),]
##with(dailyAspectsStatsPrice[Date > as.Date("2020-09-01") & Date < as.Date("2020-09-22"),], plot(Change, Weight))
#with(dailyAspectsStatsPrice, plot(Change, Weight))

aspectView <- merge(
  securityData[, c('Date', 'Eff', 'Actbin')],
  dailyAspects, by = "Date"
)

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

trainIndex <- createDataPartition(aspectView$Eff, p = 0.80, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

trainXgbLinearModel <- function() {
  control <- trainControl(
    method = "cv",
    number = 20,
    savePredictions = "final",
    returnResamp = "all",
    classProbs = T,
    allowParallel = T,
    verboseIter = T,
    trim = F
  )

  selectCols <- names(aspectViewTrain)[c(-1, -3)]
  fitModel <- train(
    formula(Eff ~ .),
    data = aspectViewTrain[, ..selectCols],
    method = "xgbLinear", # 0.52
    metric = "Kappa",
    maximize = T,
    trControl = control,
    tuneLength = 2
  )

  fitModel$finalModel %>% summary()
  fitModel %>% summary()
  fitModel %>% print()
  fitModel %>% varImp() %>% print()

  cat("--VALIDATE MODEL--\n\n")
  # Validate test data accuracy.
  validateActbinPred <- predict(fitModel, aspectViewValidate, type = "raw")
  validateActbinPred <- mapvalues(validateActbinPred, from = c("up", "down"), to = c("buy", "sell"))
  validateResult <- table(
    actualclass = as.character(aspectViewValidate$Actbin),
    predictedclass = as.character(validateActbinPred)
  ) %>%
    confusionMatrix(positive = "buy")
  print(validateResult)

  cat("--TEST MODEL--\n\n")
  # Validate test data accuracy.
  testActbinPred <- predict(fitModel, aspectViewTest, type = "raw")
  testActbinPred <- mapvalues(testActbinPred, from = c("up", "down"), to = c("buy", "sell"))
  testResult <- table(
    actualclass = as.character(aspectViewTest$Actbin),
    predictedclass = as.character(testActbinPred)
  ) %>%
    confusionMatrix(positive = "buy")
  print(testResult)

  return(fitModel)
}

fitModel1 <- trainXgbLinearModel()
fitModel2 <- trainXgbLinearModel()
fitModel3 <- trainXgbLinearModel()

# Predict outcomes for all weak learner models.
aspectView$EffUpP1 <- predict(fitModel1, aspectView, type = "prob")$up
aspectView$EffUpP2 <- predict(fitModel2, aspectView, type = "prob")$up
aspectView$EffUpP3 <- predict(fitModel3, aspectView, type = "prob")$up

ensambleControl <- trainControl(
  method = "boot",
  savePredictions = "final",
  returnResamp = "all",
  classProbs = T,
  allowParallel = T,
  verboseIter = T,
  trim = F
)

# Train ensamble model.
probCols <- c('EffUpP1', 'EffUpP2', 'EffUpP3')
ensambleModel <- train(
  x = aspectView[, ..probCols],
  y = aspectView$Actbin,
  method = "gbm",
  trControl = ensambleControl,
  tuneLength = 2
)

# Predict outcomes for all weak learner models.
aspectViewTest$EffUpP1 <- predict(fitModel1, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP2 <- predict(fitModel2, aspectViewTest, type = "prob")$up
aspectViewTest$EffUpP3 <- predict(fitModel3, aspectViewTest, type = "prob")$up

# Final ensamble prediction.
aspectViewTest$ActionPred <- predict(ensambleModel, aspectViewTest, type = "raw")

table(
  actualclass = as.character(aspectViewTest$Actbin),
  predictedclass = as.character(aspectViewTest$ActionPred)
) %>%
  confusionMatrix(positive = "buy") %>%
  print()

# Full data set prediction.
dailyAspects$EffUpP1 <- predict(fitModel1, dailyAspects, type = "prob")$up
dailyAspects$EffUpP2 <- predict(fitModel2, dailyAspects, type = "prob")$up
dailyAspects$EffUpP3 <- predict(fitModel3, dailyAspects, type = "prob")$up
dailyAspects$EffPred <- predict(ensambleModel, dailyAspects, type = "raw")

# Round probabilities.
dailyAspects[, EffUpP1 := format(EffUpP1, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP2 := format(EffUpP2, format = "f", big.mark = ",", digits = 3)]
dailyAspects[, EffUpP3 := format(EffUpP3, format = "f", big.mark = ",", digits = 3)]

fwrite(dailyAspects, paste("./predictions/", symbol, "-predict-xgblinearLI-ensamble", ".csv", sep = ""))

#saveRDS(fitModel, paste("./models/", symbol, "_xgb1", ".rds", sep=""))
#fwrite(dailyAspects, paste("~/Desktop/ml", symbol, "daily-xgb3.csv", sep = "-"))
