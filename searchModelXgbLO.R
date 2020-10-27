# Title     : Search XGB linear model fit with generalized aspects / planets activation count
#             to predict trending and flat days.
# Created by: pablocc
# Created on: 07/10/2020

# CONCLUSION:
# This model is not able to fit to predict trending days, the accuracy is less than 50%.

library(boot)
library(caret)
library(psych)
library(plyr)
source("./analysis.r")
source("./indicatorPlots.r")

symbol <- "ADA-USD"
pxSelect <- c(
  #'MO',
  'ME',
  'VE',
  'SU',
  'MA',
  'JU',
  'SA',
  'UR',
  'NE',
  'NN',
  'PL'
)

pySelect <- c(
  'VE',
  'SU',
  'MA',
  'JU',
  'SA',
  'NN',
  'UR',
  'NE',
  'PL'
)

aspectFilter <- c(
  #0,
  #30,
  #45,
  #60,
  #90,
  103
  #120
  #135
  #150
  #180
)

orbLimit <- 12
dailyAspectsCount <- dailyAspectsGeneralizedCount(
  orbLimit = orbLimit,
  pxSelect = pxSelect,
  pySelect = pySelect,
  aspectFilter = aspectFilter
)

dailyAspectsPlanetXCount <- dailyPlanetXActivationCount(
  orbLimit = orbLimit,
  pxSelect = pxSelect,
  pySelect = pySelect,
  aspectFilter = aspectFilter
)

dailyAspectsPlanetYCount <- dailyPlanetYActivationCount(
  orbLimit = orbLimit,
  pxSelect = c("MO", pxSelect),
  pySelect = pySelect,
  aspectFilter = aspectFilter
)

dailyAspectsPlanetYOrb <- dailyPlanetYAspectMeanOrb(
  orbLimit = orbLimit,
  pxSelect = c("MO", pxSelect),
  pySelect = pySelect,
  aspectFilter = aspectFilter
)

dailyPlanetSpeed <- dailyPlanetsSpeed()
dailyPlanetDeclination <- dailyPlanetsDeclination()

dailyAspects <- dailyAspectsCount
dailyAspects <- merge(dailyAspects, dailyAspectsPlanetYCount, by = c('Date'))
dailyAspects <- merge(dailyAspects, dailyAspectsPlanetXCount, by = c('Date'))
dailyAspects <- merge(dailyAspects, dailyAspectsPlanetYOrb, by = c('Date'))
dailyAspects <- merge(dailyAspects, dailyPlanetSpeed, by = c('Date'))
dailyAspects <- merge(dailyAspects, dailyPlanetDeclination, by = c('Date'))

securityData <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2017-01-01", "2020-08-30"
)

#summary(securityData$zdiffPercent)
hist(securityData$diffPercent)
securityData[,
  category := cut(zdiffPercent, c(-100, 0.02, 100), c("low", "high"))
]
plot(securityData$category)

aspectView <- merge(
  securityData[, c('Date', 'category')],
  dailyAspects, by = "Date"
)

groupsSummary <- describeBy(aspectView, aspectView$category)
print(groupsSummary$low)
print(groupsSummary$high)
print(groupsSummary$high - groupsSummary$low)

#  Reserved data for final test, skip a week to avoid timeserie memory.
securityDataTest <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2020-09-15"
)

hist(securityDataTest$diffPercent)
securityDataTest[,
  category := cut(diffPercent, c(-100, 0.02, 100), c("low", "high"))
]
plot(securityDataTest$category)

aspectViewTest <- merge(
  securityDataTest[, c('Date', 'category')],
  dailyAspects,
  by = "Date"
)

trainIndex <- createDataPartition(aspectView$category, p = 0.90, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

customSummary <- function(data, lev = levels(data$obs), model = NULL) {
  c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
}

control <- trainControl(
  method = "cv",
  number = 10,
  savePredictions = "all",
  returnResamp = "all",
  allowParallel = T,
  verboseIter = T,
  #summaryFunction = customSummary,
  trim = F
)

selectCols <- names(aspectViewTrain)[c(-1)]
fitModel <- train(
  formula(category ~ .),
  data = aspectViewTrain[, ..selectCols],
  #method = "xgbDART", # 0.51
  #method = "xgbLinear", # 0.52
  #method = "xgbTree", # 0.45
  method = "kknn",
  #method = "gamLoess",
  metric = "Kappa",
  maximize = T,
  trControl = control,
  preProc = c("scale"),
  tuneLength = 7
)

fitModel %>% print()
fitModel %>% varImp() %>% print()

cat("--VALIDATE MODEL--\n\n")
# Validate test data accuracy.
validateActbinPred <- predict(fitModel, aspectViewValidate, type = "raw")
#validateActbinPred <- mapvalues(validateActbinPred, from = c("up", "down"), to = c("buy", "sell"))
validateResult <- table(
  actualclass = as.character(aspectViewValidate$category),
  predictedclass = as.character(validateActbinPred)
) %>%
  confusionMatrix()
print(validateResult)

cat("--TEST MODEL--\n\n")
# Validate test data accuracy.
testActbinPred <- predict(fitModel, aspectViewTest, type = "raw")
#testActbinPred <- mapvalues(testActbinPred, from = c("up", "down"), to = c("buy", "sell"))
testResult <- table(
  actualclass = as.character(aspectViewTest$category),
  predictedclass = as.character(testActbinPred)
) %>%
  confusionMatrix()
print(testResult)

#fwrite(dailyAspects, paste("~/Desktop/", symbol, "-predict-xgblinearLO-ensamble", ".csv", sep = ""))
#saveRDS(fitModel, paste("./models/", symbol, "_xgb1", ".rds", sep=""))
#fwrite(dailyAspects, paste("~/Desktop/ml", symbol, "daily-xgb3.csv", sep = "-"))
##