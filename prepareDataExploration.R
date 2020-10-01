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

cutOff <- 0.50
trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.80, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

#factors = vars(
# SUSA, VESA, MOME, VEMA, SUMA, SUUR, VEJU, SUJU, MASA, MEJU, MEPL, MESA,
# MEUR, MEMA, MENN, VESU, VEUR, VENN, SUNN, MEVE, MENE, MOSA, MOUR, MONE, MONN),
fitModel <- glm(
  Eff ~ MOME + MOSA + MOUR + MESA + MEUR + MEMA + MEVE + MENE + MEPL,
  data = aspectViewTrain,
  family = quasibinomial(),
  control = list(maxit = 30)
)

# Train data predictions.
fitModel %>% summary()
trendPredictProb <- predict(fitModel, aspectView, type = "response")
aspectView$EffPred <- ifelse(trendPredictProb > cutOff, "up", "down")

table(
  actualclass = as.character(aspectView$Eff),
  predictedclass = as.character(aspectView$EffPred)
) %>%
  confusionMatrix(positive = "up") %>%
  print()

# Validate data predictions.
testTrendPredictProb <- predict(fitModel, aspectViewValidate, type = "response")
aspectViewValidate$EffPred <- ifelse(testTrendPredictProb > cutOff, "up", "down")

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
testTrendPredictProb <- predict(fitModel, aspectViewTest)
aspectViewTest$EffPred <- ifelse(testTrendPredictProb > cutOff, "up", "down")

table(
  actualclass = as.character(aspectViewTest$Eff),
  predictedclass = as.character(aspectViewTest$EffPred)
) %>%
  confusionMatrix(positive = "up") %>%
  print()

fwrite(
  aspectView,
  paste("~/Desktop/", symbol, "-planets-comb-aspects-factors-ma3-6-2orb.csv", sep = "")
)
