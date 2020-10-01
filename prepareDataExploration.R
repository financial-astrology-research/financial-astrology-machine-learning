# Title     : Prepare daily aspects for exploratory analysis.
# Created by: pablocc
# Created on: 30/09/2020

library(caret)
source("./analysis.r")
source("./indicatorPlots.r")

dailyAspects <- dailyCombPlanetAspectsFactorsTable()

symbol <- "LINK-USD"
securityData <- mainOpenSecurity(
  symbol, 7, 14, "%Y-%m-%d",
  "2010-01-01", "2020-07-31"
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

#factors = vars(
# SUSA, VESA, MOME, VEMA, SUMA, SUUR, VEJU, SUJU, MASA, MEJU, MEPL, MESA,
# MEUR, MEMA, MENN, VESU, VEUR, VENN, SUNN, MEVE, MENE, MOSA, MOUR, MONE, MONN),
fitModel <- glm(
  Eff ~ SUSA
    +  VESA +  VEMA +  SUMA +  SUUR
    +  VEJU +  MEJU + MESA + MENN
    + VESU + MEVE + MOUR,
  data = aspectView,
  family = "binomial"
)

fitModel %>% summary()
trendPredictProb <- predict(fitModel, aspectView, type = "response")
aspectView$EffPred <- ifelse(trendPredictProb > 0.5, "up", "down")

table(
  actualclass = as.character(aspectView$Eff),
  predictedclass = as.character(aspectView$EffPred)
) %>%
  confusionMatrix() %>%
  print()

# Validate with reserved data.
securityDataTest <- mainOpenSecurity(symbol, 7, 14, "%Y-%m-%d", "2020-08-01")
aspectViewTest <- merge(
  securityDataTest,
  dailyAspects, by = "Date"
)
testTrendPredictProb <- predict(fitModel, aspectViewTest)
aspectViewTest$EffPred <- ifelse(testTrendPredictProb > 0.50, "up", "down")

table(
  actualclass = as.character(aspectViewTest$Eff),
  predictedclass = as.character(aspectViewTest$EffPred)
) %>%
  confusionMatrix() %>%
  print()

fwrite(
  aspectView,
  paste("~/Desktop/", symbol, "-planets-comb-aspects-factors-2orb.csv", sep = "")
)
