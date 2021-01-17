# Title     : Random predictions accuracy performance test.
rm(list = ls())
library(caret)
library(psych)
source("analysis.r")

symbolTest <- "ADA-USD"
startDate <- as.Date(format(Sys.Date() - 80, "%Y-%m-01"))
securityDataTest <- mainOpenSecurity(
  symbolTest, 2, 4,
  "%Y-%m-%d", startDate
)

# Simulate 500 random (buy/sell) prediction experiments to see the probability that
# a machine learning model accuracy greater than 0.7 could be caused by chance.
allAccuracy <- c()
for(i in 1:500) {
  securityDataTest$EffPred <- ifelse(runif(n = nrow(securityDataTest)) >= 0.5, "buy", "sell")
  categoryLevels = c("buy", "sell")
  confusionData <- table(
    actualclass = factor(securityDataTest$Actbin, levels = categoryLevels),
    predictedclass = factor(securityDataTest$EffPred, levels = categoryLevels)
  ) %>% caret::confusionMatrix()
  allAccuracy <- append(allAccuracy, confusionData$overall['Accuracy'])
}

describe(allAccuracy)
