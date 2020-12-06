# Title     : Random predictions accuracy performance test.
rm(list = ls())
library(caret)
library(psych)
source("analysis.r")

symbolTest <- "ADA-USD"
startDate <- as.Date(format(Sys.Date() - 60, "%Y-%m-01"))
securityDataTest <- mainOpenSecurity(
  symbolTest, 2, 4,
  "%Y-%m-%d", startDate
)

securityDataTest$EffPred <- ifelse(runif(n = nrow(securityDataTest)) >= 0.5, "buy", "sell")
categoryLevels = c("buy", "sell")
confusionData <- table(
  actualclass = factor(securityDataTest$Actbin, levels = categoryLevels),
  predictedclass = factor(securityDataTest$EffPred, levels = categoryLevels)
) %>% caret::confusionMatrix()

print(confusionData)
