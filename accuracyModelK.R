# Title     : Test accuracy of different models predictions using latest price.

rm(list = ls())
library(caret)
library(psych)
source("analysis.r")

basePath <- "~/Sites/own/financial-astrology-stats/machine_learning/signals_index/"

assetPredictionsTest <- function(symbolTest) {
  securityDataTest <- mainOpenSecurity(
    symbolTest, 2, 4,
    "%Y-%m-%d", "2020-08-01"
  )

  indicatorFile <- paste("ml", symbolTest, "daily-index", sep = "-")
  dailyIndicator <- fread(paste(basePath, indicatorFile, ".csv", sep = ""))
  dailyIndicator[, Date := as.Date(Date)]
  dailyIndicator[, YearMonth := format(Date, "%Y-%m")]
  dailyIndicator <- merge(securityDataTest[, c('Date', 'Mid', 'diffPercent', 'Eff2', 'Actbin')], dailyIndicator, by = "Date")
  dailyIndicator <- dailyIndicator[Action != "neutral",]

  calculateAccuracy <- function(monthlyData) {
    categoryLevels = c("buy", "sell")
    confusionData <- table(
      actualclass = factor(monthlyData$Actbin, levels = categoryLevels),
      predictedclass = factor(monthlyData$Action, levels = categoryLevels)
    ) %>% caret::confusionMatrix()

    accuracy <- confusionData$overall['Accuracy']
    prevalence <- confusionData$byClass['Prevalence']

    list(Accuracy = accuracy, Prevalence = prevalence, N = nrow(monthlyData))
  }

  cat("\n", symbolTest, "montly predictions performacne test:", "\n")
  accuracyTest <- dailyIndicator[, calculateAccuracy(.SD), by = "YearMonth"]
  print(accuracyTest)
  describe(accuracyTest[, c('Accuracy', 'Prevalence')])
}

listFilePath <- npath(paste("~/Sites/own/astro-trading/hisdata/symbols/working.csv", sep = ""))
symbolsList <- read.csv(listFilePath, header = F, stringsAsFactors = F)
testResults <- lapply(symbolsList$V1, assetPredictionsTest)
