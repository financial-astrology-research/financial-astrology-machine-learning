# Title     : Models predictions accuracy performance / stability CSV report.
rm(list = ls())
library(caret)
library(psych)
source("analysis.r")

testPredictAccuracy <- function(predictFilename) {
  cat("Processing: ", predictFilename, "\n")
  filenameParts <- unlist(strsplit(predictFilename, "-"))
  symbolTest <- paste(filenameParts[1], filenameParts[2], sep = "-")
  startDate <- as.Date(format(Sys.Date() - 210, "%Y-%m-01"))
  securityDataTest <- mainOpenSecurity(
    symbolTest, 2, 4,
    "%Y-%m-%d", startDate
  )

  predictPath <- paste(basePath, predictFilename, sep = "")
  predictFileInfo <- file.info(predictPath)
  dailyIndicator <- fread(predictPath)

  dailyIndicator[, Date := as.Date(Date)]
  dailyIndicator[, YearMonth := format(Date, "%Y-%m")]
  dailyIndicator <- merge(
    securityDataTest[, c('Date', 'Mid', 'diffPercent', 'Eff', 'Actbin')],
    dailyIndicator,
    by = "Date"
  )

  calculateAccuracy <- function(monthlyData) {
    categoryLevels = c("buy", "sell")
    confusionData <- table(
      actualclass = factor(monthlyData$Actbin, levels = categoryLevels),
      predictedclass = factor(monthlyData$EffPred, levels = categoryLevels)
    ) %>% caret::confusionMatrix()

    accuracy <- confusionData$overall['Accuracy']
    prevalence <- confusionData$byClass['Prevalence']

    list(Accuracy = accuracy, Prevalence = prevalence)
  }

  accuracyTest <- dailyIndicator[, calculateAccuracy(.SD), by = "YearMonth"]
  descriptives6m <- round(describe(head(accuracyTest[, c('Accuracy', 'Prevalence')], 6)), 3)
  descriptives3m <- round(describe(tail(accuracyTest[, c('Accuracy', 'Prevalence')], 3)), 3)
  descriptives1m <- round(describe(tail(accuracyTest[, c('Accuracy', 'Prevalence')], 1)), 3)

  return(
    data.table(
        PredictFile = predictFilename,
        Created = predictFileInfo$mtime,
        Acc6m = descriptives6m$mean[1],
        Acc3m = descriptives3m$mean[1],
        Acc1m = descriptives1m$mean[1],
        AccSD6m = descriptives6m$sd[1],
        AccSD3m = descriptives3m$sd[1],
        Prev6m = descriptives6m$mean[2],
        Prev3m = descriptives3m$mean[2],
        Prev1m = descriptives1m$mean[2],
        PrevSD6m = descriptives6m$sd[2],
        PrevSD3m = descriptives3m$sd[2]
    )
  )
}

#basePath <- "~/Sites/own/trading-signal-processing/csv_indicators/"
#basePath <- "~/Desktop/"
basePath <- "~/Desktop/ModelsPred/"
predictFiles <- list.files(basePath, pattern = "*.csv")
testResults <- setDT(rbindlist(lapply(predictFiles, testPredictAccuracy)))
reportDate <- format(Sys.Date(), "%Y-%m-%d")
modelsPredictSummaryFilename <- paste("~/Desktop/", "models-predict-performance-", reportDate, ".csv", sep = "")

fwrite(testResults, modelsPredictSummaryFilename)
cat("Models summary exported to:", modelsPredictSummaryFilename, "\n")