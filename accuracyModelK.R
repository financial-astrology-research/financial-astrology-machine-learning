# Title     : Test accuracy of different models predictions using latest price.

rm(list = ls())
library(caret)
source("analysis.r")

symbolTest <- "BAT-USD"
securityDataTest <- mainOpenSecurity(
  symbolTest, 2, 4,
  "%Y-%m-%d", "2020-01-01", "2020-10-31"
)
#basePath <- "~/Sites/own/trading-signal-processing/csv_indicators/"
#basePath <- "~/Desktop/"
basePath <- "~/Desktop/ModelsPred/"

symbolNormalized <- str_replace(symbolTest, "-", "")

#indicatorFile <- "ADA-USD-predict-ensamble" # A: 63 M, 7 SD / P: 54 M, 14 SD
#indicatorFile <- "ADA-USD-predict-glmLDC-ensamble" # A: 61, 11 / P: 51, 14
#indicatorFile <- "ADA-USD-predict-glmLDD-ensamble" # A: 58, 13 / P: 51, 13
#indicatorFile <- "ADA-USD-predict-glmLDF-ensamble" # A: 60, 13 / P: 50, 20
#indicatorFile <- "ADA-USD-predict-glmLDG-ensamble" # A: 61, 11 / P: 54, 15
#indicatorFile <- "ADA-USD-predict-kknnLDA-ensamble" # A: 66, 14 / P: 54, 13
#indicatorFile <- "ADA-USD-predict-kknnLDAA-ensamble" # A: 66, 12 / P: 53, 13 (Best)
#indicatorFile <- "ADA-USD-predict-kknnLDAC-ensamble" # A: 67, 10 / P: 52, 17
#indicatorFile <- "ADA-USD-predict-kknnLDAD-ensamble" # A: 67, 11 / P: 51, 21
#indicatorFile <- "ADA-USD-predict-kknnLDAE-ensamble" # A: 69, 11 / P: 52, 17
#indicatorFile <- "ADA-USD-predict-kknnLDAF-ensamble" # A: 70, 13 / P: 50, 13 (Best)
#indicatorFile <- "ADA-USD-predict-kknnLDAG-ensamble" # A: 70, 10 / P: 57, 19
#indicatorFile <- "ADA-USD-predict-kknnLDAH-ensamble" # A: 64, 14 / P: 48, 19
#indicatorFile <- "ADA-USD-predict-kknnLDB-ensamble" # A: 66, 15 / P: 49, 20
#indicatorFile <- "ADA-USD-predict-kknnLDC-ensamble" # A: 67, 8 / P: 51, 19
#indicatorFile <- "ADA-USD-predict-kknnLDD-ensamble" # A: 68, 9 / P: 52, 15 (Best)
#indicatorFile <- "ADA-USD-predict-kknnLDDA-ensamble" # A: 64, 14 / P: 56, 12
#indicatorFile <- "ADA-USD-predict-kknnLDDB-ensamble" # A: 66, 17 / P: 51, 17
#indicatorFile <- "ADA-USD-predict-kknnLDDC-ensamble" # A: 66, 12 / P: 58, 10
#indicatorFile <- "ADA-USD-predict-kknnLDDD-ensamble" # A: 67, 13 / P: 51, 17 (Best)
#indicatorFile <- "ADA-USD-predict-kknnLDDE-ensamble" # A: 66, 11 / P: 45, 13
#indicatorFile <- "ADA-USD-predict-kknnLDE-ensamble" # A: 65, 12 / P: 52, 22
#indicatorFile <- "ADA-USD-predict-xgblinearLN-ensamble" # 66, 14 / P: 58, 13

#indicatorFile <- "BAT-USD-predict-ensamble" # A: 55, 8 / P: 44, 12
#indicatorFile <- "BAT-USD-predict-glmLDAB-ensamble" # A: 56, 11 / P: 42, 14
#indicatorFile <- "BAT-USD-predict-glmLDB-ensamble" # A: 57, 11 / P: 48, 21
#indicatorFile <- "BAT-USD-predict-glmLDC-ensamble" # A: 57, 17 / P: 43, 15
#indicatorFile <- "BAT-USD-predict-glmLDD-ensamble" # A: 57, 16 / P: 48, 15
#indicatorFile <- "BAT-USD-predict-glmLDF-ensamble" # A: 57, 11 / P: 47, 17
#indicatorFile <- "BAT-USD-predict-glmLDG-ensamble" # A: 59, 11 / P: 42, 15
#indicatorFile <- "BAT-USD-predict-kknnLDA-ensamble" # A: 63, 11 / P: 54, 12
#indicatorFile <- "BAT-USD-predict-kknnLDAA-ensamble" # A: 63, 11 / P: 54, 12
#indicatorFile <- "BAT-USD-predict-kknnLDAB-ensamble" # A: 75, 18 / P: 51, 7
#indicatorFile <- "BAT-USD-predict-kknnLDAD-ensamble" # A: 66, 7 / P: 50, 13 (Best)
#indicatorFile <- "BAT-USD-predict-kknnLDAE-ensamble" # A: 61, 11 / P: 52, 13
#indicatorFile <- "BAT-USD-predict-kknnLDAF-ensamble" # A: 61, 10 / P: 52, 12
#indicatorFile <- "BAT-USD-predict-kknnLDAG-ensamble" # A: 62, 14 / P: 51, 19
#indicatorFile <- "BAT-USD-predict-kknnLDAH-ensamble" # A: 62, 10 / P: 48, 16
#indicatorFile <- "BAT-USD-predict-kknnLDB-ensamble" # A: 63, 8 / P: 52, 11  (Best)
#indicatorFile <- "BAT-USD-predict-kknnLDC-ensamble" # A: 64, 9 / P: 52, 12 (Best)
#indicatorFile <- "BAT-USD-predict-kknnLDD-ensamble" # A: 63, 13 / P: 55, 14
#indicatorFile <- "BAT-USD-predict-kknnLDDA-ensamble" # A: 62, 9 / P: 54, 8 (Best)
#indicatorFile <- "BAT-USD-predict-kknnLDDB-ensamble" # A: 63, 13 / P: 50, 13
#indicatorFile <- "BAT-USD-predict-kknnLDDC-ensamble" # A: 63, 11 / P: 54, 8
#indicatorFile <- "BAT-USD-predict-kknnLDDD-ensamble" # A: 63, 9 / P: 46, 18
#indicatorFile <- "BAT-USD-predict-kknnLDDE-ensamble" # A: 63, 11 / P: 58, 16
#indicatorFile <- "BAT-USD-predict-kknnLDE-ensamble" # A: 66, 8 / P: 49, 15 (Best)
#indicatorFile <- "BAT-USD-predict-xgblinearLH-ensamble" # A: 62, 11 / P: 49, 19
#indicatorFile <- "BAT-USD-predict-xgblinearLI-ensamble" # A: 62, 11 / P: 49, 19
#indicatorFile <- "BAT-USD-predict-xgblinearLJ-ensamble" # A: 66, 10 / P: 45, 15
#indicatorFile <- "BAT-USD-predict-xgblinearLN-ensamble" # A: 64, 9 / P: 50, 15

# TODO: Continue evaluating model stability.
#indicatorFile <- "BNB-USD-predict-glmLDAB-ensamble" # 44
#indicatorFile <- "BNB-USD-predict-glmLDF-ensamble" # 36
#indicatorFile <- "BNB-USD-predict-glmLDG-ensamble" # 51
#indicatorFile <- "BNB-USD-predict-kknnLDAA-ensamble" # 41
#indicatorFile <- "BNB-USD-predict-kknnLDAB-ensamble" # 49
#indicatorFile <- "BNB-USD-predict-kknnLDAE-ensamble" # 49
#indicatorFile <- "BNB-USD-predict-kknnLDAF-ensamble" # 59
#indicatorFile <- "BNB-USD-predict-kknnLDAG-ensamble" # 59
#indicatorFile <- "BNB-USD-predict-kknnLDAH-ensamble" # 56
#indicatorFile <- "BNB-USD-predict-kknnLDB-ensamble" # 62
#indicatorFile <- "BNB-USD-predict-kknnLDC-ensamble" # 59
#indicatorFile <- "BNB-USD-predict-kknnLDD-ensamble" # 62
#indicatorFile <- "BNB-USD-predict-kknnLDDA-ensamble" # 61
#indicatorFile <- "BNB-USD-predict-kknnLDDB-ensamble" # 48
#indicatorFile <- "BNB-USD-predict-kknnLDDC-ensamble" # 51
#indicatorFile <- "BNB-USD-predict-kknnLDDD-ensamble" # 42
#indicatorFile <- "BNB-USD-predict-kknnLDDE-ensamble" # 58
#indicatorFile <- "BNB-USD-predict-kknnLDE-ensamble" # 61
#indicatorFile <- "BNB-USD-predict-xgblinearLN-ensamble" # 54

#indicatorFile <- "BTC-USD-predict-ensamble" # 54
#indicatorFile <- "BTC-USD-predict-glmLDAA-ensamble" # 56
#indicatorFile <- "BTC-USD-predict-glmLDB-ensamble" # 49
#indicatorFile <- "BTC-USD-predict-glmLDC-ensamble" # 54
#indicatorFile <- "BTC-USD-predict-glmLDD-ensamble" # 59
#indicatorFile <- "BTC-USD-predict-glmLDG-ensamble" # 62
#indicatorFile <- "BTC-USD-predict-kknnLDAB-ensamble" # 56
#indicatorFile <- "BTC-USD-predict-kknnLDAC-ensamble" # 59
#indicatorFile <- "BTC-USD-predict-kknnLDAD-ensamble" # 64
#indicatorFile <- "BTC-USD-predict-kknnLDAE-ensamble" # 72
#indicatorFile <- "BTC-USD-predict-kknnLDAF-ensamble" # 67
#indicatorFile <- "BTC-USD-predict-kknnLDAG-ensamble" # 62
#indicatorFile <- "BTC-USD-predict-kknnLDAH-ensamble" # 64
#indicatorFile <- "BTC-USD-predict-kknnLDB-ensamble" # 74
#indicatorFile <- "BTC-USD-predict-kknnLDC-ensamble" # 62
#indicatorFile <- "BTC-USD-predict-kknnLDD-ensamble" # 69
#indicatorFile <- "BTC-USD-predict-kknnLDDA-ensamble" # 51
#indicatorFile <- "BTC-USD-predict-kknnLDDB-ensamble" # 48
#indicatorFile <- "BTC-USD-predict-kknnLDDC-ensamble" # 45
#indicatorFile <- "BTC-USD-predict-kknnLDDD-ensamble" # 52
#indicatorFile <- "BTC-USD-predict-kknnLDDE-ensamble" # 58
#indicatorFile <- "BTC-USD-predict-kknnLDE-ensamble" # 54
#indicatorFile <- "BTC-USD-predict-xgblinearLJ-ensamble" # 56

#indicatorFile <- "DASH-USD-predict-ensamble" # 64
#indicatorFile <- "DASH-USD-predict-glmLDA-ensamble" # 54
#indicatorFile <- "DASH-USD-predict-glmLDAA-ensamble" # 59
#indicatorFile <- "DASH-USD-predict-glmLDAB-ensamble" # 51
#indicatorFile <- "DASH-USD-predict-glmLDB-ensamble" # 54
#indicatorFile <- "DASH-USD-predict-glmLDC-ensamble" # 51
#indicatorFile <- "DASH-USD-predict-glmLDC-ensamble" # 51
#indicatorFile <- "DASH-USD-predict-glmLDG-ensamble" # 62
#indicatorFile <- "DASH-USD-predict-kknnLDAE-ensamble" # 69
#indicatorFile <- "DASH-USD-predict-kknnLDAF-ensamble" # 54
#indicatorFile <- "DASH-USD-predict-kknnLDAG-ensamble" # 59
#indicatorFile <- "DASH-USD-predict-kknnLDAH-ensamble" # 59
#indicatorFile <- "DASH-USD-predict-kknnLDB-ensamble" # 64
#indicatorFile <- "DASH-USD-predict-kknnLDC-ensamble" # 72
#indicatorFile <- "DASH-USD-predict-kknnLDD-ensamble" # 64
#indicatorFile <- "DASH-USD-predict-kknnLDDA-ensamble" # 61
#indicatorFile <- "DASH-USD-predict-kknnLDDB-ensamble" # 58
#indicatorFile <- "DASH-USD-predict-kknnLDDC-ensamble" # 38
#indicatorFile <- "DASH-USD-predict-kknnLDDD-ensamble" # 48
#indicatorFile <- "DASH-USD-predict-kknnLDDE-ensamble" # 61
#indicatorFile <- "DASH-USD-predict-kknnLDE-ensamble" # 0.38

#indicatorFile <- "EOS-USD-predict-ensamble" # 54
#indicatorFile <- "EOS-USD-predict-glmLDA-ensamble" # 51
#indicatorFile <- "EOS-USD-predict-glmLDAA-ensamble" # 46
#indicatorFile <- "EOS-USD-predict-glmLDAB-ensamble" # 54
#indicatorFile <- "EOS-USD-predict-glmLDB-ensamble" # 54
#indicatorFile <- "EOS-USD-predict-glmLDC-ensamble" # 51
#indicatorFile <- "EOS-USD-predict-glmLDD-ensamble" # 54
#indicatorFile <- "EOS-USD-predict-glmLDG-ensamble" # 56
#indicatorFile <- "EOS-USD-predict-kknnLDAC-ensamble" # 59
#indicatorFile <- "EOS-USD-predict-kknnLDAD-ensamble" # 46
#indicatorFile <- "EOS-USD-predict-kknnLDAE-ensamble" # 51
#indicatorFile <- "EOS-USD-predict-kknnLDAF-ensamble" # 51
#indicatorFile <- "EOS-USD-predict-kknnLDAG-ensamble" # 59
#indicatorFile <- "EOS-USD-predict-kknnLDAH-ensamble" # 56
#indicatorFile <- "EOS-USD-predict-kknnLDB-ensamble" # 61
#indicatorFile <- "EOS-USD-predict-kknnLDC-ensamble" # 64
#indicatorFile <- "EOS-USD-predict-kknnLDD-ensamble" # 62
#indicatorFile <- "EOS-USD-predict-kknnLDDA-ensamble" # 61
#indicatorFile <- "EOS-USD-predict-kknnLDDB-ensamble" # 51
#indicatorFile <- "EOS-USD-predict-kknnLDDC-ensamble" # 51
#indicatorFile <- "EOS-USD-predict-kknnLDDD-ensamble" # 48
#indicatorFile <- "EOS-USD-predict-kknnLDDE-ensamble" # 64
#indicatorFile <- "EOS-USD-predict-kknnLDE-ensamble" # 48
#indicatorFile <- "EOS-USD-predict-xgblinearLN-ensamble" # 67

#indicatorFile <- "LINK-USD-predict-ensamble" # 59
#indicatorFile <- "LINK-USD-predict-glmLDB-ensamble" # 46
#indicatorFile <- "LINK-USD-predict-glmLDC-ensamble" # 49
#indicatorFile <- "LINK-USD-predict-glmLDG-ensamble" # 69
#indicatorFile <- "LINK-USD-predict-kknnLDAC-ensamble" # 51
#indicatorFile <- "LINK-USD-predict-kknnLDAE-ensamble" # 62
#indicatorFile <- "LINK-USD-predict-kknnLDAF-ensamble" # 62
#indicatorFile <- "LINK-USD-predict-kknnLDAG-ensamble" # 59
#indicatorFile <- "LINK-USD-predict-kknnLDAH-ensamble" # 59
#indicatorFile <- "LINK-USD-predict-kknnLDB-ensamble" # 62
#indicatorFile <- "LINK-USD-predict-kknnLDC-ensamble" # 56
#indicatorFile <- "LINK-USD-predict-kknnLDD-ensamble" # 62
#indicatorFile <- "LINK-USD-predict-kknnLDDA-ensamble" # 45
#indicatorFile <- "LINK-USD-predict-kknnLDDB-ensamble" # 32
#indicatorFile <- "LINK-USD-predict-kknnLDDC-ensamble" # 45
#indicatorFile <- "LINK-USD-predict-kknnLDDD-ensamble" # 41
#indicatorFile <- "LINK-USD-predict-kknnLDDE-ensamble" # 61
#indicatorFile <- "LINK-USD-predict-kknnLDE-ensamble" # 58

#indicatorFile <- "LTC-USD-predict-glmLDAA-ensamble" # 54
#indicatorFile <- "LTC-USD-predict-glmLDAB-ensamble" # 59
#indicatorFile <- "LTC-USD-predict-glmLDB-ensamble" # 49
#indicatorFile <- "LTC-USD-predict-glmLDC-ensamble" # 56
#indicatorFile <- "LTC-USD-predict-glmLDG-ensamble" # 54
#indicatorFile <- "LTC-USD-predict-kknnLDAD-ensamble" # 67
#indicatorFile <- "LTC-USD-predict-kknnLDAE-ensamble" # 67
#indicatorFile <- "LTC-USD-predict-kknnLDAG-ensamble" # 59
#indicatorFile <- "LTC-USD-predict-kknnLDAH-ensamble" # 59
#indicatorFile <- "LTC-USD-predict-kknnLDB-ensamble" # 62
#indicatorFile <- "LTC-USD-predict-kknnLDC-ensamble" # 51
#indicatorFile <- "LTC-USD-predict-kknnLDD-ensamble" # 59
#indicatorFile <- "LTC-USD-predict-kknnLDDA-ensamble" # 51
#indicatorFile <- "LTC-USD-predict-kknnLDDB-ensamble" # 51
#indicatorFile <- "LTC-USD-predict-kknnLDDC-ensamble" # 77
#indicatorFile <- "LTC-USD-predict-kknnLDDD-ensamble" # 51
#indicatorFile <- "LTC-USD-predict-kknnLDDE-ensamble" # 68
#indicatorFile <- "LTC-USD-predict-kknnLDE-ensamble" # 48

#indicatorFile <- "ZEC-USD-predict-glmLDAB-ensamble" # 56
#indicatorFile <- "ZEC-USD-predict-glmLDC-ensamble" # 51
#indicatorFile <- "ZEC-USD-predict-glmLDD-ensamble" # 49
#indicatorFile <- "ZEC-USD-predict-glmLDG-ensamble" # 51
#indicatorFile <- "ZEC-USD-predict-kknnLDAD-ensamble" # 67
#indicatorFile <- "ZEC-USD-predict-kknnLDAE-ensamble" # 62
#indicatorFile <- "ZEC-USD-predict-kknnLDAG-ensamble" # 64
#indicatorFile <- "ZEC-USD-predict-kknnLDAH-ensamble" # 51
#indicatorFile <- "ZEC-USD-predict-kknnLDB-ensamble" # 62
#indicatorFile <- "ZEC-USD-predict-kknnLDC-ensamble" # 56
#indicatorFile <- "ZEC-USD-predict-kknnLDD-ensamble" # 74
#indicatorFile <- "ZEC-USD-predict-kknnLDDA-ensamble" # 48
#indicatorFile <- "ZEC-USD-predict-kknnLDDB-ensamble" # 54
#indicatorFile <- "ZEC-USD-predict-kknnLDDC-ensamble" # 51
#indicatorFile <- "ZEC-USD-predict-kknnLDDD-ensamble" # 54
#indicatorFile <- "ZEC-USD-predict-kknnLDDE-ensamble" # 58
#indicatorFile <- "ZEC-USD-predict-kknnLDE-ensamble" # 54

#indicatorFile <- "ZRX-USD-predict-glmLDAA-ensamble" # 62
#indicatorFile <- "ZRX-USD-predict-glmLDB-ensamble" # 72
#indicatorFile <- "ZRX-USD-predict-glmLDG-ensamble" # 56
#indicatorFile <- "ZRX-USD-predict-kknnLDAD-ensamble" # 56
#indicatorFile <- "ZRX-USD-predict-kknnLDAE-ensamble" # 64
#indicatorFile <- "ZRX-USD-predict-kknnLDAG-ensamble" # 59
#indicatorFile <- "ZRX-USD-predict-kknnLDAH-ensamble" # 56
#indicatorFile <- "ZRX-USD-predict-kknnLDB-ensamble" # 69
#indicatorFile <- "ZRX-USD-predict-kknnLDC-ensamble" # 51
#indicatorFile <- "ZRX-USD-predict-kknnLDD-ensamble" # 51
#indicatorFile <- "ZRX-USD-predict-kknnLDDA-ensamble" # 54
#indicatorFile <- "ZRX-USD-predict-kknnLDDB-ensamble" # 51
#indicatorFile <- "ZRX-USD-predict-kknnLDDC-ensamble" # 64
#indicatorFile <- "ZRX-USD-predict-kknnLDDD-ensamble" # 67
#indicatorFile <- "ZRX-USD-predict-kknnLDDE-ensamble" # 54
#indicatorFile <- "ZRX-USD-predict-kknnLDE-ensamble" # 64

dailyIndicator <- fread(
  paste(basePath, indicatorFile, ".csv", sep = "")
)
dailyIndicator[, Date := as.Date(Date)]
dailyIndicator[, YearMonth := format(Date, "%Y-%m")]
dailyIndicator <- merge(securityDataTest[, c('Date', 'Mid', 'diffPercent', 'Eff', 'Actbin')], dailyIndicator, by = "Date")

calculateAccuracy <- function(monthlyData) {
  categoryLevels = c("buy", "sell")
  confusionData <- table(
    actualclass = factor(monthlyData$Actbin, levels = categoryLevels),
    predictedclass = factor(monthlyData$EffPred, levels = categoryLevels)
  ) %>% confusionMatrix()

  accuracy <- confusionData$overall['Accuracy']
  prevalence <- confusionData$byClass['Prevalence']

  list(Accuracy = accuracy, Prevalence = prevalence)
}

cat("\n", symbolTest, "montly predictions performacne test:", "\n")
accuracyTest <- dailyIndicator[, calculateAccuracy(.SD), by = "YearMonth"]
print(accuracyTest)
describe(accuracyTest[, c('Accuracy', 'Prevalence')])