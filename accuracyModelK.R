# Title     : Test accuracy of different models predictions using latest price.

library(caret)
source("analysis.r")
symbolTest <- "ZRX-USD"
securityDataTest <- mainOpenSecurity(
  symbolTest, 14, 28, "%Y-%m-%d", "2020-09-25"
)
#basePath <- "~/Sites/own/trading-signal-processing/csv_indicators/"
basePath <- "~/Desktop/ModelsPred/"

symbolNormalized <- str_replace(symbolTest, "-", "")

#indicatorFile <- "ADA-USD-predict-ensamble" # 0.67
#indicatorFile <- "ADA-USD-predict-xgblinear-ensamble" # 0.61
#indicatorFile <- "ADA-USD-predict-xgblinearLI-ensamble" # 0.64
#indicatorFile <- "ADA-USD-predict-xgblinearLJ-ensamble" # 0.67
#indicatorFile <- "ADA-USD-predict-xgblinearLM-ensamble" # 0.49
#indicatorFile <- "ADA-USD-predict-glmLDA-ensamble" # 0.68

#indicatorFile <- "BAT-USD-predict-ensamble" # 0.56
#indicatorFile <- "BAT-USD-predict-xgblinear-ensamble" # 0.52
#indicatorFile <- "BAT-USD-predict-xgblinearLG-ensamble" # 0.41
#indicatorFile <- "BAT-USD-predict-xgblinearLI-ensamble" # 0.47
#indicatorFile <- "BAT-USD-predict-xgblinearLJ-ensamble" # 0.41
#indicatorFile <- "BAT-USD-predict-xgblinearLM-ensamble" # 0.59
#indicatorFile <- "BAT-USD-predict-xgblinearm2-ensamble" # 0.5
#indicatorFile <- "BAT-USD-predict-glmLDA-ensamble" # 0.56

#indicatorFile <- "BNB-USD-predict-ensamble" # 0.43
#indicatorFile <- "BNB-USD-predict-xgblinear-ensamble" # 0.57
#indicatorFile <- "BNB-USD-predict-xgblinearLI-ensamble" # 0.56
#indicatorFile <- "BNB-USD-predict-xgblinearLJ-ensamble" # 0.4
#indicatorFile <- "BNB-USD-predict-xgblinearLN-ensamble" # 0.7
#indicatorFile <- "BNB-USD-predict-xgblinearm3-ensamble" # 0.53
#indicatorFile <- "BNB-USD-predict-glmLDA-ensamble" # 0.53

#indicatorFile <- "BTC-USD-predict-xgblinear-ensamble" # 0.42
#indicatorFile <- "BTC-USD-predict-xgblinearLI-ensamble" # 0.51
#indicatorFile <- "BTC-USD-predict-xgblinearLJ-ensamble" # 0.57
#indicatorFile <- "BTC-USD-predict-xgblinearLM-ensamble" # 0.45
#indicatorFile <- "BTC-USD-predict-glmLDA-ensamble" # 0.57

#indicatorFile <- "EOS-USD-predict-ensamble" # 0.56
#indicatorFile <- "EOS-USD-predict-xgblinear-ensamble" # 0.62
#indicatorFile <- "EOS-USD-predict-xgblinearLI-ensamble" # 0.5
#indicatorFile <- "EOS-USD-predict-xgblinearLJ-ensamble" # 0.56
#indicatorFile <- "EOS-USD-predict-xgblinearLM-ensamble" # 0.56
#indicatorFile <- "EOS-USD-predict-glmLDA-ensamble" # 0.53

#indicatorFile <- "DASH-USD-predict-xgblinear-ensamble" # 0.60
#indicatorFile <- "DASH-USD-predict-xgblinearLM-ensamble" # 0.48
#indicatorFile <- "DASH-USD-predict-glmLDA-ensamble" # 0.57

#indicatorFile <- "LINK-USD-predict-ensamble" # 0.53
#indicatorFile <- "ml-LINK-USD-daily-avnnet" # 0.43
#indicatorFile <- "LINK-USD-predict-xgblinear-ensamble" # 0.48
#indicatorFile <- "LINK-USD-predict-xgblinearLI-ensamble" # 0.59
#indicatorFile <- "LINK-USD-predict-xgblinearLJ-ensamble" # 0.56
#indicatorFile <- "LINK-USD-predict-xgblinearLM-ensamble" # 0.43
#indicatorFile <- "LINK-USD-predict-glmLDA-ensamble" # 0.53

#indicatorFile <- "LTC-USD-predict-ensamble" # 0.60
#indicatorFile <- "LTC-USD-predict-glmLDA-ensamble" # 0.57

#indicatorFile <- "ZEC-USD-predict-ensamble" # 0.53
#indicatorFile <- "ZEC-USD-predict-glmLDA-ensamble" # 0.53

#indicatorFile <- "ZRX-USD-predict-ensamble" # 0.65
#indicatorFile <- "ZRX-USD-predict-xgblinear-ensamble" # 0.68
#indicatorFile <- "ZRX-USD-predict-glmLDA-ensamble" # 0.66


dailyIndicator <- fread(
  paste(basePath, indicatorFile, ".csv", sep = "")
)
dailyIndicator[, Date := as.Date(Date)]
dailyIndicator <- merge(securityDataTest[, c('Date', 'Mid', 'diffPercent', 'Eff', 'Actbin')], dailyIndicator, by = "Date")

cat("\n")
print(dailyIndicator[, c('Date', 'Mid', 'diffPercent', 'Eff', 'Actbin', 'EffPred')])
cat("\n\n")

table(
  actualclass = as.character(dailyIndicator$Actbin),
  predictedclass = as.character(dailyIndicator$EffPred)
) %>%
  confusionMatrix() %>%
  print()
