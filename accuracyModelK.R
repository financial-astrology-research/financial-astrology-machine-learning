# Title     : Test accuracy of different models predictions using latest price.

rm(list = ls())
library(caret)
source("analysis.r")

symbolTest <- "ZRX-USD"
securityDataTest <- mainOpenSecurity(
  symbolTest, 14, 28, "%Y-%m-%d", "2020-09-25"
)
#basePath <- "~/Sites/own/trading-signal-processing/csv_indicators/"
#basePath <- "~/Desktop/"
basePath <- "~/Desktop/ModelsPred/"

symbolNormalized <- str_replace(symbolTest, "-", "")

#indicatorFile <- "ADA-USD-predict-ensamble" # 67
#indicatorFile <- "ADA-USD-predict-glmLDC-ensamble" # 56
#indicatorFile <- "ADA-USD-predict-glmLDD-ensamble" # 54
#indicatorFile <- "ADA-USD-predict-glmLDF-ensamble" # 62
#indicatorFile <- "ADA-USD-predict-glmLDG-ensamble" # 62
#indicatorFile <- "ADA-USD-predict-kknnLDA-ensamble" # 44
#indicatorFile <- "ADA-USD-predict-kknnLDAA-ensamble" # 49
#indicatorFile <- "ADA-USD-predict-kknnLDAC-ensamble" # 59
#indicatorFile <- "ADA-USD-predict-kknnLDAD-ensamble" # 64
#indicatorFile <- "ADA-USD-predict-kknnLDAE-ensamble" # 64
#indicatorFile <- "ADA-USD-predict-kknnLDAF-ensamble" # 69
#indicatorFile <- "ADA-USD-predict-kknnLDAG-ensamble" # 61
#indicatorFile <- "ADA-USD-predict-kknnLDAH-ensamble" # 59
#indicatorFile <- "ADA-USD-predict-kknnLDB-ensamble" # 59
#indicatorFile <- "ADA-USD-predict-xgblinearLN-ensamble" # 44
#indicatorFile <- "ADA-USD-predict-kknnLDAD-ensamble" # 64

#indicatorFile <- "BAT-USD-predict-ensamble" # 56
#indicatorFile <- "BAT-USD-predict-glmLDAB-ensamble" # 59
#indicatorFile <- "BAT-USD-predict-glmLDB-ensamble" # 51
#indicatorFile <- "BAT-USD-predict-glmLDC-ensamble" # 46
#indicatorFile <- "BAT-USD-predict-glmLDD-ensamble" # 49
#indicatorFile <- "BAT-USD-predict-glmLDF-ensamble" # 62
#indicatorFile <- "BAT-USD-predict-glmLDG-ensamble" # 64
#indicatorFile <- "BAT-USD-predict-kknnLDA-ensamble" # 56
#indicatorFile <- "BAT-USD-predict-kknnLDAA-ensamble" # 56
#indicatorFile <- "BAT-USD-predict-kknnLDAB-ensamble" # 64
#indicatorFile <- "BAT-USD-predict-kknnLDAD-ensamble" # 54
#indicatorFile <- "BAT-USD-predict-kknnLDAE-ensamble" # 56
#indicatorFile <- "BAT-USD-predict-kknnLDAF-ensamble" # 56
#indicatorFile <- "BAT-USD-predict-kknnLDAG-ensamble" # 62
#indicatorFile <- "BAT-USD-predict-kknnLDAH-ensamble" # 59
#indicatorFile <- "BAT-USD-predict-kknnLDB-ensamble" # 61
#indicatorFile <- "BAT-USD-predict-xgblinearLH-ensamble" # 54
#indicatorFile <- "BAT-USD-predict-xgblinearLI-ensamble" # 54
#indicatorFile <- "BAT-USD-predict-xgblinearLJ-ensamble" # 44
#indicatorFile <- "BAT-USD-predict-xgblinearLN-ensamble" # 59

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
#indicatorFile <- "BTC-USD-predict-kknnLDB-ensamble" # 72
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
#indicatorFile <- "DASH-USD-predict-kknnLDB-ensamble" # 51

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
#indicatorFile <- "EOS-USD-predict-kknnLDB-ensamble" # 53
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
#indicatorFile <- "LINK-USD-predict-kknnLDB-ensamble" # 56

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

#indicatorFile <- "ZEC-USD-predict-glmLDAB-ensamble" # 56
#indicatorFile <- "ZEC-USD-predict-glmLDC-ensamble" # 51
#indicatorFile <- "ZEC-USD-predict-glmLDD-ensamble" # 49
#indicatorFile <- "ZEC-USD-predict-glmLDG-ensamble" # 51
#indicatorFile <- "ZEC-USD-predict-kknnLDAD-ensamble" # 67
#indicatorFile <- "ZEC-USD-predict-kknnLDAE-ensamble" # 62
#indicatorFile <- "ZEC-USD-predict-kknnLDAG-ensamble" # 64
#indicatorFile <- "ZEC-USD-predict-kknnLDAH-ensamble" # 51
#indicatorFile <- "ZEC-USD-predict-kknnLDB-ensamble" # 64

#indicatorFile <- "ZRX-USD-predict-glmLDAA-ensamble" # 62
#indicatorFile <- "ZRX-USD-predict-glmLDB-ensamble" # 72
#indicatorFile <- "ZRX-USD-predict-glmLDG-ensamble" # 56
#indicatorFile <- "ZRX-USD-predict-kknnLDAD-ensamble" # 56
#indicatorFile <- "ZRX-USD-predict-kknnLDAE-ensamble" # 64
#indicatorFile <- "ZRX-USD-predict-kknnLDAG-ensamble" # 59
#indicatorFile <- "ZRX-USD-predict-kknnLDAH-ensamble" # 56
#indicatorFile <- "ZRX-USD-predict-kknnLDB-ensamble" # 67

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
