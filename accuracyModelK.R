# Title     : Test accuracy of different models predictions using latest price.

rm(list = ls())
library(caret)
source("analysis.r")

symbolTest <- "ZRX-USD"
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
#indicatorFile <- "ADA-USD-predict-kknnLDDF-ensamble" # A: 63, 11 / P: 57, 10
#indicatorFile <- "ADA-USD-predict-kknnLDE-ensamble" # A: 65, 12 / P: 52, 22
#indicatorFile <- "ADA-USD-predict-kknnLDF-ensamble" # A: 65, 15 / P: 47, 23
#indicatorFile <- "ADA-USD-predict-kknnLDH-ensamble" # A: 69, 12 / P: 56, 14 (Best)
#indicatorFile <- "ADA-USD-predict-kknnLDJ-ensamble" # A: 69, 10 / P: 50, 18
#indicatorFile <- "ADA-USD-predict-kknnLDJA-ensamble" # A: 65, 12 / P: 57, 14
#indicatorFile <- "ADA-USD-predict-kknnLDJB-ensamble" # A: 71, 7 / P: 60, 10 (Best)
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
#indicatorFile <- "BAT-USD-predict-kknnLDDF-ensamble" # A: 63, 11 / P: 59, 7
#indicatorFile <- "BAT-USD-predict-kknnLDE-ensamble" # A: 66, 8 / P: 49, 15 (Best)
#indicatorFile <- "BAT-USD-predict-kknnLDF-ensamble" # A: 61, 13 / P: 63, 14
#indicatorFile <- "BAT-USD-predict-kknnLDH-ensamble" # A: 63, 11 / P: 58, 6 (Best)
#indicatorFile <- "BAT-USD-predict-kknnLDJ-ensamble" # A: 65, 14 / P: 52, 13
#indicatorFile <- "BAT-USD-predict-kknnLDJA-ensamble" # A: 65, 10 / P: 60, 9 (Best)
#indicatorFile <- "BAT-USD-predict-kknnLDJB-ensamble" # A: 61, 10 / P: 55, 9
#indicatorFile <- "BAT-USD-predict-xgblinearLH-ensamble" # A: 62, 11 / P: 49, 19
#indicatorFile <- "BAT-USD-predict-xgblinearLI-ensamble" # A: 62, 11 / P: 49, 19
#indicatorFile <- "BAT-USD-predict-xgblinearLJ-ensamble" # A: 66, 10 / P: 45, 15
#indicatorFile <- "BAT-USD-predict-xgblinearLN-ensamble" # A: 64, 9 / P: 50, 15

#indicatorFile <- "BNB-USD-predict-glmLDAB-ensamble" # A: 56, 11 / P: 55, 18
#indicatorFile <- "BNB-USD-predict-glmLDF-ensamble" # A: 54, 11 / P: 52, 23
#indicatorFile <- "BNB-USD-predict-glmLDG-ensamble" # A: 58, 6 / P: 65, 17
#indicatorFile <- "BNB-USD-predict-kknnLDAA-ensamble" # A: 64, 12 / P: 53, 18
#indicatorFile <- "BNB-USD-predict-kknnLDAB-ensamble" # A: 76, 23 / P: 56, 10
#indicatorFile <- "BNB-USD-predict-kknnLDAE-ensamble" # A: 61, 11 / P: 57, 15
#indicatorFile <- "BNB-USD-predict-kknnLDAF-ensamble" # A: 61, 12 / P: 50, 20
#indicatorFile <- "BNB-USD-predict-kknnLDAG-ensamble" # A: 67, 7 / P: 57, 14 (Best)
#indicatorFile <- "BNB-USD-predict-kknnLDAH-ensamble" # A: 64, 10 / P: 51, 16
#indicatorFile <- "BNB-USD-predict-kknnLDB-ensamble" # A: 66, 8 / P: 53, 17
#indicatorFile <- "BNB-USD-predict-kknnLDC-ensamble" # A: 66, 8 / P: 54, 15 (Best)
#indicatorFile <- "BNB-USD-predict-kknnLDD-ensamble" # A: 64, 7 / P: 48, 15 (Best)
#indicatorFile <- "BNB-USD-predict-kknnLDDA-ensamble" # A: 69, 12 / P: 60, 12
#indicatorFile <- "BNB-USD-predict-kknnLDDB-ensamble" # A: 61, 13 / P: 54, 15
#indicatorFile <- "BNB-USD-predict-kknnLDDC-ensamble" # A: 67, 11 / P: 61, 11
#indicatorFile <- "BNB-USD-predict-kknnLDDD-ensamble" # A: 62, 13 / P: 57, 17
#indicatorFile <- "BNB-USD-predict-kknnLDDE-ensamble" # A: 64, 12 / P: 59, 15
#indicatorFile <- "BNB-USD-predict-kknnLDDF-ensamble" # A: 67, 12 / P: 59, 14
#indicatorFile <- "BNB-USD-predict-kknnLDE-ensamble" # A: 68, 9 / P: 50, 18 (Best)
#indicatorFile <- "BNB-USD-predict-kknnLDF-ensamble" # A: 65, 10 / P: 61, 12
#indicatorFile <- "BNB-USD-predict-kknnLDH-ensamble" # A: 66, 12 / P: 61, 10 (Best)
#indicatorFile <- "BNB-USD-predict-kknnLDJ-ensamble" # A: 68, 9 / P: 64, 12
#indicatorFile <- "BNB-USD-predict-kknnLDJA-ensamble" # A: 67, 11 / P: 59, 14 (Best)
#indicatorFile <- "BNB-USD-predict-kknnLDJB-ensamble" # A: 68, 12 / P: 57, 16
#indicatorFile <- "BNB-USD-predict-xgblinearLN-ensamble" # A: 70, 7 / P: 51, 19 (Best)

#indicatorFile <- "BTC-USD-predict-ensamble" # A: 50, 7 / P: 57, 10
#indicatorFile <- "BTC-USD-predict-glmLDAA-ensamble" # A: 59, 11 / P: 60, 14
#indicatorFile <- "BTC-USD-predict-glmLDB-ensamble" # A: 58, 12 / P: 62, 14
#indicatorFile <- "BTC-USD-predict-glmLDC-ensamble" # A: 65, 14 / P: 66, 13
#indicatorFile <- "BTC-USD-predict-glmLDD-ensamble" # A: 65, 12 / P: 67, 14
#indicatorFile <- "BTC-USD-predict-glmLDG-ensamble" # A: 61, 11 / P: 59, 17
#indicatorFile <- "BTC-USD-predict-kknnLDAB-ensamble" # A: 77, 17 / P: 51, 14
#indicatorFile <- "BTC-USD-predict-kknnLDAC-ensamble" # A: 62, 12 / P: 55, 13 (Best)
#indicatorFile <- "BTC-USD-predict-kknnLDAD-ensamble" # A: 61, 12 / P: 57, 12
#indicatorFile <- "BTC-USD-predict-kknnLDAE-ensamble" # A: 63, 11 / P: 60, 16
#indicatorFile <- "BTC-USD-predict-kknnLDAF-ensamble" # A: 69, 10 / P: 63, 16
#indicatorFile <- "BTC-USD-predict-kknnLDAG-ensamble" # A: 69, 11 / P: 56, 12 (Best)
#indicatorFile <- "BTC-USD-predict-kknnLDAH-ensamble" # A: 59, 14 / P: 53, 12
#indicatorFile <- "BTC-USD-predict-kknnLDB-ensamble" # A: 63, 11 / P: 60, 12
#indicatorFile <- "BTC-USD-predict-kknnLDC-ensamble" # A: 65, 8 / 56, 11 (Best)
#indicatorFile <- "BTC-USD-predict-kknnLDD-ensamble" # A: 66, 10 / 55, 9 (Best)
#indicatorFile <- "BTC-USD-predict-kknnLDDA-ensamble" # A: 65, 11 / 57, 11
#indicatorFile <- "BTC-USD-predict-kknnLDDB-ensamble" # A: 62, 17 / 53, 11
#indicatorFile <- "BTC-USD-predict-kknnLDDC-ensamble" # A: 62, 11 / 61, 10
#indicatorFile <- "BTC-USD-predict-kknnLDDD-ensamble" # A: 62, 17 / 56, 16
#indicatorFile <- "BTC-USD-predict-kknnLDDE-ensamble" # A: 64, 11 / 59, 12
#indicatorFile <- "BTC-USD-predict-kknnLDDF-ensamble" # A: 67, 13 / 60, 15
#indicatorFile <- "BTC-USD-predict-kknnLDE-ensamble" # A: 64, 15 / 57, 19
#indicatorFile <- "BTC-USD-predict-kknnLDF-ensamble" # A: 65, 15 / 60, 14
#indicatorFile <- "BTC-USD-predict-kknnLDH-ensamble" # A: 64, 11 / 53, 13
#indicatorFile <- "BTC-USD-predict-kknnLDJ-ensamble" # A: 67, 12 / 57, 16
#indicatorFile <- "BTC-USD-predict-kknnLDJA-ensamble" # A: 68, 11 / 54, 14 (Best)
#indicatorFile <- "BTC-USD-predict-kknnLDJB-ensamble" # A: 66, 14 / 54, 14
#indicatorFile <- "BTC-USD-predict-xgblinearLJ-ensamble" # A: 59, 10 / 61, 18

#indicatorFile <- "DASH-USD-predict-ensamble" # A: 59, 13 / P: 44, 20
#indicatorFile <- "DASH-USD-predict-glmLDA-ensamble" # A: 58, 13 / P: 44, 10
#indicatorFile <- "DASH-USD-predict-glmLDAA-ensamble" # A: 57, 15 / P: 43, 12
#indicatorFile <- "DASH-USD-predict-glmLDAB-ensamble" # A: 57, 11 / P: 43, 10
#indicatorFile <- "DASH-USD-predict-glmLDB-ensamble" # A: 56, 13 / P: 41, 12
#indicatorFile <- "DASH-USD-predict-glmLDC-ensamble" # A: 63, 14 / P: 51, 9 (Best)
#indicatorFile <- "DASH-USD-predict-glmLDG-ensamble" # A: 56, 14 / P: 47, 14
#indicatorFile <- "DASH-USD-predict-kknnLDAE-ensamble" # A: 60, 6 / P: 45, 16
#indicatorFile <- "DASH-USD-predict-kknnLDAF-ensamble" # A: 59, 9 / P: 46, 15
#indicatorFile <- "DASH-USD-predict-kknnLDAG-ensamble" # A: 60, 9 / P: 43, 12
#indicatorFile <- "DASH-USD-predict-kknnLDAH-ensamble" # A: 61, 15 / P: 44, 16
#indicatorFile <- "DASH-USD-predict-kknnLDB-ensamble" # A: 60, 13 / P: 32, 16
#indicatorFile <- "DASH-USD-predict-kknnLDC-ensamble" # A: 62, 12 / P: 45, 13 (Best)
#indicatorFile <- "DASH-USD-predict-kknnLDD-ensamble" # A: 59, 15 /  P: 46, 14
#indicatorFile <- "DASH-USD-predict-kknnLDDA-ensamble" # A: 62, 8 / P: 52, 10 (Best)
#indicatorFile <- "DASH-USD-predict-kknnLDDB-ensamble" # A: 57, 17 / P: 48, 13
#indicatorFile <- "DASH-USD-predict-kknnLDDC-ensamble" # A: 59, 12 / P: 46, 12
#indicatorFile <- "DASH-USD-predict-kknnLDDD-ensamble" # A: 58, 12 / P: 50, 12
#indicatorFile <- "DASH-USD-predict-kknnLDDE-ensamble" # A: 56, 13 / P: 51, 14
#indicatorFile <- "DASH-USD-predict-kknnLDDF-ensamble" # A: 59, 12 / P: 51, 9
#indicatorFile <- "DASH-USD-predict-kknnLDE-ensamble" # A: 58, 15 / P: 46, 13
#indicatorFile <- "DASH-USD-predict-kknnLDF-ensamble" # A: 58, 18 / P: 44, 14
#indicatorFile <- "DASH-USD-predict-kknnLDH-ensamble" # A: 63, 8 / P: 46, 13 (Best)
#indicatorFile <- "DASH-USD-predict-kknnLDJ-ensamble" # A: 63, 11 / P: 48, 15
#indicatorFile <- "DASH-USD-predict-kknnLDJA-ensamble" # A: 60, 12 / P: 50, 9
#indicatorFile <- "DASH-USD-predict-kknnLDJB-ensamble" # A: 61, 11 / P: 50, 11

#indicatorFile <- "EOS-USD-predict-ensamble" # A: 55, 13 / P: 41, 21
#indicatorFile <- "EOS-USD-predict-glmLDA-ensamble" # A: 53, 14 / P: 47, 17
#indicatorFile <- "EOS-USD-predict-glmLDAA-ensamble" # A: 56, 10 / P: 49, 15
#indicatorFile <- "EOS-USD-predict-glmLDAB-ensamble" # A: 56, 11 / P: 49, 15
#indicatorFile <- "EOS-USD-predict-glmLDB-ensamble" # A: 53, 12 / P: 56, 18
#indicatorFile <- "EOS-USD-predict-glmLDC-ensamble" # A: 58, 11 / P: 45, 17
#indicatorFile <- "EOS-USD-predict-glmLDD-ensamble" # A: 60, 17 / P: 53, 18
#indicatorFile <- "EOS-USD-predict-glmLDG-ensamble" # A: 60, 8 / P: 51, 18 (Best)
#indicatorFile <- "EOS-USD-predict-kknnLDAC-ensamble" # A: 64, 11 / P: 51, 13 (Best)
#indicatorFile <- "EOS-USD-predict-kknnLDAD-ensamble" # A: 63, 13 / P: 55, 19
#indicatorFile <- "EOS-USD-predict-kknnLDAE-ensamble" # A: 62, 13 / P: 54, 17
#indicatorFile <- "EOS-USD-predict-kknnLDAF-ensamble" # A: 63, 14 / P: 49, 13
#indicatorFile <- "EOS-USD-predict-kknnLDAG-ensamble" # A: 66, 12 / P: 50, 11
#indicatorFile <- "EOS-USD-predict-kknnLDAH-ensamble" # A: 62, 15 / P: 50, 13
#indicatorFile <- "EOS-USD-predict-kknnLDB-ensamble" # A: 65, 8 / P: 50, 12 (Best)
#indicatorFile <- "EOS-USD-predict-kknnLDC-ensamble" # A: 68, 9 / P: 49, 13 (Best)
#indicatorFile <- "EOS-USD-predict-kknnLDD-ensamble" # A: 64, 13 / P: 42, 18
#indicatorFile <- "EOS-USD-predict-kknnLDDA-ensamble" # A: 66, 5 / P: 47, 8 (Best)
#indicatorFile <- "EOS-USD-predict-kknnLDDB-ensamble" # A: 68, 14 / P: 53, 14
#indicatorFile <- "EOS-USD-predict-kknnLDDC-ensamble" # A: 61, 17 / P: 53, 14
#indicatorFile <- "EOS-USD-predict-kknnLDDD-ensamble" # A: 63, 16 / P: 50, 17
#indicatorFile <- "EOS-USD-predict-kknnLDDE-ensamble" # A: 64, 12 / P: 51, 13
#indicatorFile <- "EOS-USD-predict-kknnLDDF-ensamble" # A: 63, 14 / P: 58, 9
#indicatorFile <- "EOS-USD-predict-kknnLDE-ensamble" # A: 63, 17 / P: 51, 10
#indicatorFile <- "EOS-USD-predict-kknnLDF-ensamble" # A: 66, 11 / P: 51, 14
#indicatorFile <- "EOS-USD-predict-kknnLDH-ensamble" # A: 63, 14 / P: 50, 10
#indicatorFile <- "EOS-USD-predict-kknnLDJ-ensamble" # A: 66, 13 / P: 56, 16
#indicatorFile <- "EOS-USD-predict-kknnLDJA-ensamble" # A: 65, 12 / P: 54, 13 (Best)
#indicatorFile <- "EOS-USD-predict-kknnLDJB-ensamble" # A: 63, 15 / P: 49, 13
#indicatorFile <- "EOS-USD-predict-xgblinearLN-ensamble" # A: 69, 7 / 39, 21

#indicatorFile <- "LINK-USD-predict-ensamble" # A: 61, 7 / P: 65, 14
#indicatorFile <- "LINK-USD-predict-glmLDB-ensamble" # A: 58, 9 / P: 53, 15
#indicatorFile <- "LINK-USD-predict-glmLDC-ensamble" # A: 63, 10 / P: 57, 20
#indicatorFile <- "LINK-USD-predict-glmLDG-ensamble" # A: 60, 12 / P: 52, 18
#indicatorFile <- "LINK-USD-predict-kknnLDAC-ensamble" # A: 62, 11 / P: 55, 11 (Best)
#indicatorFile <- "LINK-USD-predict-kknnLDAE-ensamble" # A: 66, 7 / P: 63, 14
#indicatorFile <- "LINK-USD-predict-kknnLDAF-ensamble" # A: 65, 6 / P: 56, 14 (Best)
#indicatorFile <- "LINK-USD-predict-kknnLDAG-ensamble" # A: 65, 9 / P: 62, 14
#indicatorFile <- "LINK-USD-predict-kknnLDAH-ensamble" # A: 64, 9 / P: 56, 15 (Best)
#indicatorFile <- "LINK-USD-predict-kknnLDB-ensamble" # A: 63, 8 / P: 63, 12
#indicatorFile <- "LINK-USD-predict-kknnLDC-ensamble" # A: 63, 9 / P: 60, 11
#indicatorFile <- "LINK-USD-predict-kknnLDD-ensamble" # A: 64, 8 / P: 50, 12 (Best)
#indicatorFile <- "LINK-USD-predict-kknnLDDA-ensamble" # A: 68, 9 / P: 57, 9
#indicatorFile <- "LINK-USD-predict-kknnLDDB-ensamble" # A: 62, 14 / P: 54, 7
#indicatorFile <- "LINK-USD-predict-kknnLDDC-ensamble" # A: 61, 14 / P: 57, 11
#indicatorFile <- "LINK-USD-predict-kknnLDDD-ensamble" # A: 66, 13 / P: 51, 15
#indicatorFile <- "LINK-USD-predict-kknnLDDE-ensamble" # A: 60, 16 / P: 60, 13
#indicatorFile <- "LINK-USD-predict-kknnLDDF-ensamble" # A: 64, 12 / P: 60, 8
#indicatorFile <- "LINK-USD-predict-kknnLDE-ensamble" # A: 67, 10 / P: 53, 12 (Best)
#indicatorFile <- "LINK-USD-predict-kknnLDF-ensamble" # A: 65, 13 / P: 65, 10
#indicatorFile <- "LINK-USD-predict-kknnLDH-ensamble" # A: 61, 11 / P: 58, 12
#indicatorFile <- "LINK-USD-predict-kknnLDJ-ensamble" # A: 68, 10 / P: 52, 12 (Best)
#indicatorFile <- "LINK-USD-predict-kknnLDJA-ensamble" # A: 67, 11 / P: 58, 8 (Best)
#indicatorFile <- "LINK-USD-predict-kknnLDJB-ensamble" # A: 65, 12 / P: 55, 10

#indicatorFile <- "LTC-USD-predict-glmLDAA-ensamble" # A: 54, 10 / P: 46, 12
#indicatorFile <- "LTC-USD-predict-glmLDAB-ensamble" # A: 54, 11 / P: 51, 16
#indicatorFile <- "LTC-USD-predict-glmLDB-ensamble" # A: 51, 11 / P: 44, 13
#indicatorFile <- "LTC-USD-predict-glmLDC-ensamble" # A: 56, 15 / P: 35, 15
#indicatorFile <- "LTC-USD-predict-glmLDG-ensamble" # A: 62, 9 / P: 47, 15
#indicatorFile <- "LTC-USD-predict-kknnLDAD-ensamble" # A: 64, 13 / P: 48, 16
#indicatorFile <- "LTC-USD-predict-kknnLDAE-ensamble" # A: 62, 10 / P: 49, 9 (Best)
#indicatorFile <- "LTC-USD-predict-kknnLDAG-ensamble" # A: 64, 11 / P: 49, 14
#indicatorFile <- "LTC-USD-predict-kknnLDAH-ensamble" # A: 61, 13 / P: 49, 13
#indicatorFile <- "LTC-USD-predict-kknnLDB-ensamble" # A: 66, 8 / P: 50, 13 (Best)
#indicatorFile <- "LTC-USD-predict-kknnLDC-ensamble" # A: 63, 12 / P: 49, 11
#indicatorFile <- "LTC-USD-predict-kknnLDD-ensamble" # A: 64, 12 / P: 47, 16
#indicatorFile <- "LTC-USD-predict-kknnLDDA-ensamble" # A: 64, 13 / P: 56, 10
#indicatorFile <- "LTC-USD-predict-kknnLDDB-ensamble" # A: 66, 15 / P: 50, 13
#indicatorFile <- "LTC-USD-predict-kknnLDDC-ensamble" # A: 67, 11 / P: 55, 11 (Best)
#indicatorFile <- "LTC-USD-predict-kknnLDDD-ensamble" # A: 63, 14 / P: 50, 13
#indicatorFile <- "LTC-USD-predict-kknnLDDE-ensamble" # A: 63, 14 / P: 54, 11
#indicatorFile <- "LTC-USD-predict-kknnLDDF-ensamble" # A: 66, 12 / P: 52, 10 (Best)
#indicatorFile <- "LTC-USD-predict-kknnLDE-ensamble" # A: 68, 13 / P: 53, 13
#indicatorFile <- "LTC-USD-predict-kknnLDF-ensamble" # A: 67, 13 / P: 45, 14
#indicatorFile <- "LTC-USD-predict-kknnLDH-ensamble" # A: 64, 15 / P: 51, 11
#indicatorFile <- "LTC-USD-predict-kknnLDJ-ensamble" # A: 68, 14 / P: 53, 13
#indicatorFile <- "LTC-USD-predict-kknnLDJA-ensamble" # A: 67, 12 / P: 51, 11 (Best)
#indicatorFile <- "LTC-USD-predict-kknnLDJB-ensamble" # A: 65, 13 / P: 49, 13

#indicatorFile <- "ZEC-USD-predict-glmLDAB-ensamble" # A: 56, 7 / P: 38, 14
#indicatorFile <- "ZEC-USD-predict-glmLDC-ensamble" # A: 59: 15 / P: 47, 15
#indicatorFile <- "ZEC-USD-predict-glmLDD-ensamble" # A: 60, 11 / P: 37, 13
#indicatorFile <- "ZEC-USD-predict-glmLDG-ensamble" # A: 58, 9 / P: 30, 15
#indicatorFile <- "ZEC-USD-predict-kknnLDAD-ensamble" # A: 64, 12 / P: 45, 17
#indicatorFile <- "ZEC-USD-predict-kknnLDAE-ensamble" # A: 65, 9 / P: 45, 16 (Best)
#indicatorFile <- "ZEC-USD-predict-kknnLDAG-ensamble" # A: 66, 7 / P: 51, 12 (Best)
#indicatorFile <- "ZEC-USD-predict-kknnLDAH-ensamble" # A: 62, 11 / P: 48, 14
#indicatorFile <- "ZEC-USD-predict-kknnLDB-ensamble" # A: 63, 10 / P: 52, 14
#indicatorFile <- "ZEC-USD-predict-kknnLDC-ensamble" # A: 63, 10 / P: 51, 11 (Best)
#indicatorFile <- "ZEC-USD-predict-kknnLDD-ensamble" # A: 63, 15 / P: 48, 20
#indicatorFile <- "ZEC-USD-predict-kknnLDDA-ensamble" # A: 65, 11 / P: 49, 11 (Best)
#indicatorFile <- "ZEC-USD-predict-kknnLDDB-ensamble" # A: 65, 11 / P: 47, 15
#indicatorFile <- "ZEC-USD-predict-kknnLDDC-ensamble" # A: 66, 12 / P 49, 10
#indicatorFile <- "ZEC-USD-predict-kknnLDDD-ensamble" # A: 63, 10 / P: 44, 15
#indicatorFile <- "ZEC-USD-predict-kknnLDDE-ensamble" # A: 63, 10 / P: 44, 15
#indicatorFile <- "ZEC-USD-predict-kknnLDDF-ensamble" # A: 67, 13 / P: 50, 12
#indicatorFile <- "ZEC-USD-predict-kknnLDE-ensamble" # A: 65, 12 / P: 47, 14
#indicatorFile <- "ZEC-USD-predict-kknnLDF-ensamble" # A: 64, 12 / P: 56, 9
#indicatorFile <- "ZEC-USD-predict-kknnLDH-ensamble" # A: 68, 12 / P: 48, 8 (Best)
#indicatorFile <- "ZEC-USD-predict-kknnLDJ-ensamble" # A: 66, 11 / P: 51, 13 (Best)
#indicatorFile <- "ZEC-USD-predict-kknnLDJA-ensamble" # A: 68, 11 / P: 49, 12 (Best)
#indicatorFile <- "ZEC-USD-predict-kknnLDJB-ensamble" # A: 66, 10 / P: 45, 16

#indicatorFile <- "ZRX-USD-predict-glmLDAA-ensamble" # A: 56, 9 / P: 40, 16
#indicatorFile <- "ZRX-USD-predict-glmLDB-ensamble" # A: 58, 13 / P: 47, 13
#indicatorFile <- "ZRX-USD-predict-glmLDG-ensamble" # A: 59, 7 / P: 43, 17
#indicatorFile <- "ZRX-USD-predict-kknnLDAD-ensamble" # A: 64, 8 / P: 44, 19
#indicatorFile <- "ZRX-USD-predict-kknnLDAE-ensamble" # A: 65, 7 / P: 43, 17
#indicatorFile <- "ZRX-USD-predict-kknnLDAG-ensamble" # A: 63, 10 / P: 49, 16 (Best)
#indicatorFile <- "ZRX-USD-predict-kknnLDAH-ensamble" # A: 67, 11 / P: 50, 13 (Best)
#indicatorFile <- "ZRX-USD-predict-kknnLDB-ensamble" # A: 65, 5 / P: 44, 11 (Best)
#indicatorFile <- "ZRX-USD-predict-kknnLDC-ensamble" # A: 64, 8 / P: 46, 16
#indicatorFile <- "ZRX-USD-predict-kknnLDD-ensamble" # A: 60, 10 / P: 32, 14
#indicatorFile <- "ZRX-USD-predict-kknnLDDA-ensamble" # A: 66, 11 / P: 49, 9 (Best)
#indicatorFile <- "ZRX-USD-predict-kknnLDDB-ensamble" # A: 63, 13 / P: 46, 17
#indicatorFile <- "ZRX-USD-predict-kknnLDDC-ensamble" # A: 68, 7 / P: 53, 12 (Best)
#indicatorFile <- "ZRX-USD-predict-kknnLDDD-ensamble" # A: 71, 11 / P: 43, 17
#indicatorFile <- "ZRX-USD-predict-kknnLDDE-ensamble" # A: 58, 14 / P: 52, 11
#indicatorFile <- "ZRX-USD-predict-kknnLDDF-ensamble" # A: 64, 9 / P: 50, 10 (Best)
#indicatorFile <- "ZRX-USD-predict-kknnLDE-ensamble" # A: 69, 10 / P: 41, 12
#indicatorFile <- "ZRX-USD-predict-kknnLDE-ensamble" # A: 69, 10 / P: 41, 12
#indicatorFile <- "ZRX-USD-predict-kknnLDF-ensamble" # A: 64, 9 / P: 40, 14
#indicatorFile <- "ZRX-USD-predict-kknnLDH-ensamble" # A: 62, 13 / P: 50, 16
#indicatorFile <- "ZRX-USD-predict-kknnLDJ-ensamble" # A: 67, 5 / P: 43, 14 (Best)
#indicatorFile <- "ZRX-USD-predict-kknnLDJA-ensamble" # A: 67, 10 / P: 51, 10 (Best)
#indicatorFile <- "ZRX-USD-predict-kknnLDJB-ensamble" # A: 62, 10 / P: 47, 12

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