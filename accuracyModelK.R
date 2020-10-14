# Title     : Test accuracy of ModelK indicator predictions.

# Test latest price period accuracy.
library(caret)
source("analysis.r")
symbolTest <- "BAT-USD"
securityDataTest <- mainOpenSecurity(symbolTest, 14, 28, "%Y-%m-%d", "2020-09-13")
#basePath <- "~/Sites/own/trading-signal-processing/csv_indicators/"
basePath <- "~/Desktop/ModelsPred/"

symbolNormalized <- str_replace(symbolTest, "-", "")
#indicatorFile <- "ADA-USD-predict-ensamble"
#indicatorFile <- "ZRX-USD-predict-ensamble"
#indicatorFile <- "BAT-USD-predict-ensamble"
#indicatorFile <- "ml-LINK-USD-daily-avnnet"
#indicatorFile <- "ml-BNBUSDT-daily"
#indicatorFile <- "ml-LINK-USD-daily-avnnet"
indicatorFile <- "ml-BATUSDT-daily"
dailyIndicator <- fread(
  paste(basePath, indicatorFile, ".csv", sep = "")
)
dailyIndicator[, Date := as.Date(Date)]
dailyIndicator <- merge(securityDataTest[, c('Date', 'Eff', 'Actbin')], dailyIndicator, by = "Date")

table(
  actualclass = as.character(dailyIndicator$Actbin),
  predictedclass = as.character(dailyIndicator$Action)
) %>%
  confusionMatrix() %>%
  print()
