# Title     : Test accuracy of ModelK indicator predictions.

# Test latest price period accuracy.
source("analysis.r")
symbol <- "BAT-USD"
securityData <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2020-08-16")

symbolNormalized <- str_replace(symbol, "-", "")
indicatorFile <- paste("ml-", str_replace(symbolNormalized, "USD", "USDT"), "-daily", sep = "")
dailyIndicator <- fread(
  paste("/Users/pablocc/Sites/own/trading-signal-processing/csv_indicators/", indicatorFile, ".csv", sep = "")
)
dailyIndicator[, Date := as.Date(Date)]
dailyIndicator <- merge(securityData[, c('Date', 'diffPercent')], dailyIndicator, by = "Date")
dailyIndicator[, Actual := as.character(cut(diffPercent, c(-100, 0, 100), c("sell", "buy")))]

table(
  actualclass = as.character(dailyIndicator$Actual),
  predictedclass = as.character(dailyIndicator$Action)
) %>%
  confusionMatrix() %>%
  print()

