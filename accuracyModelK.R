# Title     : Test accuracy of ModelK indicator predictions.

# Test latest price period accuracy.
source("analysis.r")
symbolTest <- "LINK-USD"
securityDataTest <- mainOpenSecurity(symbolTest, 14, 28, "%Y-%m-%d", "2020-08-16")
# basePath <- "~/Sites/own/trading-signal-processing/csv_indicators/"
basePath <- "~/Desktop/"

symbolNormalized <- str_replace(symbolTest, "-", "")
indicatorFile <- paste("ml-", str_replace(symbolNormalized, "USD", "USDT"), "-daily", sep = "")
dailyIndicator <- fread(
  paste(basePath, indicatorFile, ".csv", sep = "")
)
dailyIndicator[, Date := as.Date(Date)]
dailyIndicator <- merge(securityDataTest[, c('Date', 'diffPercent')], dailyIndicator, by = "Date")
dailyIndicator[, Actual := as.character(cut(diffPercent, c(-100, 0, 100), c("sell", "buy")))]

table(
  actualclass = as.character(dailyIndicator$Actual),
  predictedclass = as.character(dailyIndicator$Action)
) %>%
  confusionMatrix() %>%
  print()

