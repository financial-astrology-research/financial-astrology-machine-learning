# Title     : Test accuracy of ModelK indicator predictions.

# Test latest price period accuracy.
library(caret)
source("analysis.r")
symbolTest <- "LINK-USD"
securityDataTest <- mainOpenSecurity(symbolTest, 14, 28, "%Y-%m-%d", "2020-09-13")
#basePath <- "~/Sites/own/trading-signal-processing/csv_indicators/"
basePath <- "~/Desktop/ModelsPred/"

symbolNormalized <- str_replace(symbolTest, "-", "")
indicatorFile <- "LINK-USD-predict-ensamble"
dailyIndicator <- fread(
  paste(basePath, indicatorFile, ".csv", sep = "")
)
dailyIndicator[, Date := as.Date(Date)]
dailyIndicator <- merge(securityDataTest[, c('Date', 'Eff', 'Actbin')], dailyIndicator, by = "Date")

cat("DAILY EFFECT\n")
table(
  actualclass = as.character(dailyIndicator$Actbin),
  predictedclass = as.character(dailyIndicator$Action)
) %>%
  confusionMatrix() %>%
  print()
