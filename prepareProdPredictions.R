# Title     : Prepare selected models predictions for production usage.
# Created by: pablocc
# Created on: 09/12/2020

library(data.table)

selectionFile <- "../hisdata/predfiles_selected.csv"
sourceDirectory <- "~/Desktop/ModelsPred"
targetDirectory <- "~/Desktop/ModelsProd"
predictionsList <- fread(selectionFile)

preparePredictionCSV <- function(predictFilename) {
  cat("Processing: ", predictFilename, "\n")
  filenameParts <- unlist(strsplit(predictFilename, "-"))
  symbolId <- paste(filenameParts[1], filenameParts[2], sep = "")
  sourceFilePath <- paste(sourceDirectory, predictFilename, sep = "/")
  predictionTable <- fread(sourceFilePath)
  targetFilename <- paste("ml", symbolId, "daily.csv", sep = "-")
  targetFilePath <- paste(targetDirectory, targetFilename, sep = "/")

  exportCols <- c('Date', 'EffPred')
  fwrite(predictionTable[, ..exportCols], targetFilePath)
  cat(symbolId, "predictions exported to:", targetFilePath, "\n")

  return(predictionTable[, ..exportCols])
}

# Calculate a daily buy/sell signal count index for all symbols.
allSymbolsPredictions <- setDT(rbindlist(lapply(predictionsList$filename, preparePredictionCSV)))
allSymbolsSignalIndex <- dcast(
  allSymbolsPredictions,
  Date ~ EffPred,
  fun.aggregate = SIT::count,
  value.var = "EffPred",
  fill = 0
)
setDT(allSymbolsSignalIndex)

indexFilePath <- paste(targetDirectory, "ml-signals-index.csv", sep = "/")
cat("Daily signals count index exported to:", indexFilePath)
fwrite(allSymbolsSignalIndex, indexFilePath)