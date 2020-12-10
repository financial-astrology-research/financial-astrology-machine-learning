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
  symbolId <- paste(filenameParts[1], filenameParts[2], "T", sep = "")
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

# Calculate index signal based on the majority of all symbols signals side.
allSymbolsSignalIndex[,
  Action := ifelse(buy > sell, "buy",
    ifelse(buy == sell, "neutral", "sell")
  )
]

indexFilePath <- paste(targetDirectory, "ml-signals-index.csv", sep = "/")
cat("Daily signals count index exported to:", indexFilePath)
fwrite(allSymbolsSignalIndex, indexFilePath)