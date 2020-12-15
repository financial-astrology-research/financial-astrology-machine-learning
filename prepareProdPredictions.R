# Title     : Prepare selected models predictions for production usage.
# Created by: pablocc
# Created on: 09/12/2020

library(data.table)

selectionFile <- "../hisdata/predfiles_selected.csv"
sourceDirectory <- "~/Desktop/ModelsPred"
targetDirectory <- "~/Desktop/ModelsProd"
predictionsList <- fread(selectionFile)
exportCols <- c('Date', 'EffPred')

getTargetFilePath <- function(symbolId) {
  targetFilename <- paste("ml", symbolId, "daily.csv", sep = "-")
  targetFilePath <- paste(targetDirectory, targetFilename, sep = "/")
}

getConsensusTargetFilePath <- function(symbolId) {
  targetFilename <- paste("ml", symbolId, "daily-consensus.csv", sep = "-")
  targetFilePath <- paste(targetDirectory, targetFilename, sep = "/")
}

preparePredictionCSV <- function(predictFilename) {
  cat("Processing: ", predictFilename, "\n")
  filenameParts <- unlist(strsplit(predictFilename, "-"))
  symbolId <- paste(filenameParts[1], filenameParts[2], "T", sep = "")
  sourceFilePath <- paste(sourceDirectory, predictFilename, sep = "/")
  predictionTable <- fread(sourceFilePath)
  targetFilePath <- getTargetFilePath(symbolId)

  fwrite(predictionTable[, ..exportCols], targetFilePath)
  cat(symbolId, "predictions exported to:", targetFilePath, "\n")

  return(predictionTable[, ..exportCols])
}

# Calculate a daily buy/sell signal count index for all symbols.
allSymbolsPredictions <- setDT(rbindlist(lapply(predictionsList$filename, preparePredictionCSV)))
allSymbolsPredictions[, Date := as.Date(Date)]
allSymbolsPredictions[, YearMonth := format(Date, "%Y-%m")]
allSymbolsPredictions[, YearWeek := format(Date, "%Y-%V")]

getIndexFilePath <- function(indexName) {
  targetFile <- paste("ml-all", indexName, "index.csv", sep = "-")
  indexFilePath <- paste(targetDirectory, targetFile, sep = "/")
}

computeAllSymbolsIndex <- function(byFormula, indexName) {
  allSymbolsIndex <- dcast(
    allSymbolsPredictions,
    byFormula,
    fun.aggregate = SIT::count,
    value.var = "EffPred",
    fill = 0
  )
  setDT(allSymbolsIndex)

  # Calculate index signal based on the majority of all symbols signals side.
  allSymbolsIndex[,
    Action := ifelse(buy > sell, "buy", ifelse(buy == sell, "neutral", "sell"))
  ]

  indexFilePath <- getIndexFilePath(indexName)
  cat("Signals count index exported to:", indexFilePath, "\n")
  fwrite(allSymbolsIndex, indexFilePath)
}

computeAllSymbolsIndex("Date ~ EffPred", "daily")
computeAllSymbolsIndex("YearWeek ~ EffPred", "weekly")
computeAllSymbolsIndex("YearMonth ~ EffPred", "monthly")

prepareConsensusPredictionCSV <- function(predictFilename) {
  cat("Processing: ", predictFilename, "\n")
  filenameParts <- unlist(strsplit(predictFilename, "-"))
  symbolId <- paste(filenameParts[1], filenameParts[2], "T", sep = "")
  targetFilePath <- getTargetFilePath(symbolId)
  indexFilePath <- getIndexFilePath("daily")
  symbolIndicator <- fread(targetFilePath)
  indexIndicator <- fread(indexFilePath)
  consensusIndicator <- merge(symbolIndicator, indexIndicator[, c('Date', 'Action')], by = "Date")
  setnames(consensusIndicator, c("Date", "SymbolAction", "IndexAction"))

  # Rely on index signal when symbol signal differs from index indicator.
  consensusIndicator[SymbolAction != IndexAction, EffPred := IndexAction]
  consensusIndicator[SymbolAction == IndexAction | SymbolAction == "neutral", EffPred := SymbolAction]
  consensusTargetFilePath <- getConsensusTargetFilePath(symbolId)
  fwrite(consensusIndicator[, ..exportCols], consensusTargetFilePath)
  cat("Consensus indicator exported to:", consensusTargetFilePath, "\n")

  return(consensusTargetFilePath)
}

consensusFiles <- lapply(predictionsList$filename, prepareConsensusPredictionCSV)