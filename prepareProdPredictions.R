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

  return(targetFilePath)
}

productionFiles <- lapply(predictionsList$filename, preparePredictionCSV)
cat("Production prediction files export completed:\n", unlist(productionFiles))