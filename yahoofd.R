library("quantmod")
startDate = as.Date("1970-01-01")
maxretry <- 1

processGetSymbol <- function(symbol) {
  for(t in 1:maxretry) {
    tryCatch({
      cat("Downloading ", symbol, "\t\t Attempt: ", t , "/", maxretry, "\n")
      symbol.df <- getSymbols(symbol, src="yahoo", from=startDate, env=NULL, return.class='data.frame')
      filename <- paste('./stocks/', symbol, ".csv", sep='')
      symbol.df <- cbind(rownames(symbol.df), symbol.df)
      names(symbol.df) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adj Close')
      write.csv(symbol.df, file=filename, row.names=F)
      cat("Sucessfully saved the stock data to ", filename, "\n")
      return(1)
    }, error = function(e) {
      print(e)
      return(0)
    })
  }
}

getMySymbolsData  <- function(listfile) {
  #Load the list of ticker symbols from a csv, each row contains a ticker
  symbolsls <- read.csv(paste("./symbols/", listfile, '.csv', sep=''),  header=F, stringsAsFactors=F)
  res <- lapply(symbolsls$V1, processGetSymbol)
}
