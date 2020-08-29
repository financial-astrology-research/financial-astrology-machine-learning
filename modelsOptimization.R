source("./indicatorPlots.r")

symbol <- "EOS-USD"
securityData <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
securityData[, fold := kfold(securityData, k = 5)]

# Experiment grid search with different aspects energy factors.
hourlyAspects <- prepareHourlyAspectsModelH2()
enOpts <- seq(-1, 1, by=1)
searchResult <- grid_search(
  predictSecurityModelH2A,
  list(
    en0 = enOpts, en30 = enOpts, en45 = enOpts, en51 = enOpts, en60 = enOpts, en72 = enOpts, en90 = enOpts,
    en103 = enOpts, en120 = enOpts, en135 = enOpts, en144 = enOpts, en150 = enOpts, en180 = enOpts
  ),
  speedDecay = 0.001,
  security = securityData,
  hourlyAspects = hourlyAspects,
  n.iter = 1
)

print(searchResult)