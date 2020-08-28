source("./indicatorPlots.r")

symbol <- "EOS-USD"
securityData <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
securityData[, fold := kfold(securityData, k = 5)]

# Experiment grid search with different aspects energy factors.
hourlyAspects <- prepareHourlyAspectsModelH2()
enOpts <- seq(-3, 3, by=0.5)
searchResult <- grid_search(
  predictSecurityModelH2A,
  list(en0 = enOpts, en30 = enOpts, en45 = enOpts),
  speedDecay = 0.001,
  security = securityData,
  hourlyAspects = hourlyAspects,
  n.iter = 1
)

print(searchResult)