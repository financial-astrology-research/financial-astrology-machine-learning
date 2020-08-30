source("./indicatorPlots.r")
setDTthreads(0)

symbol <- "EOS-USD"
securityData <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
securityData[, fold := kfold(securityData, k = 5)]

# Experiment grid search with different aspects energy factors.
hourlyAspects <- prepareHourlyAspectsModelH1()
enOpts <- seq(-1, 1, by = 1)
searchResult <- random_search(
  predictSecurityModelH1A,
  params = list(
    en0 = enOpts, en30 = enOpts, en45 = enOpts, en60 = enOpts, en90 = enOpts,
    en120 = enOpts, en135 = enOpts, en150 = enOpts, en180 = enOpts
  ),
  speedDecay = 0.001,
  security = securityData,
  hourlyAspects = hourlyAspects,
  output = "data.frame",
  n.sample = 10000,
)

print(searchResult)

# Best parameters results:
# en0.test en30.test en45.test en60.test en90.test en120.test en135.test en150.test en180.test    output
#     -1         0         0         0         1          0          0          1          0     0.3969207
#     -1        -1         0         0         1          0         -1          1          0     0.4151665
#     -1         0         0         0         1          1         -1          1          0     0.3942058
#     -1         0         0         0         1          1         -1          1          0     0.3942058