source("./indicatorPlots.r")
setDTthreads(0)

symbol <- "EOS-USD"
securityData <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
securityData[, fold := kfold(securityData, k = 5)]

# Experiment grid search with different aspects energy factors.
hourlyAspects <- prepareHourlyAspectsModelH2()
enOpts <- seq(-1, 1, by = 1)
searchResult <- random_search(
  predictSecurityModelH2A,
  params = list(
    en0 = enOpts, en30 = enOpts, en45 = enOpts, en51 = enOpts, en60 = enOpts, en72 = enOpts, en90 = enOpts,
    en103 = enOpts, en120 = enOpts, en135 = enOpts, en144 = enOpts, en150 = enOpts, en180 = enOpts
  ),
  speedDecay = 0.001,
  security = securityData,
  hourlyAspects = hourlyAspects,
  output = "data.frame",
  n.sample = 10000,
)

print(searchResult)

# TODO: Chart predictSecurityModelH3 energy index with this params:
# ~ 0.35 correlation
# PARAMS -  Speed:  0.001 en0: -1 en30: 0 en45: 0 en51: 1 en60: -1 en72: 1 en90: 0
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: -1 en51: 1 en60: 0 en72: 1 en90: 0
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: 0 en51: 1 en60: -1 en72: 0 en90: 0
# PARAMS -  Speed:  0.001 en0: -1 en30: 0 en45: -1 en51: 1 en60: 0 en72: 1 en90: 0
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: -1 en51: 0 en60: 0 en72: -1 en90: 1
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: -1 en51: 1 en60: 0 en72: -1 en90: 1
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: 0 en51: 1 en60: 1 en72: -1 en90: 1
# ~ 0.4 correlation
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: 0 en51: 1 en60: -1 en72: 0 en90: 1
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: -1 en51: 1 en60: -1 en72: 0 en90: 1
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: -1 en51: 1 en60: 0 en72: 0 en90: 1
# PARAMS -    -1   -1   -1    1    0    0    1

# en0.test en30.test en45.test en51.test en60.test en72.test en90.test en103.test en120.test en135.test en144.test en150.test en180.test    output
#    1       -1        -1         0         1         0         0         1         -1         -1         -1         -1          1          0 0.3998239
#    1       -1        -1         1         0         0         0         1         -1         -1          0          0          1          0 0.3922397
