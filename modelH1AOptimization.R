library(GA)
library(parallel)
library(doParallel)
source("./indicatorPlots.r")
setDTthreads(6)

symbol <- "EOS-USD"
securityData <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
securityData[, fold := kfold(securityData, k = 5)]

# Experiment grid search with different aspects energy factors.
hourlyAspects <- prepareHourlyAspectsModelH1()
hourlyAspectsPrice <- merge(hourlyAspects, securityData[, c('Date', 'diffPercent')], by="Date")
enOpts <- seq(-1, 1, by = 1)
#searchResult <- random_search(
# predictSecurityModelH1A,
# params = list(
# en0 = enOpts, en30 = enOpts, en45 = enOpts, en60 = enOpts, en90 = enOpts,
# en120 = enOpts, en135 = enOpts, en150 = enOpts, en180 = enOpts
# ),
# speedDecay = 0.001,
# security = securityData,
# hourlyAspects = hourlyAspects,
# output = "data.frame",
# n.sample = 10000,
#)
#
#print(searchResult)

# Best parameters results:
# en0.test en30.test en45.test en60.test en90.test en120.test en135.test en150.test en180.test output
# -1 0 0 0 1 0 0 1 0 0.3969207
# -1 -1 0 0 1 0 -1 1 0 0.4151665
# -1 0 0 0 1 1 -1 1 0 0.3942058
# -1 0 0 0 1 1 -1 1 0 0.3942058

# Suggest the best solutions found through random search.
solutions <- rbind(
  c(-3, -1, 1, -1, 1, 2, -1, 2, -1),
  c(-1, 0, 0, 0, 1, 0, 0, 1, 0),
  c(-1, -1, 0, 0, 1, 0, -1, 1, 0),
  c(-1, 0, 0, 0, 1, 1, -1, 1, 0),
  c(-1, 0, 0, 0, 1, 1, -1, 1, 0),
  c(-2, -2, 0, -1, 2, -1, -1, 2, 0),
  c(-3, -2, 0, -1, 2, 0, -1, 2, 0)
)

gar <- ga(
  "real-valued",
  fitness = predictSecurityModelH1A,
  lower = c(-3, -3, -3, -3, -3, -3, -3, -3, -3),
  upper = c(3, 3, 3, 3, 3, 3, 3, 3, 3),
  names = c('a0', 'a30', 'a45', 'a60', 'a90', 'a120', 'a135', 'a150', 'a180'),
  suggestions = solutions,
  popSize = 200, elitism = 20, pcrossover = 0.9, pmutation = 0.1,
  maxiter = 100, run = 30,
  selection = gaint_rwSelection, mutation = gaint_raMutation,
  crossover = gaint_spCrossover, population = gaint_Population,
  parallel = F, monitor = gaMonitor, keepBest = T,
  securityData, hourlyAspects
)

summary(gar)
plot(gar)

#,PARAMS,-,en0:,-3,en30:,-2,en45:,0,en60:,-1,en90:,2,en120:,0,en135:,-1,en150:,2,en180:,0
#Fitness function value = 0.4965062
#Solution =
#a0 a30 a45 a60 a90 a120 a135 a150 a180
#[1,] -3  -2   0  -1   3   -1   -1    3    0