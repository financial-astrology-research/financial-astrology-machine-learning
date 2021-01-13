# Title     : Prepare daily aspects for exploratory analysis.
# Created by: pablocc
# Created on: 30/09/2020
library(boot)
library(caret)
library(psych)
source("./analysis.r")
source("./indicatorPlots.r")

symbol <- "BTC-USD"
orbLimit <- 4
pxSelect <- c(
  'MO',
  'ME',
  'VE',
  'SU',
  'MA',
  'JU',
  'SA'
)

pySelect <- c(
  'ME',
  'VE',
  'SU',
  'MA',
  'JU',
  'SA',
  #'NN',
  'UR',
  'NE',
  'PL'
)

aspectSelect <- c(
  0,
  30,
  45,
  60,
  90,
  120,
  135,
  150,
  180
)

idCols <- c('Date', 'Hour')
setClassicAspectsSet8()
setPlanetsMOMEVESUMAJUNNSAURNEPL()
hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
dailyAspectsRows <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)

dailyAspects <- dailyCombPlanetAspectsFactorsTable(
  dailyAspects = dailyAspectsRows,
  pxSelect = pxSelect,
  pySelect = pySelect,
  aspectSelect = aspectSelect,
  orbLimit = orbLimit
)

securityData <- mainOpenSecurity(
  symbol, 2, 4, "%Y-%m-%d",
  "2010-01-01", "2020-06-30"
)

# Filter the extreme outliers.
cat(paste("Original days rows: ", nrow(securityData)), "\n")
securityData <- securityData[zdiffPercent < 3 & zdiffPercent > -3,]
hist(securityData$zdiffPercent)
cat(paste("Total days rows: ", nrow(securityData)), "\n")

aspectView <- merge(
  securityData,
  dailyAspects, by = "Date"
)

fwrite(
  aspectView,
  paste("~/Desktop/", symbol, "-planets-comb-aspects-factors-ma3-6-2orb.csv", sep = "")
)