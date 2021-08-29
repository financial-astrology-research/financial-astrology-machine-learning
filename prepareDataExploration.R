# Title     : Prepare daily aspects for exploratory analysis.
# Created by: pablocc
# Created on: 30/09/2020
library(boot)
library(caret)
library(psych)
source("./analysis.r")
source("./indicatorPlots.r")

symbol <- "BTC-USD"
orbLimit <- 0
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
#dailyAspectsRows <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
dailyPlanets <- hourlyPlanets[Hour == 12,]

# dailyAspects <- dailyCombPlanetAspectsFactorsTable(
#   dailyAspects = dailyAspectsRows,
#   pxSelect = pxSelect,
#   pySelect = pySelect,
#   aspectSelect = aspectSelect,
#   orbLimit = orbLimit
# )

securityData <- mainOpenSecurity(
  symbol, 2, 4, "%Y-%m-%d", "2010-01-01"
)

securityData[, prevMid := data.table::shift(Mid, 1)]
aspectView <- merge(
  securityData,
  dailyPlanets, by = "Date"
)

selCols <- c('Date', 'SUMAANG', 'prevMid', 'Mid', 'midPriceDiff', 'PriceAction')
keyAngles <- c(0, 15, 30, 45, 60, 75, 90, 105, 120, 135, 150, 165, 180)
aspectView[, SUMAANG := ceiling(SUMALON)]
aspectView[, midPriceDiff := round(Mid - prevMid, 2)]
aspectView <- aspectView[, ..selCols]
aspectViewResults <- aspectView[SUMAANG %in% keyAngles, ..selCols]


# fwrite(
#   aspectView,
#   paste("./predictions/", symbol, "-planets-comb-aspects-factors-ma3-6-2orb.csv", sep = "")
# )
