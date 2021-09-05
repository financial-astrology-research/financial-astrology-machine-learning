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
hourlyPlanets <- openHourlyPlanets('planets_11', clear = F)
#dailyAspectsRows <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
dailyPlanets <- hourlyPlanets[Hour == 12,]

# dailyAspects <- dailyCombPlanetAspectsFactorsTable(
#   dailyAspects = dailyAspectsRows,
#   pxSelect = pxSelect,
#   pySelect = pySelect,
#   aspectSelect = aspectSelect,
#   orbLimit = orbLimit
# )
splitDigits <- function (number) {
  as.numeric(strsplit(as.character(number), "")[[1]])
}

vortexNumber <- function(number) {
  baseNumber <- as.character(number)
  while(str_count(baseNumber) > 1) {
    baseNumber <- sum(splitDigits(baseNumber))
  }

  return(round(baseNumber, 0))
}

securityData <- mainOpenSecurity(
  symbol, 2, 4, "%Y-%m-%d", "2010-01-01"
)

colNames <- colnames(dailyPlanets)
harColNames <- colNames[grep(".*H[0-9]+$", colNames)]
selAstroCols <- names
securityData[, prevMid := data.table::shift(Mid, 1)]
#securityData[, datePlain := format(Date, "%Y%m%d")]
#securityData[, dateNum := vortexNumber(datePlain), by=Date]
#securityData[, dateNum := paste0('n', dateNum)]
securityData[, priceDiff := Mid - prevMid]
securityData[, PriceAction := cut(priceDiff, c(-10000, 0, 10000), labels =  c('bearish', 'bullish'))]
securityData <- securityData[!is.na(priceDiff),]
selAstroCols <- c('Date', harColNames)
selSecurityCols <- c('Date', 'Mid')
aspectView <- merge(
  securityData[, ..selSecurityCols],
  dailyPlanets[, ..selAstroCols]
)

varCorrelations <- aspectView[, -c(1)] %>%
  cor() %>%
  round(digits = 2)
finalCorrelations <- sort(varCorrelations[, 1])

# fwrite(
#   aspectView,
#   paste("./predictions/", symbol, "-planets-comb-aspects-factors-ma3-6-2orb.csv", sep = "")
# )
