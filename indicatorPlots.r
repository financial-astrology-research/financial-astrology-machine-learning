library(grid)
library(paramtest)
library(purrr)
source("./analysis.r")
todayDate <- as.Date(Sys.Date())

analyzeSecurity <- function(security) {
  setClassicAspectsSet()
  setPlanetsMOMEVESUMACEJUNNSAURNEPL()
  span <- 22

  drawSecurityPriceSerie <- function() {
    securityPeriod <- security[Date >= chartPeriod[1] & Date <= chartPeriod[2],]
    p <- ggplot(data = securityPeriod) +
      geom_line(aes(x = Date, y = Mid), colour = "white", alpha = 0.7) +
      geom_vline(xintercept = as.Date(datesHighs), linetype = "dashed", color = "green", size = 0.6, alpha = 0.7) +
      geom_vline(xintercept = as.Date(datesLows), linetype = "dashed", color = "red", size = 0.6, alpha = 0.7) +
      scale_x_date(date_breaks = dateBreaks, date_labels = "%Y-%m-%d", limits = chartPeriod) +
      scale_y_log10() +
      theme_black() +
      theme(panel.spacing = unit(c(0, 0, 0, 0), "null")) +
      theme(plot.margin = unit(c(0, 0, 0, 0), "null")) +
      theme(panel.grid = element_blank()) +
      theme(panel.border = element_blank()) +
      theme(plot.margin = unit(c(0, 0, 0, 0), "null")) +
      theme(panel.spacing = unit(c(0, 0, 0, 0), "null")) +
      theme(axis.ticks = element_blank()) +
      theme(axis.text = element_blank()) +
      theme(axis.title = element_blank()) +
      theme(axis.line = element_blank()) +
      theme(legend.position = "none") +
      theme(axis.ticks.length = unit(0, "null")) +
      theme(axis.ticks.margin = unit(0, "null")) +
      theme(legend.margin = unit(0, "null")) +
      theme(axis.text.x = element_text(angle = 90, size = 10), axis.title.x = element_blank(), axis.title.y = element_blank())
  }

  dailyPlanets = buildPlanetsIndicators()
  dailyPlanets <<- dailyPlanets
  dailyPlanetsResearch <- dailyPlanets[Date > as.Date('2017-01-01') & Date < as.Date('2021-01-01'),]
  chartPeriod <- c(as.Date("2018-01-10"), as.Date("2020-12-31"))
  currentDates <- c(todayDate, todayDate + 1)
  dateBreaks <- "7 days"
  security <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
  dailyPlanetPriceResearch <- merge(dailyPlanets, security, by = c('Date'))

  cat("Relevant peak dates:\n")
  datesHighs <- security$Date[peaks(security$Mid, span)]
  datesLows <- security$Date[peaks(-security$Mid, span)]

  # Indicator / Price charts.
  p1 <- uranusIndicators()
  p2 <- drawSecurityPriceSerie()
  grid.newpage()
  grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))

  cat("1 day daily price diff percentages\n")
  print(summary(abs(Delt(security$Mid, k = 1))))

  cat("3 day daily price diff percentages:\n")
  print(summary(abs(Delt(security$Mid, k = 3))))

  return(dailyPlanetPriceResearch)
}

dailyAspectsAddSpeed <- function(dailyAspects, dailyPlanets, idCols = c('Date')) {
  # Melt speeds.
  dailySpeed <- data.table::melt(
    dailyPlanets, id.var = idCols, variable.name = 'origin',
    value.name = 'sp', measure.var = planetsSpCols
  )

  # Aggregate by date.
  dailySpeed <- dailySpeed[, mean(sp), by = list(Date, origin)]
  setnames(dailySpeed, c('Date', 'origin', 'sp'))
  dailySpeed[, spn := normalize(sp), by = c('origin')]

  dailySpeedX <- copy(dailySpeed)
  dailySpeedX[, p.x := substr(origin, 1, 2)]
  dailySpeedX[, sp.x := round(sp, 3)]
  dailySpeedX[, spn.x := spn]
  dailySpeedY <- copy(dailySpeed)
  dailySpeedY[, p.y := substr(origin, 1, 2)]
  dailySpeedY[, sp.y := round(sp, 3)]
  dailySpeedY[, spn.y := spn]
  selectColsY <- c('Date', 'p.y', 'sp.y', 'spn.y')
  dailyAspects <- merge(dailyAspects, dailySpeedY[, ..selectColsY], by = c('Date', 'p.y'))
  selectColsX <- c('Date', 'p.x', 'sp.x', 'spn.x')
  dailyAspects <- merge(dailyAspects, dailySpeedX[, ..selectColsX], by = c('Date', 'p.x'))
}

dailyAspectsAddDeclination <- function(dailyAspects, dailyPlanets, idCols = c('Date')) {
  # Melt speeds.
  dailyDeclination <- data.table::melt(
    dailyPlanets, id.var = idCols, variable.name = 'origin',
    value.name = 'dc', measure.var = planetsDecCols
  )

  # Aggregate by date.
  dailyDeclination <- dailyDeclination[, mean(dc), by = list(Date, origin)]
  setnames(dailyDeclination, c('Date', 'origin', 'dc'))
  dailyDeclination[, dcn := normalize(dc), by = c('origin')]

  dailyDeclinationX <- copy(dailyDeclination)
  dailyDeclinationX[, p.x := substr(origin, 1, 2)]
  dailyDeclinationX[, dc.x := round(dc, 3)]
  dailyDeclinationX[, dcn.x := dcn]
  dailyDeclinationY <- copy(dailyDeclination)
  dailyDeclinationY[, p.y := substr(origin, 1, 2)]
  dailyDeclinationY[, dc.y := round(dc, 3)]
  dailyDeclinationY[, dcn.y := dcn]
  selectColsY <- c('Date', 'p.y', 'dc.y', 'dcn.y')
  dailyAspects <- merge(dailyAspects, dailyDeclinationY[, ..selectColsY], by = c('Date', 'p.y'))
  selectColsX <- c('Date', 'p.x', 'dc.x', 'dcn.x')
  dailyAspects <- merge(dailyAspects, dailyDeclinationX[, ..selectColsX], by = c('Date', 'p.x'))
}

# Aggregate hourly observations by day leaving the min orb.
hourlyAspectsDateAggregate <- function(hourlyAspects) {
  # Aggregate aspect by day with average orb.
  hourlyAspects <- hourlyAspects[, min(orb), by = list(Date, origin, aspect)]
  setnames(hourlyAspects, c('Date', 'origin', 'aspect', 'orb'))
}

dailyAspectsAddOrbs <- function(dailyAspects, dailyPlanets, idCols = c('Date')) {
  # Melt orbs.
  dailyAspectsOrbs <- data.table::melt(
    dailyPlanets, id.var = idCols, variable.name = 'origin',
    value.name = 'orb', measure.var = planetsCombOrb
  )

  dailyAspectsOrbs[, orb := round(orb, 2)]
  dailyAspectsOrbs[, origin := substr(origin, 1, 4)]
  dailyAspects[, origin := substr(origin, 1, 4)]
  # Join aspects & orbs.
  merge(dailyAspects, dailyAspectsOrbs, by = c(idCols, 'origin'))
}

dailyAspectsAddOrbsDir <- function(dailyAspects) {
  # Calculate orb direction (applicative, separative).
  dailyAspects[, orbdir := round(orb - Lag(orb, k = 1), 4), by = c('origin', 'aspect')]
  dailyAspects[is.na(orbdir), orbdir := 0]
  dailyAspects[, type := cut(orbdir, c(-100, 0, 100), labels = (c('A', 'S')))]
}

dailyAspectsAddLongitude <- function(dailyAspects, dailyPlanets, idCols = c('Date')) {
  # Melt longitudes.
  dailyLongitudes <- data.table::melt(dailyPlanets, id.var = idCols, variable.name = 'origin',
                          value.name = 'lon', measure.var = planetsLonCols)
  # Aggregate by date.
  dailyLongitudes <- dailyLongitudes[, mean(lon), by = list(Date, origin)]
  setnames(dailyLongitudes, c('Date', 'origin', 'lon'))

  # For first planet.
  dailyLongitudesX <- copy(dailyLongitudes)
  dailyLongitudesX[, p.x := substr(origin, 1, 2)]
  dailyLongitudesX[, lon.x := lon]
  # For second planet.
  dailyLongitudesY <- copy(dailyLongitudes)
  dailyLongitudesY[, p.y := substr(origin, 1, 2)]
  dailyLongitudesY[, lon.y := lon]
  # Merge
  dailyAspects[, p.x := substr(origin, 1, 2)]
  dailyAspects[, p.y := substr(origin, 3, 4)]
  selectColsY <- c('Date', 'p.y', 'lon.y')
  dailyAspects <- merge(dailyAspects, dailyLongitudesY[, ..selectColsY], by = c('Date', 'p.y'))
  selectColsX <- c('Date', 'p.x', 'lon.x')
  dailyAspects <- merge(dailyAspects, dailyLongitudesX[, ..selectColsX], by = c('Date', 'p.x'))
}

dailyAspectsAddPlanetsActivation <- function(dailyAspects) {
  aspCols <- paste("a", aspects, sep = "")
  dailyPlanetActivation <- data.table::melt(
    dailyAspects, id.var = c('Date', 'p.x'),
    variable.name = 'origin', measure.var = c('p.y')
  )
  setnames(dailyPlanetActivation, c('Date', 'p.x', 'origin', 'p.y'))

  dailyPlanetActivationCountX <- dailyPlanetActivation[,
    data.table(table(p.x, p.y)), by = list(Date)
  ]

  dailyPlanetActivationCountXWide <- dcast(
    dailyPlanetActivationCountX,
    Date + p.x ~ factor(p.y, levels = planetsBaseCols),
    value.var = "N", fill = 0
  )
  setDT(dailyPlanetActivationCountXWide)

  dailyPlanetActivationCountY <- dailyPlanetActivation[,
    data.table(table(p.x, p.y)), by = list(Date)
  ]

  dailyPlanetActivationCountYWide <- dcast(
    dailyPlanetActivationCountY,
    Date + p.y ~ factor(p.x, levels = planetsBaseCols),
    value.var = "N", fill = 0
  )
  setDT(dailyPlanetActivationCountYWide)

  # Merge X/Y planets activation counts.
  dailyAspects <- merge(dailyAspects, dailyPlanetActivationCountXWide, by = c("Date", "p.x"))
  dailyAspects <- merge(dailyAspects, dailyPlanetActivationCountYWide, by = c("Date", "p.y"))

  # Calculate cumulative planets activation.
  dailyAspects[, ME := ME.x + ME.y]
  dailyAspects[, VE := VE.x + VE.y]
  dailyAspects[, SU := SU.x + SU.y]
  dailyAspects[, MA := MA.x + MA.y]
  dailyAspects[, JU := JU.x + JU.y]
  dailyAspects[, SA := SA.x + SA.y]
  #dailyAspects[, UR := UR.x + UR.y]
  #dailyAspects[, NE := NE.x + NE.y]
  #dailyAspects[, PL := PL.x + PL.y]
}

dailyAspectsAddAspectsCount <- function(dailyAspects) {
  aspCols <- paste("a", aspects, sep = "")
  dailyPlanetAspects <- data.table::melt(
    dailyAspects, id.var = c('Date', 'aspect'),
    variable.name = 'origin', measure.var = c('p.x', 'p.y')
  )
  setnames(dailyPlanetAspects, c('Date', 'aspect', 'origin', 'planet'))

  # Daily total aspects per planet count.
  dailyAspectsPlanetCount <- dailyPlanetAspects[, data.table(table(aspect, planet)), by = list(Date)]
  dailyAspectsPlanetCount[, aspect := paste("a", aspect, sep = "")]
  dailyAspectsPlanetCountWide <- dcast(
    dailyAspectsPlanetCount,
    Date + planet ~ factor(aspect, levels = aspCols),
    value.var = "N", fill = 0
  )
  setDT(dailyAspectsPlanetCountWide)

  # Merge aspects count once per each former planets.
  setnames(dailyAspectsPlanetCountWide, c('Date', 'p.x', paste(aspCols, 'x', sep = ".")))
  dailyAspects <- merge(dailyAspects, dailyAspectsPlanetCountWide, by = c("Date", "p.x"))
  aspColsX <- paste(aspCols, 'x', sep = ".")
  #dailyAspects[, c(aspColsX) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = aspColsX]

  setnames(dailyAspectsPlanetCountWide, c('Date', 'p.y', paste(aspCols, 'y', sep = ".")))
  dailyAspects <- merge(dailyAspects, dailyAspectsPlanetCountWide, by = c("Date", "p.y"))
  aspColsY <- paste(aspCols, 'y', sep = ".")
  #dailyAspects[, c(aspColsY) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = aspColsY]

  # Daily total aspects count.
  dailyAspectsCount <- dailyPlanetAspects[, data.table(table(aspect)), by = list(Date)]
  dailyAspectsCount[, aspect := paste("a", aspect, sep = "")]
  dailyAspectsCountWide <- dcast(
    dailyAspectsCount,
    Date ~ factor(aspect, levels = aspCols),
    value.var = "N", fill = 0
  )
  setDT(dailyAspectsCountWide)

  setnames(dailyAspectsCountWide, c('Date', paste(aspCols, 'g', sep = ".")))
  dailyAspects <- merge(dailyAspects, dailyAspectsCountWide, by = c("Date"))
  aspColsT <- paste(aspCols, 'g', sep = ".")
  #dailyAspects[, c(aspColsT) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = aspColsT]

  # aggregate x/y aspects count by type.
  dailyAspects[, a0 := a0.x + a0.y]
  dailyAspects[, a30 := a30.x + a30.y]
  dailyAspects[, a45 := a45.x + a45.y]
  dailyAspects[, a60 := a60.x + a60.y]
  dailyAspects[, a90 := a90.x + a90.y]
  dailyAspects[, a120 := a120.x + a120.y]
  dailyAspects[, a135 := a135.x + a135.y]
  dailyAspects[, a150 := a150.x + a150.y]
  dailyAspects[, a180 := a180.x + a180.y]

  # aggregate x/y aspects count by type.
  dailyAspects[, a0.d := a0.x - a0.y]
  dailyAspects[, a30.d := a30.x - a30.y]
  dailyAspects[, a45.d := a45.x - a45.y]
  dailyAspects[, a60.d := a60.x - a60.y]
  dailyAspects[, a90.d := a90.x - a90.y]
  dailyAspects[, a120.d := a120.x - a120.y]
  dailyAspects[, a135.d := a135.x - a135.y]
  dailyAspects[, a150.d := a150.x - a150.y]
  dailyAspects[, a180.d := a180.x - a180.y]

  # Total aspect cumulative and daily totals count.
  dailyAspects[, acx := a0.x +
    a30.x +
    a45.x +
    a60.x +
    a90.x +
    a120.x +
    a135.x +
    a150.x +
    a180.x
  ]

  dailyAspects[, acy := a0.y +
    a30.y +
    a45.y +
    a60.y +
    a90.y +
    a120.y +
    a135.y +
    a150.y +
    a180.y
  ]

  dailyAspects[, agt := a0.g +
    a30.g +
    a45.g +
    a60.g +
    a90.g +
    a120.g +
    a135.g +
    a150.g +
    a180.g
  ]
  #dailyAspects[, acp := a30 + a60 + a120]
  #dailyAspects[, acn := a45 + a90 + a135]

  return(dailyAspects)
}

dailyPlanetAspectsCumulativeEnergyTable <- function(dailyAspects) {
  aspCols <- paste("a", aspects, sep = "")
  dailyPlanetAspects <- data.table::melt(
    dailyAspects, id.var = c('Date', 'p.x', 'p.y', 'aspect', 'orb', 'ennow'),
    variable.name = 'origin', measure.var = c('p.x', 'p.y')
  )
  setnames(dailyPlanetAspects, c('Date', 'p.x', 'p.y', 'aspect', 'orb', 'ennow', 'origin', 'planet'))

  # Daily total energy per aspect and planet.
  dailyAspectsPlanetCumEnergy <- dailyPlanetAspects[,
    data.table(aggregate(ennow, list(aspect, planet), sum)), by = list(Date)
  ]
  setnames(dailyAspectsPlanetCumEnergy, c('Date', 'aspect', 'planet', 'ennow'))

  dailyAspectsPlanetCumEnergy[, aspect := paste("a", aspect, sep = "")]
  dailyAspectsPlanetCumEnergyWide <- dcast(
    dailyAspectsPlanetCumEnergy,
    Date + planet ~ factor(aspect, levels = aspCols),
    value.var = "ennow", fill = 0
  )
  setDT(dailyAspectsPlanetCumEnergyWide)

  return(dailyAspectsPlanetCumEnergyWide)
}

dailyAspectsAddAspectsCumulativeEnergy <- function(dailyAspects) {
  aspCols <- paste("a", aspects, sep = "")
  dailyPlanetAspectsCumulativeEnergy <- dailyPlanetAspectsCumulativeEnergyTable(dailyAspects)
  # Merge aspects cumulative energy once per each former planets.
  setnames(dailyPlanetAspectsCumulativeEnergy, c('Date', 'p.x', paste(aspCols, 'x', sep = ".")))
  dailyAspects <- merge(dailyAspects, dailyPlanetAspectsCumulativeEnergy, by = c("Date", "p.x"))
  setnames(dailyPlanetAspectsCumulativeEnergy, c('Date', 'p.y', paste(aspCols, 'y', sep = ".")))
  dailyAspects <- merge(dailyAspects, dailyPlanetAspectsCumulativeEnergy, by = c("Date", "p.y"))

  # sum x/y aspects energy by type.
  dailyAspects[, a0 := a0.x + a0.y]
  dailyAspects[, a30 := a30.x + a30.y]
  dailyAspects[, a45 := a45.x + a45.y]
  dailyAspects[, a60 := a60.x + a60.y]
  dailyAspects[, a90 := a90.x + a90.y]
  dailyAspects[, a120 := a120.x + a120.y]
  dailyAspects[, a150 := a150.x + a150.y]
  dailyAspects[, a180 := a180.x + a180.y]

  # diff x/y aspects energy by type.
  dailyAspects[, a0.d := a0.x - a0.y]
  dailyAspects[, a30.d := a30.x - a30.y]
  dailyAspects[, a45.d := a45.x - a45.y]
  dailyAspects[, a60.d := a60.x - a60.y]
  dailyAspects[, a90.d := a90.x - a90.y]
  dailyAspects[, a120.d := a120.x - a120.y]
  dailyAspects[, a150.d := a150.x - a150.y]
  dailyAspects[, a180.d := a180.x - a180.y]

  return(dailyAspects)
}

dailyAspectsAddEnergy <- function(dailyAspects, speedDecay = 0.6, aspectsEnergyCustom = NULL) {
  if (is.null(aspectsEnergyCustom)) {
    aspectsEnergyCustom <- aspectsEnergy
  }

  aspectsEnergyIndex <- matrix(
    aspectsEnergyCustom, nrow = 1, ncol = length(aspectsEnergyCustom),
    byrow = T, dimnames = list(c('energy'), aspects)
  )

  # Calculate max and proportional energy.
  dailyAspects[, enmax := aspectsEnergyIndex['energy', as.character(aspect)]]
  dailyAspects[, ennow := energyDecay(enmax, orb, speedDecay)]

  return(dailyAspects)
}

dailyAspectsAddEnergy2 <- function(dailyAspects, aspectsEnergyCustom = NULL) {
  if (is.null(aspectsEnergyCustom)) {
    aspectsEnergyCustom <- aspectsEnergy
  }

  aspectsEnergyIndex <- matrix(
    aspectsEnergyCustom, nrow = 1, ncol = length(aspectsEnergyCustom),
    byrow = T, dimnames = list(c('energy'), aspects)
  )

  # Calculate max and proportional energy.
  dailyAspects[, enmax := aspectsEnergyIndex['energy', as.character(aspect)]]
  dailyAspects[, ennow := enmax]

  return(dailyAspects)
}

# Energy proportional to speed.
dailyAspectsAddEnergy3 <- function(dailyAspects, speedDecay = 0.6) {
  aspectsEnergyIndex <- matrix(
    aspectsEnergy, nrow = 1, ncol = length(aspectsEnergy),
    byrow = T, dimnames = list(c('energy'), aspects)
  )

  # Calculate max and proportional energy.
  dailyAspects[, enmax := aspectsEnergyIndex['energy', as.character(aspect)]]
  dailyAspects[, ennow := energyDecay(enmax, orb, speedDecay) *
    (1 - sp.x) *
    (1 - sp.y)]

  return(dailyAspects)
}

dailyAspectsAddEnergyWeightWithDecay <- function(dailyAspects, speedDecay = 0.6, aspectWeight) {
  energyConstant <- 2
  aspectWeightIndex <- matrix(
    aspectWeight, nrow = 1, ncol = length(aspectWeight),
    byrow = T, dimnames = list(c('weight'), aspects)
  )

  # Calculate max and proportional energy.
  dailyAspects[, enmax := energyConstant * aspectWeightIndex['weight', as.character(aspect)]]
  dailyAspects[, ennow := energyDecay(enmax, orb, speedDecay)]

  return(dailyAspects)
}

dailyAspectsAddEnergyWeightWithDecay2 <- function(
  dailyAspects,
  speedDecay = 0.6,
  planetWeight,
  aspectWeight
) {
  energyConstant <- 2
  aspectWeightIndex <- matrix(
    aspectWeight, nrow = 1, ncol = length(aspectWeight),
    byrow = T, dimnames = list(c('weight'), aspects)
  )

  planetWeightIndex <- matrix(
    planetWeight, nrow = 1, ncol = length(planetWeight),
    byrow = T, dimnames = list(c('weight'), planetsBaseCols)
  )

  # Calculate max and proportional energy.
  dailyAspects[, enweight := aspectWeightIndex['weight', as.character(aspect)] * planetWeightIndex['weight', p.y]]
  dailyAspects[, enmax := energyConstant * enweight]
  dailyAspects[, ennow := energyDecay(enmax, orb, speedDecay)]

  return(dailyAspects)
}

dailyAspectsAddEnergyWeightWithDecay3 <- function(
  dailyAspects,
  speedDecay = 0.6,
  planetWeight
) {
  energyConstant <- 2
  planetWeightIndex <- matrix(
    planetWeight, nrow = 1, ncol = length(planetWeight),
    byrow = T, dimnames = list(c('weight'), planetsBaseCols)
  )

  # Calculate max and proportional energy.
  dailyAspects[, enweight := planetWeightIndex['weight', p.y]]
  dailyAspects[, enmax := energyConstant * enweight]
  dailyAspects[, ennow := energyDecay(enmax, orb, speedDecay)]

  return(dailyAspects)
}

dailyAspectsAddEnergyWeightWithDecay4 <- function(
  dailyAspects,
  speedDecay = 0.6,
  planetWeight
) {
  energyConstant <- 2
  planetWeightIndex <- matrix(
    planetWeight, nrow = 1, ncol = length(planetWeight),
    byrow = T, dimnames = list(c('weight'), planetsBaseCols)
  )

  # Calculate max and proportional energy.
  dailyAspects[, enweight := planetWeightIndex['weight', p.x] * planetWeightIndex['weight', p.y]]
  dailyAspects[, enmax := energyConstant * enweight]
  dailyAspects[, ennow := energyDecay(enmax, orb, speedDecay)]

  return(dailyAspects)
}

dailyAspectsAddEnergyWeightWithDecay5 <- function(
  dailyAspects,
  speedDecay = 0.6,
  planetWeight,
  aspectWeight
) {
  energyConstant <- 2
  aspectWeightIndex <- matrix(
    aspectWeight, nrow = 1, ncol = length(aspectWeight),
    byrow = T, dimnames = list(c('weight'), aspects)
  )

  planetWeightIndex <- matrix(
    planetWeight, nrow = 1, ncol = length(planetWeight),
    byrow = T, dimnames = list(c('weight'), planetsBaseCols)
  )

  # Calculate max and proportional energy.
  dailyAspects[,
    enweight :=
      aspectWeightIndex['weight', as.character(aspect)] *
        planetWeightIndex['weight', p.x] *
        planetWeightIndex['weight', p.y]
  ]
  dailyAspects[, enmax := energyConstant * enweight]
  dailyAspects[, ennow := energyDecay(enmax, orb, speedDecay)]

  return(dailyAspects)
}

dailyAspectsAddCumulativeEnergy <- function(dailyAspects) {
  # Merge daily security prices with aspects.
  # dailyAspectsPriceResearch <- merge(dailyAspects, securityTrain[, c('Date', 'diffPercent')], by = c('Date'))

  # Calculate the historical mean aspect effect.
  #aspectsEffect <- dailyAspectsPriceResearch[
  #  orb <= 1,
  #  list(round(mean(diffPercent), 4), round(median(diffPercent), 4)),
  #  by = c('origin', 'aspect', 'type')
  #]
  #setnames(aspectsEffect, c('origin', 'aspect', 'type', 'diffMean', 'diffMedian'))
  #dailyAspects[, diffMean := setAspectEffect(.SD), by=c('Date', 'origin', 'aspect', 'type')]
  # dailyAspectsPriceResearch <- merge(dailyAspectsPriceResearch, aspectsEffect, by = c('origin', 'aspect', 'type'))
  # dailyAspects <- merge(dailyAspects, aspectsEffect, by = c('origin', 'aspect', 'type'))

  # Melt involved planets current energy for body strength calculation.
  dailyAspectsPlanetsEnergy <- data.table::melt(
    dailyAspects, id.var = c('Date', 'ennow'),
    variable.name = 'origin', value.name = 'planet',
    measure.var = c('p.x', 'p.y')
  )

  dailyAspectsCumulativeEnergy <- dailyAspectsPlanetsEnergy[, round(sum(ennow), 2), by = c('Date', 'planet')]
  setkey(dailyAspectsCumulativeEnergy, 'Date')

  # Merge cumulative planets energy.
  setnames(dailyAspectsCumulativeEnergy, c('Date', 'p.x', 'encum.x'))
  dailyAspects <- merge(dailyAspects, dailyAspectsCumulativeEnergy, by = c('Date', 'p.x'))
  setnames(dailyAspectsCumulativeEnergy, c('Date', 'p.y', 'encum.y'))
  dailyAspects <- merge(dailyAspects, dailyAspectsCumulativeEnergy, by = c('Date', 'p.y'))

  return(dailyAspects)
}

# Decay cumulative energy that can result in reversed polarity when cumulative and own energy both are negative
# due the fact that -1 x -1 = 1.
dailyAspectsAddEffectM1 <- function(dailyAspects) {
  dailyAspects[, entot := round((encum.x + encum.y) * ennow, 2)]
  dailyAspects[, effect := entot]

  return(dailyAspects)
}

# Cumulative and own energy polarity converted to the mean past effect.
dailyAspectsAddEffectM2 <- function(dailyAspects) {
  dailyAspects[, entot := round((encum.x + encum.y) * ennow, 2)]
  dailyAspects[, effect := round(sign(diffMean) * entot, 2)]

  return(dailyAspects)
}

# Decay cumulative energy disregard the executor aspect polarity.
dailyAspectsAddEffectM3 <- function(dailyAspects) {
  dailyAspects[, entot := round((encum.x + encum.y) * abs(ennow), 2)]
  dailyAspects[, effect := entot]

  return(dailyAspects)
}

# Combinatory cumulative + own energy.
dailyAspectsAddEffectM4 <- function(dailyAspects) {
  dailyAspects[, entot := round(encum.x + encum.y + ennow, 2)]
  dailyAspects[, effect := entot]

  return(dailyAspects)
}

# Cumulative log10 energy disregard the executor aspect polarity.
dailyAspectsAddEffectM5 <- function(dailyAspects) {
  dailyAspects[, entot := round(log10((encum.x + encum.y) * abs(ennow)), 2)]
  dailyAspects[, effect := entot]

  return(dailyAspects)
}

dailyAspectsEffectIndex <- function(dailyAspects, idCols = c('Date')) {
  # Daily aspects effect index.
  dailyAspectsIndex <- dailyAspects[, round(sum(effect), 2), by = idCols]
  dailyAspectsIndex[, diff := round(Delt(V1, k = 1), 2)]
  setnames(dailyAspectsIndex, c('Date', 'effect', 'diff'))
  dailyAspectsIndex[, effectMA := SMA(effect, 3)]

  return(dailyAspectsIndex)
}

crossValidateModelReport <- function(modelId, dailyAspectsIndex, security) {
  cat("VALIDATING: ", modelId, "\n")
  predictSecurityModelReport(dailyAspectsIndex, security[fold == 1,])
  predictSecurityModelReport(dailyAspectsIndex, security[fold == 2,])
  predictSecurityModelReport(dailyAspectsIndex, security[fold == 3,])
  #predictSecurityModelReport(dailyAspectsIndex, security[fold == 4,])
  #predictSecurityModelReport(dailyAspectsIndex, security[fold == 5,])
}

crossValidateModelOptimization <- function(modelId, dailyAspectsIndex, security) {
  cat("OPTIMIZING: ", modelId, "\n")
  modelFit1 <- predictSecurityFit(dailyAspectsIndex, security[fold == 1,])
  modelFit2 <- predictSecurityFit(dailyAspectsIndex, security[fold == 2,])
  modelFit3 <- predictSecurityFit(dailyAspectsIndex, security[fold == 3,])
  medianFit <- median(c(modelFit1, modelFit2, modelFit3))
  cat("MEDIAN FIT: ", medianFit, "\n")
  cat("\n")

  return(medianFit)
}

predictSecurityFit <- function(dailyAspectsIndex, securityTest) {
  modelTest <- merge(dailyAspectsIndex, securityTest[, c('Date', 'Mid', 'diffPercent')], by = c('Date'))
  modelFit <- cor(modelTest$effect, modelTest$Mid, method = "pearson")
  cat("CORRELATION EFFECT / PRICE: ", modelFit, "\n")

  return(modelFit)
}

predictSecurityModelReport <- function(dailyAspectsIndex, securityTest) {
  dailyAspectsIndexProjected <- dailyAspectsIndex[Date > todayDate - 60,][0:120]
  modelTest <- merge(dailyAspectsIndex, securityTest[, c('Date', 'Mid', 'diffPercent')], by = c('Date'))

  #cat("Today aspects:", format(todayDate, "%Y-%m-%d"), "\n")
  #print(dailyAspects[Date == todayDate,][order(-entot)], topn = 200)
  #cat("\n")
  #
  #cat("Tomorrow aspects:", format(todayDate + 1, "%Y-%m-%d"), "\n")
  #print(dailyAspects[Date == todayDate + 1,][order(-entot)], topn = 200)
  #cat("\n")
  #
  #cat("Past tomorrow aspects:", format(todayDate + 2, "%Y-%m-%d"), "\n")
  #print(dailyAspects[Date == todayDate + 2,][order(-entot)], topn = 200)
  #cat("\n")
  #
  #cat("Daily aspects effect index:\n")
  #print(dailyAspectsIndexProjected, topn = 100)
  #cat("\n")

  # cat("Daily test period:\n")
  # print(modelTest)

  p1 <- ggplot(data = modelTest) +
    geom_line(aes(x = Date, y = Mid), colour = "white", alpha = 0.8) +
    theme_black()

  p2 <- ggplot(data = modelTest) +
    geom_line(aes(x = Date, y = effect), colour = "white", alpha = 0.8) +
    geom_line(aes(x = Date, y = effectMA), colour = "yellow", alpha = 0.8) +
    # Do the effect index log conversion per model due to some energy models has negative values.
    # scale_y_log10() +
    theme_black()

  p3 <- ggplot(data = modelTest) +
    geom_point(aes(x = abs(effect), y = abs(diffPercent)), colour = "white", alpha = 0.8) +
    theme_black()

  p4 <- ggplot(data = modelTest) +
    geom_point(aes(x = diff, y = diffPercent), colour = "white", alpha = 0.8) +
    theme_black()

  p5 <- ggplot(data = dailyAspectsIndexProjected) +
    geom_line(aes(x = Date, y = effect), colour = "white", alpha = 0.8) +
    geom_line(aes(x = Date, y = effectMA), colour = "yellow", alpha = 0.8) +
    scale_x_date(date_breaks = '7 days', date_labels = "%Y-%m-%d") +
    theme_black()

  pgridTop <- plot_grid(p1, p3, p2, p4)
  pgrid <- plot_grid(pgridTop, p5, ncol = 1, rel_heights = c(1.7, 1))
  print(pgrid)
  cat("\nCORRELATION EFFECT / MOVE RANGE: ", cor(abs(modelTest$effect), abs(modelTest$diffPercent), method = "pearson"), "\n")
  cat("\nCORRELATION EFFECT / PRICE: ", cor(modelTest$effect, modelTest$Mid, method = "pearson"), "\n")
  cat("\nCORRELATION EFFECT MA / PRICE: ", cor(modelTest$effectMA, modelTest$Mid, method = "pearson"), "\n")
}

dailyAspectsNameCols <- function(dailyAspects) {
  # Set more convenient order for analysis.
  colsOrder <- c('Date', 'origin', 'p.x', 'lon.x', 'sp.x', 'spn.x', 'p.y', 'lon.y', 'sp.y', 'spn.y', 'aspect', 'type', 'orb', 'orbdir', 'enmax', 'ennow', 'encum.x', 'encum.y', 'entot', 'effect')
  setcolorder(dailyAspects, colsOrder)

  return(dailyAspects)
}

dailyHourlyAspectsTablePrepare <- function(hourlyPlanets, idCols, orbLimit = NULL) {
  hourlyPlanetsRange <- hourlyPlanets[Date >= as.Date("2018-01-01") & Date <= as.Date("2022-12-31")]
  # Melt aspects.
  hourlyAspects <- data.table::melt(
    hourlyPlanetsRange, id.var = idCols,
    variable.name = 'origin', value.name = 'aspect',
    value.factor = T, measure.var = planetsCombAsp, na.rm = T
  )

  # hourlyAspects[, origin := substr(origin, 1, 4)]
  #setkey(hourlyAspects, 'Date', 'Hour')

  hourlyAspects <- dailyAspectsAddOrbs(hourlyAspects, hourlyPlanetsRange, idCols)

  if (!is.null(orbLimit)) {
    hourlyAspects <- hourlyAspects[orb <= orbLimit,]
  }

  dailyAspects <- hourlyAspectsDateAggregate(hourlyAspects)
  dailyAspects <- dailyAspectsAddOrbsDir(dailyAspects)
  dailyAspects <- dailyAspectsAddLongitude(dailyAspects, hourlyPlanetsRange, idCols)
  dailyAspects <- dailyAspectsAddSpeed(dailyAspects, hourlyPlanetsRange, idCols)
  dailyAspects <- dailyAspectsAddDeclination(dailyAspects, hourlyPlanetsRange, idCols)

  return(dailyAspects)
}

dailyAspectsTablePrepare <- function(dailyPlanets) {
  dailyPlanetsRange <- dailyPlanets[Date >= as.Date("2015-01-01") & Date <= as.Date("2023-01-01")]
  # Melt aspects.
  dailyAspects <- data.table::melt(dailyPlanetsRange, id.var = c('Date'), variable.name = 'origin',
                       value.name = 'aspect', value.factor = T, measure.var = planetsCombAsp, na.rm = T)
  dailyAspects[, origin := substr(origin, 1, 4)]

  dailyAspects <- dailyAspectsAddOrbs(dailyAspects, dailyPlanets)
  dailyAspects <- dailyAspectsAddLongitude(dailyAspects, dailyPlanets)
  dailyAspects <- dailyAspectsAddSpeed(dailyAspects, dailyPlanets)

  return(dailyAspects)
}

hourlyAspectsEffectIndex <- function(hourlyAspects) {
  # Daily aspects effect index.
  hourlyAspectsIndex <- hourlyAspects[, round(sum(effect), 2), by = c('Date', 'Hour')]
  hourlyAspectsIndex[, diff := round(Delt(V1, k = 1), 2)]
  setnames(hourlyAspectsIndex, c('Date', 'Hour', 'effect', 'diff'))
  hourlyAspectsIndex[, effectMA := SMA(effect, 3)]

  return(hourlyAspectsIndex)
}

# This model uses:
# - Daily aspects & prices.
# - Classical aspects set.
# - Includes CE but not NN or SN cycles.
# - Use common daily aspects true energy disregard the historical security effect.
# - Increase strength of 90 aspects energy by 2x.
predictSecurityModelA <- function(security) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet()
  setPlanetsMEVESUMACEJUSAURNEPL()
  dailyPlanets <<- openPlanets('planets_10', clear = F)
  dailyAspects <- dailyAspectsTablePrepare(dailyPlanets)
  dailyAspects <- dailyAspectsAddEnergy(dailyAspects, 0.5)
  dailyAspects <- dailyAspectsAddCumulativeEnergy(dailyAspects)
  dailyAspects <- dailyAspectsAddEffectM1(dailyAspects)
  dailyAspects <- dailyAspectsNameCols(dailyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(dailyAspects)

  crossValidateModelReport("modelA", dailyAspectsIndex, security)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set.
# - Don't include MO, CE and NN.
# - Use common daily aspects true energy disregard the historical security effect.
# - Increase strength of 90 aspects energy by 2x.
predictSecurityModelB <- function(security) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet()
  setPlanetsMEVESUMAJUSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)

  idCols <- c('Date', 'Hour')
  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.5)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects)
  hourlyAspects <- dailyAspectsAddEffectM1(hourlyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)

  crossValidateModelReport("modelB", dailyAspectsIndex, security)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set.
# - Don't include CE, and include all the planets and MO.
# - Use common daily aspects true energy disregard the historical security effect.
# - Increase strength of 90 aspects energy by 2x.
predictSecurityModelC <- function(security) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet()
  #setPlanetsMOMEVESUMAJUSAURNEPL()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)

  idCols <- c('Date', 'Hour')
  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.5)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects)
  hourlyAspects <- dailyAspectsAddEffectM1(hourlyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)

  crossValidateModelReport("modelC", dailyAspectsIndex, security)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set but limiting energy to a set: 45, 90, 120 and 150.
# - Increase strength of 90 and 150 aspects energy by 2x.
# - Don't include CE, and include all the planets and MO.
# - Use common daily aspects true energy disregard the historical security effect.
predictSecurityModelD <- function(security) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet2()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)

  idCols <- c('Date', 'Hour')
  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.5)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects)
  hourlyAspects <- dailyAspectsAddEffectM1(hourlyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)

  crossValidateModelReport("modelD", dailyAspectsIndex, security)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set with energy polarity.
# - Boost strength of 90 and 150 aspects energy over others.
# - Don't include CE, and include all the planets and MO.
# - Use common daily aspects true energy disregard the historical security effect.
predictSecurityModelE <- function(security) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet3()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)

  idCols <- c('Date', 'Hour')
  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.5)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects)
  hourlyAspects <- dailyAspectsAddEffectM1(hourlyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)

  crossValidateModelReport("modelE", dailyAspectsIndex, security)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set with energy polarity.
# - Boost strength of 90 and 150 aspects energy over others.
# - Effect is the proportion to the orb from the received aspect energy accounting with the original polarity.
# - Don't include CE, and include all the planets and MO.
# - Use common daily aspects true energy disregard the historical security effect.
predictSecurityModelF <- function(security) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet3()
  setPlanetsMOMEVESUMAJUSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)

  idCols <- c('Date', 'Hour')
  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.5)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects)
  hourlyAspects <- dailyAspectsAddEffectM3(hourlyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)

  crossValidateModelReport("modelF", dailyAspectsIndex, security)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set with energy polarity.
# - Boost strength of 90 and 150 aspects energy over others.
# - Effect is the sum of the cumulative and own energy.
# - Don't include CE, and include all the planets and MO.
# - Use common daily aspects true energy disregard the historical security effect.
predictSecurityModelG <- function(security) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet3()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)

  idCols <- c('Date', 'Hour')
  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.5)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects)
  hourlyAspects <- dailyAspectsAddEffectM4(hourlyAspects)
  # hourlyAspectsIndex <- hourlyAspectsEffectIndex(hourlyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)

  crossValidateModelReport("modelG", dailyAspectsIndex, security)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set with energy polarity.
# - Boost strength of 90 and 150 aspects energy over others.
# - Effect is the proportion to the orb from the received aspects cumulative energy accounting with the original polarity.
# - Don't include CE, and include all the planets and MO.
# - Orb decay energy speed is slower to 0.2.
# - MO energy only accounts as cumulative for other aspects.
# - Use common daily aspects true energy disregard the historical security effect.
predictSecurityModelH <- function(security) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet3()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)

  idCols <- c('Date', 'Hour')
  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.2)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects)
  # MO only contribute to the cumulative effect but is not a major indicator.
  hourlyAspects <- hourlyAspects[p.x != 'MO',]
  hourlyAspects <- dailyAspectsAddEffectM3(hourlyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)

  crossValidateModelReport("modelH", dailyAspectsIndex, security)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set with energy polarity.
# - Increase strength of 90 aspects energy by 2x.
# - Effect is the proportion to the orb from the received aspects cumulative energy accounting with the original polarity.
# - Don't include CE, and include all the planets and MO.
# - Orb decay energy speed is slower to 0.2.
# - MO energy only accounts as cumulative for other aspects.
# - Ignore slow to slow planets aspects energy.
# - Use common daily aspects true energy disregard the historical security effect.
# - Changed orbs for all classic aspects to 5 degrees.
predictSecurityModelI <- function(security) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet4()
  setPlanetsMOMEVESUMAJUSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)

  idCols <- c('Date', 'Hour')
  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.2)
  # Ignore slow to slow planets aspects due we lack a complete cycle.
  hourlyAspects <- hourlyAspects[p.x %ni% c('JU', 'SA', 'UR', 'NE', 'PL'),]
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects)
  # MO only contribute to the cumulative effect but is not a major indicator.
  hourlyAspects <- hourlyAspects[p.x != 'MO',]
  hourlyAspects <- dailyAspectsAddEffectM3(hourlyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)

  crossValidateModelReport("modelI", dailyAspectsIndex, security)
}

# Combines characteristics of Model E & H.
predictSecurityModelJ <- function(security) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet3()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)

  idCols <- c('Date', 'Hour')
  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.45)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects)
  # MO only contribute to the cumulative effect but is not a major indicator.
  hourlyAspects <- hourlyAspects[p.x != 'MO',]
  hourlyAspects <- dailyAspectsAddEffectM1(hourlyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)

  crossValidateModelReport("modelJ", dailyAspectsIndex, security)
}

# Based on ModelH with few variations:
# - Decrease speed from 0.2 to 0.1
# - Customize aspect set: increase orbs to the point of max correlation.
# - Enable 0 aspect energy.
predictSecurityModelH1 <- function(security) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet5()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)

  idCols <- c('Date', 'Hour')
  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.1)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects)
  # MO only contribute to the cumulative effect but is not a major indicator.
  hourlyAspects <- hourlyAspects[p.x != 'MO',]
  hourlyAspects <- dailyAspectsAddEffectM3(hourlyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)

  crossValidateModelReport("modelH1", dailyAspectsIndex, security)
}

# Based on ModelH with few variations:
# - Decrease speed from 0.2 to 0.1
# - Customize aspect set: use all aspects set, except: minor 36, 40, 80, 108, 154 and 160.
# - Enable 0 aspect energy.
predictSecurityModelH2 <- function(security) {
  # Best effect correlation when using classic aspects only.
  setModernAspectsSet2()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)

  idCols <- c('Date', 'Hour')
  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.1)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects)
  # MO only contribute to the cumulative effect but is not a major indicator.
  hourlyAspects <- hourlyAspects[p.x != 'MO',]
  hourlyAspects <- dailyAspectsAddEffectM3(hourlyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)

  crossValidateModelReport("modelH2", dailyAspectsIndex, security)
}

# Based on ModelH with few variations:
# - Decrease speed from 0.2 to 0.1
# - Customize aspect set: use all aspects set, including the minors second scale aspects.
# - Enable 0 aspect energy.
# - Is evident that when using second scale aspects we lost few points of effect/price correlation.
predictSecurityModelH3 <- function(security) {
  # Best effect correlation when using classic aspects only.
  setModernAspectsSet3()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)

  idCols <- c('Date', 'Hour')
  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.1)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects)
  # MO only contribute to the cumulative effect but is not a major indicator.
  hourlyAspects <- hourlyAspects[p.x != 'MO',]
  hourlyAspects <- dailyAspectsAddEffectM3(hourlyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)

  crossValidateModelReport("modelH3", dailyAspectsIndex, security)
}

prepareHourlyAspectsModelH2 <- function() {
  idCols <- c('Date', 'Hour')
  setModernAspectsSet2()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)

  return(dailyAspects)
}

# Based on ModelH2 for grid search optimization.
predictSecurityModelH2A <- function(
  iter, security, hourlyAspects, speedDecay, en0, en30, en45, en51,
  en60, en72, en90, en103, en120, en135, en144, en150, en180
) {
  cat(
    "PARAMS - ",
    "en0:", en0, "en30:", en30, "en45:", en45, "en51:", en51, "en60:", en60, "en72:", en72, "en90:", en90,
    "en103:", en103, "en120:", en120, "en135:", en135, "en144:", en144, "en150:", en150, "en180:", en180,
    "\n"
  )

  setModernAspectsSet2()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  aspectsEnergyCustom <- c(en0, en30, en45, en51, en60, en72, en90, en103, en120, en135, en144, en150, en180)

  idCols <- c('Date', 'Hour')
  # Create an iteration table so parallel run has it's own DT.
  hourlyAspectsIteration <- copy(hourlyAspects)
  hourlyAspectsIteration <- dailyAspectsAddEnergy2(hourlyAspectsIteration, speedDecay, aspectsEnergyCustom)
  hourlyAspectsIteration <- dailyAspectsAddCumulativeEnergy(hourlyAspectsIteration)
  # MO only contribute to the cumulative effect but is not a major indicator.
  hourlyAspectsIteration <- hourlyAspectsIteration[p.x != 'MO',]
  hourlyAspectsIteration <- dailyAspectsAddEffectM3(hourlyAspectsIteration)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspectsIteration)
  medianFit <- crossValidateModelOptimization("modelH2A", dailyAspectsIndex, security)

  return(medianFit)
}

prepareHourlyAspectsModelH1 <- function() {
  idCols <- c('Date', 'Hour')
  setClassicAspectsSet5()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols)
  dailyAspects <- dailyAspectsAddAspectsCount(dailyAspects)

  return(dailyAspects)
}

# Aggregated hourly planets that filter partil aspects.
prepareHourlyAspectsModelK <- function() {
  idCols <- c('Date', 'Hour')
  setClassicAspectsSet5()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols)

  # Speed x/y diff, product and ratio.
  dailyAspects <- dailyAspects[, spd := sp.x - sp.y]
  dailyAspects <- dailyAspects[, spp := sp.x * sp.y]
  dailyAspects <- dailyAspects[, spr := (sp.x + 1) / (sp.y + 1)]
  # Declination x/y diff, product and ratio.
  dailyAspects <- dailyAspects[, dcd := dc.x - dc.y]
  dailyAspects <- dailyAspects[, dcp := dc.x * dc.y]
  dailyAspects <- dailyAspects[, dcr := (dc.x + 1) / (dc.y + 1)]

  # Filter aspects within 2 degrees of orb for cumulative aspects count.
  dailyAspects <- dailyAspects[orb <= 1,]
  dailyAspects <- dailyAspectsAddAspectsCount(dailyAspects)
  dailyAspects <- dailyAspectsAddPlanetsActivation(dailyAspects)
  # Add week day.
  dailyAspects[, wd := as.numeric(format(Date, "%w")) + 1]
  # Add zodiacal signs.
  dailyAspects[, zx := ceiling((lon.x + 0.01) / 30)]
  dailyAspects[, zy := ceiling((lon.y + 0.01) / 30)]

  # Remove other redundant cols.
  filterCols <- c('orbdir')
  dailyAspects <- dailyAspects[, -..filterCols]

  # Only leave partil aspects for the observations.
  dailyAspects <- dailyAspects[orb <= 0.2,]

  return(dailyAspects)
}

# Based on ModelH1 for grid search optimization.
predictSecurityModelH1A <- function(params, security, hourlyAspects) {
  cat(
    "PARAMS - ",
    "en0:", params[1], "en30:", params[2], "en45:", params[3], "en60:", params[4], "en90:", params[5],
    "en120:", params[6], "en135:", params[7], "en150:", params[8], "en180:", params[9],
    "\n"
  )

  setClassicAspectsSet5()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()

  idCols <- c('Date', 'Hour')
  # Create an iteration table so parallel run has it's own DT.
  hourlyAspectsIteration <- copy(hourlyAspects)
  hourlyAspectsIteration <- dailyAspectsAddEnergy2(hourlyAspectsIteration, speedDecay, params)
  hourlyAspectsIteration <- dailyAspectsAddCumulativeEnergy(hourlyAspectsIteration)
  # MO only contribute to the cumulative effect but is not a major indicator.
  hourlyAspectsIteration <- hourlyAspectsIteration[p.x != 'MO',]
  hourlyAspectsIteration <- dailyAspectsAddEffectM3(hourlyAspectsIteration)
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspectsIteration)
  medianFit <- crossValidateModelOptimization("modelH2A", dailyAspectsIndex, security)
  crossValidateModelReport("modelH1A", dailyAspectsIndex, security)

  return(medianFit)
}

# Aggregated hourly planets that filter partil aspects
# and keep larger orbs cumulative effects.
prepareHourlyAspectsModelL <- function() {
  idCols <- c('Date', 'Hour')
  setClassicAspectsSet5()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols)

  # Inverse speed.
  dailyAspects[, spi.x := 1 - sp.x]
  dailyAspects[, spi.y := 1 - sp.y]
  dailyAspects[, retx := ifelse(sp.x <= 0.10, 1, 0)]
  dailyAspects[, rety := ifelse(sp.y <= 0.10, 1, 0)]
  dailyAspects[, apl := ifelse(type == "A", 1, 0)]
  dailyAspects[, sep := ifelse(type == "S", 1, 0)]

  # Speed x/y diff, product and ratio.
  dailyAspects[, spd := sp.x - sp.y]
  dailyAspects[, spdi := sp.y - sp.x]
  dailyAspects[, spp := sp.x * sp.y]
  dailyAspects[, spr := (sp.x + 1) / (sp.y + 1)]
  dailyAspects[, spri := (sp.y + 1) / (sp.x + 1)]
  # Declination x/y diff, product and ratio.
  dailyAspects[, dcd := dc.x - dc.y]
  dailyAspects[, dcdi := dc.y - dc.x]
  dailyAspects[, dcp := dc.x * dc.y]
  dailyAspects[, dcr := (dc.x + 1) / (dc.y + 1)]
  dailyAspects[, dcri := (dc.y + 1) / (dc.x + 1)]

  # Filter aspects within 2 degrees of orb for cumulative aspects count.
  dailyAspects <- dailyAspects[orb <= 2,]
  dailyAspects <- dailyAspectsAddAspectsCount(dailyAspects)
  dailyAspects <- dailyAspectsAddPlanetsActivation(dailyAspects)

  # Add week day.
  dailyAspects[, wd := as.numeric(format(Date, "%w")) + 1]
  # Add zodiacal signs.
  dailyAspects[, zx := ceiling((lon.x + 0.01) / 30)]
  dailyAspects[, zy := ceiling((lon.y + 0.01) / 30)]

  # Remove other redundant cols.
  filterCols <- c('orbdir')
  dailyAspects <- dailyAspects[, -..filterCols]

  # Only leave partil aspects for the observations.
  dailyAspects <- dailyAspects[orb <= 0.1,]

  return(dailyAspects)
}

prepareHourlyAspectsModelLA <- function() {
  idCols <- c('Date', 'Hour')
  setClassicAspectsSet5()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols)

  # Inverse speed.
  dailyAspects[, spi.x := 1 - sp.x]
  dailyAspects[, spi.y := 1 - sp.y]
  dailyAspects[, retx := ifelse(sp.x <= 0.10, 1, 0)]
  dailyAspects[, rety := ifelse(sp.y <= 0.10, 1, 0)]
  dailyAspects[, apl := ifelse(type == "A", 1, 0)]
  dailyAspects[, sep := ifelse(type == "S", 1, 0)]

  # Speed x/y diff, product and ratio.
  dailyAspects[, spd := sp.x - sp.y]
  dailyAspects[, spdi := sp.y - sp.x]
  dailyAspects[, spp := sp.x * sp.y]
  dailyAspects[, spr := (sp.x + 1) / (sp.y + 1)]
  dailyAspects[, spri := (sp.y + 1) / (sp.x + 1)]
  # Declination x/y diff, product and ratio.
  dailyAspects[, dcd := dc.x - dc.y]
  dailyAspects[, dcdi := dc.y - dc.x]
  dailyAspects[, dcp := dc.x * dc.y]
  dailyAspects[, dcr := (dc.x + 1) / (dc.y + 1)]
  dailyAspects[, dcri := (dc.y + 1) / (dc.x + 1)]

  dailyAspects$ast0 <- 0
  dailyAspects$ast30 <- 0
  dailyAspects$ast45 <- 0
  dailyAspects$ast60 <- 0
  dailyAspects$ast90 <- 0
  dailyAspects$ast120 <- 0
  dailyAspects$ast135 <- 0
  dailyAspects$ast150 <- 0
  dailyAspects$ast180 <- 0

  dailyAspects[aspect == 0, ast0 := 1]
  dailyAspects[aspect == 30, ast30 := 1]
  dailyAspects[aspect == 45, ast45 := 1]
  dailyAspects[aspect == 60, ast60 := 1]
  dailyAspects[aspect == 90, ast90 := 1]
  dailyAspects[aspect == 120, ast120 := 1]
  dailyAspects[aspect == 135, ast135 := 1]
  dailyAspects[aspect == 150, ast150 := 1]
  dailyAspects[aspect == 180, ast180 := 1]

  # Filter aspects within 2 degrees of orb for cumulative aspects count.
  dailyAspects <- dailyAspects[orb <= 2,]
  dailyAspects <- dailyAspectsAddAspectsCount(dailyAspects)
  dailyAspects <- dailyAspectsAddPlanetsActivation(dailyAspects)

  # Aggregated counts.
  dailyAspects[, asc1 := a45 + a90]
  dailyAspects[, asc2 := a30 + a90]
  dailyAspects[, asc3 := a45 + a90 + a135]
  dailyAspects[, asc4 := a30 + a60 + a120]
  dailyAspects[, asc5 := a30 + a60]
  dailyAspects[, asc6 := a45.d + a90.d]
  dailyAspects[, asc7 := a60.d + a120.d]
  dailyAspects[, asc8 := a45.d + a135.d]
  dailyAspects[, asc9 := a30.d + a60.d + a120.d]
  dailyAspects[, asc10 := a30.d + a90.d]

  # Add week day.
  dailyAspects[, wd := as.numeric(format(Date, "%w")) + 1]
  # Add zodiacal signs.
  dailyAspects[, zx := ceiling((lon.x + 0.01) / 30)]
  dailyAspects[, zy := ceiling((lon.y + 0.01) / 30)]

  # Remove other redundant cols.
  filterCols <- c('orbdir')
  dailyAspects <- dailyAspects[, -..filterCols]

  # Only leave partil aspects for the observations.
  dailyAspects <- dailyAspects[orb <= 0.1,]

  return(dailyAspects)
}

prepareHourlyAspectsModelLB <- function() {
  idCols <- c('Date', 'Hour')
  setClassicAspectsSet7()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols)
  dailyAspects <- dailyAspectsAddEnergy(dailyAspects, 0.6)

  # Inverse speed.
  dailyAspects[, spi.x := 1 - sp.x]
  dailyAspects[, spi.y := 1 - sp.y]
  dailyAspects[, retx := ifelse(sp.x <= 0.10, 1, 0)]
  dailyAspects[, rety := ifelse(sp.y <= 0.10, 1, 0)]
  dailyAspects[, apl := ifelse(type == "A", 1, 0)]
  dailyAspects[, sep := ifelse(type == "S", 1, 0)]

  # Speed x/y diff, product and ratio.
  dailyAspects[, spd := sp.x - sp.y]
  dailyAspects[, spdi := sp.y - sp.x]
  dailyAspects[, spp := sp.x * sp.y]
  dailyAspects[, spr := (sp.x + 1) / (sp.y + 1)]
  dailyAspects[, spri := (sp.y + 1) / (sp.x + 1)]
  # Declination x/y diff, product and ratio.
  dailyAspects[, dcd := dc.x - dc.y]
  dailyAspects[, dcdi := dc.y - dc.x]
  dailyAspects[, dcp := dc.x * dc.y]
  dailyAspects[, dcr := (dc.x + 1) / (dc.y + 1)]
  dailyAspects[, dcri := (dc.y + 1) / (dc.x + 1)]

  dailyAspects$ast0 <- 0
  dailyAspects$ast30 <- 0
  dailyAspects$ast45 <- 0
  dailyAspects$ast60 <- 0
  dailyAspects$ast90 <- 0
  dailyAspects$ast120 <- 0
  dailyAspects$ast150 <- 0
  dailyAspects$ast180 <- 0

  dailyAspects[aspect == 0, ast0 := 1]
  dailyAspects[aspect == 30, ast30 := 1]
  dailyAspects[aspect == 45, ast45 := 1]
  dailyAspects[aspect == 60, ast60 := 1]
  dailyAspects[aspect == 90, ast90 := 1]
  dailyAspects[aspect == 120, ast120 := 1]
  dailyAspects[aspect == 150, ast150 := 1]
  dailyAspects[aspect == 180, ast180 := 1]

  # Filter aspects within 2 degrees of orb for cumulative aspects count.
  dailyAspects <- dailyAspects[orb <= 4,]
  dailyAspects <- dailyAspectsAddAspectsCumulativeEnergy(dailyAspects)

  # Remove other redundant cols.
  filterCols <- c('orbdir')
  dailyAspects <- dailyAspects[, -..filterCols]

  # Only leave partil aspects for the observations.
  dailyAspects <- dailyAspects[orb <= 0.1,]

  return(dailyAspects)
}

prepareHourlyAspectsModelLC <- function() {
  idCols <- c('Date', 'Hour')
  setClassicAspectsSet6()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols)
  # Filter aspects within 2 degrees of orb for cumulative aspects count.
  dailyAspects <- dailyAspects[p.x %ni% c('MO', 'ME') & orb <= 4,]
  dailyAspects <- dailyAspectsAddEnergy(dailyAspects, 0.59)
  dailyPlanetAspectsCumulativeEnergy <- dailyPlanetAspectsCumulativeEnergyTable(dailyAspects)

  aspCols <- paste("a", aspects, sep = "")
  dailyAspectsPlanetCumulativeEnergyWide <- data.table::dcast(
    dailyPlanetAspectsCumulativeEnergy,
    Date ~ planet,
    value.var = aspCols, fill = 0
  )
  setDT(dailyAspectsPlanetCumulativeEnergyWide)

  return(dailyAspectsPlanetCumulativeEnergyWide)
}

dailyCombPlanetAspectsFactorsTable <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = c(),
  pySelect = c(),
  aspectFilter = c(),
  aspectSelect = c()
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setClassicAspectsSet8()
    setPlanetsMOMEVESUMAJUNNSAURNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  # Filter minor MO aspects that can overlap multiples to same target planet
  # in the same day and should not be significant in effect.
  dailyAspects$filter <- F
  dailyAspects <- dailyAspects[p.x == "MO" & aspect == 30, filter := T]
  dailyAspects <- dailyAspects[p.x == "MO" & aspect == 45, filter := T]
  dailyAspects <- dailyAspects[p.x == "MO" & aspect == 135, filter := T]
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects <- dailyAspects[aspect %ni% aspectSelect, filter := T]
  dailyAspects <- dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := paste("a", aspect, sep = "")]

  # Arrange aspects factors as table wide format.
  dailyAspectsWide <- dcast(
    dailyAspects,
    Date ~ origin,
    value.var = "aspect",
    fill = "none"
  )
  setDT(dailyAspectsWide)

  aspectsCols <- names(dailyAspectsWide)[-1]
  dailyAspectsWide[, c(aspectsCols) := lapply(.SD, as.factor), .SDcols = aspectsCols]

  return(dailyAspectsWide)
}

dailyCombPlanetAspectsFactorsTableLI <- function(orbLimit = 2, aspectFilter = c(), pxSelect = c(), pySelect = c()) {
  idCols <- c('Date', 'Hour')
  setClassicAspectsSet8()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)

  # Filter minor MO aspects that can overlap multiples to same target planet
  # in the same day and should not be significant in effect.
  dailyAspects$filter <- F
  dailyAspects[p.x == "MO" & aspect == 30, filter := T]
  dailyAspects[p.x == "MO" & aspect == 45, filter := T]
  dailyAspects[p.x == "MO" & aspect == 135, filter := T]
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]

  # Arrange aspects factors as table wide format.
  dailyAspectsWide <- dcast(
    dailyAspects,
    Date ~ origin,
    value.var = "aspect",
    fill = ""
  )
  setDT(dailyAspectsWide)

  aspectsCols <- names(dailyAspectsWide)[-1]
  dailyAspectsWide[, c(aspectsCols) := lapply(.SD, as.factor), .SDcols = aspectsCols]

  return(dailyAspectsWide)
}

dailyCombPlanetAspectsFactorsTableLJ <- function(orbLimit = 2, aspectFilter = c(), pxSelect = c(), pySelect = c()) {
  idCols <- c('Date', 'Hour')
  setModernMixAspectsSet1()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)

  # Filter minor MO aspects that can overlap multiples to same target planet
  # in the same day and should not be significant in effect.
  dailyAspects$filter <- F
  dailyAspects[p.x == "MO" & aspect %in% c(15, 45, 75, 103, 135, 165), filter := T]
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]

  # Arrange aspects factors as table wide format.
  dailyAspectsWide <- dcast(
    dailyAspects,
    Date ~ origin,
    value.var = "aspect",
    fill = ""
  )
  setDT(dailyAspectsWide)

  aspectsCols <- names(dailyAspectsWide)[-1]
  dailyAspectsWide[, c(aspectsCols) := lapply(.SD, as.factor), .SDcols = aspectsCols]

  return(dailyAspectsWide)
}

dailyCombPlanetAspectsFactorsTableLDB <- function(orbLimit = 2, aspectFilter = c(), pxSelect = c(), pySelect = c()) {
  idCols <- c('Date', 'Hour')
  setClassicAspectsSet8()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)

  # Filter minor MO aspects that can overlap multiples to same target planet
  # in the same day and should not be significant in effect.
  dailyAspects$filter <- F
  dailyAspects[p.x == "MO" & aspect == 30, filter := T]
  dailyAspects[p.x == "MO" & aspect == 45, filter := T]
  dailyAspects[p.x == "MO" & aspect == 135, filter := T]
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  # Ignore aspects without type.
  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  # Categorize applicative / separative aspects.
  dailyAspects <- dailyAspects[p.x != "MO", aspect := as.character(paste(aspect, type, sep = ""))]

  # Arrange aspects factors as table wide format.
  dailyAspectsWide <- dcast(
    dailyAspects,
    Date ~ origin,
    value.var = "aspect",
    fill = ""
  )
  setDT(dailyAspectsWide)

  aspectsCols <- names(dailyAspectsWide)[-1]
  dailyAspectsWide[, c(aspectsCols) := lapply(.SD, as.factor), .SDcols = aspectsCols]

  return(dailyAspectsWide)
}

dailyCombPlanetAspectsFactorsTableLDC <- function(orbLimit = 2, aspectFilter = c(), pxSelect = c(), pySelect = c()) {
  idCols <- c('Date', 'Hour')
  setClassicAspectsSet8()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)

  # Filter minor MO aspects that can overlap multiples to same target planet
  # in the same day and should not be significant in effect.
  dailyAspects$filter <- F
  dailyAspects[p.x == "MO" & aspect == 30, filter := T]
  dailyAspects[p.x == "MO" & aspect == 45, filter := T]
  dailyAspects[p.x == "MO" & aspect == 135, filter := T]
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  # Ignore aspects without type.
  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  # Categorize applicative / separative aspects.
  dailyAspects <- dailyAspects[p.x != "MO", aspect := as.character(paste(aspect, "-", round(orb), sep = ""))]

  # Arrange aspects factors as table wide format.
  dailyAspectsWide <- dcast(
    dailyAspects,
    Date ~ origin,
    value.var = "aspect",
    fill = ""
  )
  setDT(dailyAspectsWide)

  aspectsCols <- names(dailyAspectsWide)[-1]
  dailyAspectsWide[, c(aspectsCols) := lapply(.SD, as.factor), .SDcols = aspectsCols]

  return(dailyAspectsWide)
}

dailyAspectsFilter <- function(
  dailyAspects,
  pxSelect = NULL,
  pySelect = NULL,
  aspectSelect = NULL,
  aspectFilter = NULL
) {
  dailyAspects$filter <- F

  if (!is.null(pxSelect)) {
    dailyAspects[p.x %ni% pxSelect, filter := T]
  }

  if (!is.null(pySelect)) {
    dailyAspects[p.y %ni% pySelect, filter := T]
  }

  if (!is.null(aspectSelect)) {
    dailyAspects[aspect %ni% aspectSelect, filter := T]
  }

  if (!is.null(aspectFilter)) {
    dailyAspects[aspect %in% aspectFilter, filter := T]
  }

  dailyAspects[filter != T,]
}

dailyAspectsGeneralizedCount <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = NULL,
  pySelect = NULL,
  aspectSelect = NULL,
  aspectFilter = NULL,
  binFlag = F
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setModernMixAspectsSet1()
    setPlanetsMOMEVESUMACEVSJUNNSAURCHNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_12', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  dailyAspects <- dailyAspectsFilter(
    dailyAspects,
    pxSelect = pxSelect,
    pySelect = pySelect,
    aspectSelect = aspectSelect,
    aspectFilter = aspectFilter
  )

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dailyAspects,
    Date ~ aspect,
    fun.aggregate = length,
    value.var = "aspect",
    fill = 0
  )
  setDT(dailyAspectsCount)

  # Convert counts to binary flags when setting is enabled.
  if (binFlag) {
    aspectCols <- names(dailyAspectsCount)[-1]
    dailyAspectsCount[,
      c(aspectCols) := lapply(.SD, function(x) ifelse(x > 1, 1, 0)),
      .SDcols = aspectCols
    ]
  }

  return(dailyAspectsCount)
}

dailyAspectsGeneralizedCountLDI <- function(orbLimit = 2, pxSelect = c(), pySelect = c(), aspectFilter = c(), binFlag = F) {
  idCols <- c('Date', 'Hour')
  setModernMixAspectsSet1()
  setPlanetsMOMEVESUMACEJUNNSAURNEPL()
  hourlyPlanets <<- openHourlyPlanets('planets_12', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)

  dailyAspects$filter <- F
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dailyAspects,
    Date ~ aspect,
    fun.aggregate = length,
    value.var = "aspect",
    fill = 0
  )
  setDT(dailyAspectsCount)

  # Convert counts to binary flags when setting is enabled.
  if (binFlag) {
    aspectCols <- names(dailyAspectsCount)[-1]
    dailyAspectsCount[,
      c(aspectCols) := lapply(.SD, function(x) ifelse(x > 1, 1, 0)),
      .SDcols = aspectCols
    ]
  }

  return(dailyAspectsCount)
}

dailyAspectsGeneralizedOrbsMean <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = c(),
  pySelect = c(),
  aspectSelect = c()
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setClassicAspectsSet8()
    setPlanetsMOMEVESUMAJUNNSAURNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  dailyAspects$filter <- F
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %ni% aspectSelect, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  # Arrange aspects factors as table wide format.
  dailyAspectsOrbsMean <- dcast(
    dailyAspects,
    Date ~ aspect,
    fun.aggregate = mean,
    value.var = "orb",
    fill = 4
  )
  setDT(dailyAspectsOrbsMean)

  return(dailyAspectsOrbsMean)
}

dailyAspectsGeneralizedEnergySum <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = c(),
  pySelect = c(),
  aspectSelect = c(),
  aspectFilter = c(),
  energyFunction = partial(dailyAspectsAddEnergy, speedDecay = 0.59)
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setModernMixAspectsSet1()
    setPlanetsMOMEVESUMAJUNNSAURNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  dailyAspects <- energyFunction(dailyAspects)
  dailyAspects$filter <- F
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %ni% aspectSelect, filter := T]
  dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  # Arrange aspects factors as table wide format.
  dailyAspectsEnergySum <- dcast(
    dailyAspects,
    Date ~ aspect,
    fun.aggregate = sum,
    value.var = "ennow",
    fill = 0
  )
  setDT(dailyAspectsEnergySum)

  return(dailyAspectsEnergySum)
}

# Aspects energy per planet combination.
dailyPlanetYActivationEnergy <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = c(),
  pySelect = c(),
  aspectSelect = c(),
  energyFunction = partial(dailyAspectsAddEnergy, speedDecay = 0.59)
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setClassicAspectsSet8()
    setPlanetsMOMEVESUMAJUNNSAURNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  dailyAspects <- energyFunction(dailyAspects)
  dailyAspects$filter <- F
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %ni% aspectSelect, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  dailyAspects <- dailyAspects[, p.y := as.character(paste(p.y, "YE", sep = ""))]
  # Arrange aspects factors as table wide format.
  dailyAspectsEnergy <- dcast(
    dailyAspects,
    Date ~ p.y,
    fun.aggregate = sum,
    value.var = "ennow",
    fill = 0
  )
  setDT(dailyAspectsEnergy)

  return(dailyAspectsEnergy)
}

# Count total aspects per planet Y (receiver).
dailyPlanetYActivationCount <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = NULL,
  pySelect = NULL,
  aspectSelect = NULL,
  aspectFilter = NULL,
  binFlag = F
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setModernMixAspectsSet1()
    #setPlanetsMOMEVESUMAJUNNSAURNEPL()
    setPlanetsMOMEVESUMACEVSJUNNSAURCHNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_12', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  dailyAspects <- dailyAspectsFilter(
    dailyAspects,
    pxSelect = pxSelect,
    pySelect = pySelect,
    aspectSelect = aspectSelect,
    aspectFilter = aspectFilter
  )

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  dailyAspects <- dailyAspects[, p.y := as.character(paste(p.y, "Y", sep = ""))]
  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dailyAspects,
    Date ~ p.y,
    fun.aggregate = length,
    value.var = "aspect",
    fill = 0
  )
  setDT(dailyAspectsCount)

  # Convert counts to binary flags when setting is enabled.
  if (binFlag) {
    planetsCols <- names(dailyAspectsCount)[-1]
    dailyAspectsCount[,
      c(planetsCols) := lapply(.SD, function(x) ifelse(x > 1, 1, 0)),
      .SDcols = planetsCols
    ]
  }

  return(dailyAspectsCount)
}

dailyPlanetYActivationCountLDI <- function(orbLimit = 2, pxSelect = c(), pySelect = c(), aspectFilter = c(), binFlag = F) {
  idCols <- c('Date', 'Hour')
  setModernMixAspectsSet1()
  setPlanetsMOMEVESUMACEJUNNSAURNEPL()
  hourlyPlanets <<- openHourlyPlanets('planets_12', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)

  dailyAspects$filter <- F
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  dailyAspects <- dailyAspects[, p.y := as.character(paste(p.y, "Y", sep = ""))]
  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dailyAspects,
    Date ~ p.y,
    fun.aggregate = length,
    value.var = "aspect",
    fill = 0
  )
  setDT(dailyAspectsCount)

  # Convert counts to binary flags when setting is enabled.
  if (binFlag) {
    planetsCols <- names(dailyAspectsCount)[-1]
    dailyAspectsCount[,
      c(planetsCols) := lapply(.SD, function(x) ifelse(x > 1, 1, 0)),
      .SDcols = planetsCols
    ]
  }

  return(dailyAspectsCount)
}

# Count total aspects per planet X (emitter).
dailyPlanetXActivationCount <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = c(),
  pySelect = c(),
  aspectSelect = c(),
  aspectFilter = c(),
  binFlag = F
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setModernMixAspectsSet1()
    setPlanetsMOMEVESUMAJUNNSAURNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  dailyAspects$filter <- F
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %ni% aspectSelect, filter := T]
  dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  dailyAspects <- dailyAspects[, p.x := as.character(paste(p.x, "X", sep = ""))]
  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dailyAspects,
    Date ~ p.x,
    fun.aggregate = length,
    value.var = "aspect",
    fill = 0
  )
  setDT(dailyAspectsCount)

  # Convert counts to binary flags when setting is enabled.
  if (binFlag) {
    planetsCols <- names(dailyAspectsCount)[-1]
    dailyAspectsCount[,
      c(planetsCols) := lapply(.SD, function(x) ifelse(x > 1, 1, 0)),
      .SDcols = planetsCols
    ]
  }

  return(dailyAspectsCount)
}

# Daily planet Y aspects mean orb.
dailyPlanetYAspectMeanOrb <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = c(),
  pySelect = c(),
  aspectSelect = c(),
  aspectFilter = c()
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setModernMixAspectsSet1()
    setPlanetsMOMEVESUMAJUNNSAURNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  dailyAspects$filter <- F
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %ni% aspectSelect, filter := T]
  dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  dailyAspects <- dailyAspects[, p.y := as.character(paste(p.y, "YD", sep = ""))]
  # Arrange mean orb as table wide format.
  dailyAspectsOrb <- dcast(
    dailyAspects,
    Date ~ p.y,
    fun.aggregate = function(x) { round(mean(x), 1) },
    value.var = "orb",
    fill = 0
  )
  setDT(dailyAspectsOrb)

  return(dailyAspectsOrb)
}

# Count total aspects per planet combination.
dailyAspectsPlanetCombGeneralizedCount <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = c(),
  pySelect = c(),
  aspectSelect = c(),
  aspectFilter = c()
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setModernMixAspectsSet1()
    setPlanetsMOMEVESUMACEVSJUNNSAURCHNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_12', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  dailyAspects$filter <- F
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %ni% aspectSelect, filter := T]
  dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dailyAspects,
    Date ~ origin,
    fun.aggregate = length,
    value.var = "aspect",
    fill = 0
  )
  setDT(dailyAspectsCount)

  return(dailyAspectsCount)
}

# Aspects energy per planet combination.
dailyAspectsPlanetCombGeneralizedEnergy <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = c(),
  pySelect = c(),
  aspectSelect = c(),
  energyFunction = partial(dailyAspectsAddEnergy, speedDecay = 0.59)
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setClassicAspectsSet8()
    setPlanetsMOMEVESUMAJUNNSAURNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  dailyAspects <- energyFunction(dailyAspects)
  dailyAspects$filter <- F
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %ni% aspectSelect, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  # Arrange aspects factors as table wide format.
  dailyAspectsEnergy <- dcast(
    dailyAspects,
    Date ~ origin,
    fun.aggregate = sum,
    value.var = "ennow",
    fill = 0
  )
  setDT(dailyAspectsEnergy)

  return(dailyAspectsEnergy)
}

dailyPlanetsDeclinationTablePrepare <- function(hourlyPlanets = NULL, pSelect = NULL) {
  if (is.null(hourlyPlanets)) {
    idCols <- c('Date', 'Hour')
    setModernMixAspectsSet1()
    setPlanetsMOMEVESUMAJUNNSAURNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  }

  selectCols <- planetsDecCols
  if (!is.null(pSelect)) {
    selectCols <- paste0(pSelect, "DEC")
  }

  dailyPlanetsDeclination <- hourlyPlanets[,
    lapply(.SD, function(x) mean(x)),
    by = Date, .SDcols = selectCols
  ]

  return(dailyPlanetsDeclination)
}

dailyPlanetsSpeedTablePrepare <- function(hourlyPlanets = NULL, pSelect = NULL) {
  if (is.null(hourlyPlanets)) {
    idCols <- c('Date', 'Hour')
    setModernMixAspectsSet1()
    setPlanetsMOMEVESUMAJUNNSAURNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  }

  selectCols <- planetsSpCols
  if (!is.null(pSelect)) {
    selectCols <- paste0(pSelect, "SP")
  }

  dailyPlanetsSpeed <- hourlyPlanets[,
    lapply(.SD, function(x) round(mean(x), 3)),
    by = Date, .SDcols = selectCols
  ]

  return(dailyPlanetsSpeed)
}

dailyFastPlanetsRetrograde <- function() {
  dailyPlanetsSpeed <- dailyPlanetsSpeedTablePrepare()
  dailyPlanetsSpeed[, MESL := ifelse(MESP <= 0.25, 1, 0)]
  dailyPlanetsSpeed[, VESL := ifelse(VESP <= 0.25, 1, 0)]
  dailyPlanetsSpeed[, SUSL := ifelse(SUSP <= 0.25, 1, 0)]
  dailyPlanetsSpeed[, MOSL := ifelse(MOSP <= 0.25, 1, 0)]

  selCols <- c(
    'Date',
    'MESL',
    'VESL'
  )

  return(dailyPlanetsSpeed[, ..selCols])
}

dailySlowPlanetsRetrograde <- function() {
  dailyPlanetsSpeed <- dailyPlanetsSpeedTablePrepare()
  dailyPlanetsSpeed[, MASL := ifelse(MASP <= 0.25, 1, 0)]
  dailyPlanetsSpeed[, JUSL := ifelse(JUSP <= 0.25, 1, 0)]
  dailyPlanetsSpeed[, SASL := ifelse(SASP <= 0.25, 1, 0)]
  dailyPlanetsSpeed[, URSL := ifelse(URSP <= 0.25, 1, 0)]
  dailyPlanetsSpeed[, NESL := ifelse(NESP <= 0.25, 1, 0)]
  dailyPlanetsSpeed[, PLSL := ifelse(PLSP <= 0.25, 1, 0)]

  selCols <- c(
    'Date',
    'MASL',
    'JUSL',
    'SASL',
    'URSL'
  )

  return(dailyPlanetsSpeed[, ..selCols])
}

dailyPlanetXAspectsGeneralizedCount <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = c(),
  pySelect = c(),
  aspectSelect = c(),
  aspectFilter = c(),
  binFlag = F
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setModernMixAspectsSet1()
    #setPlanetsMOMEVESUMAJUNNSAURNEPL()
    setPlanetsMOMEVESUMACEVSJUNNSAURCHNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_12', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  dailyAspects$filter <- F
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %ni% aspectSelect, filter := T]
  dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dailyAspects,
    Date ~ p.x + aspect,
    fun.aggregate = length,
    value.var = "aspect",
    fill = 0
  )
  setDT(dailyAspectsCount)

  # Convert counts to binary flags when setting is enabled.
  if (binFlag) {
    aspectCols <- names(dailyAspectsCount)[-1]
    dailyAspectsCount[,
      c(aspectCols) := lapply(.SD, function(x) ifelse(x > 1, 1, 0)),
      .SDcols = aspectCols
    ]
  }

  return(dailyAspectsCount)
}

dailyPlanetYAspectsGeneralizedCount <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = c(),
  pySelect = c(),
  aspectSelect = c(),
  aspectFilter = c(),
  binFlag = F
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setModernMixAspectsSet1()
    #setPlanetsMOMEVESUMAJUNNSAURNEPL()
    setPlanetsMOMEVESUMACEVSJUNNSAURCHNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_12', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  dailyAspects$filter <- F
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %ni% aspectSelect, filter := T]
  dailyAspects[aspect %in% aspectFilter, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dailyAspects,
    Date ~ p.y + aspect,
    fun.aggregate = length,
    value.var = "aspect",
    fill = 0
  )
  setDT(dailyAspectsCount)

  # Convert counts to binary flags when setting is enabled.
  if (binFlag) {
    aspectCols <- names(dailyAspectsCount)[-1]
    dailyAspectsCount[,
      c(aspectCols) := lapply(.SD, function(x) ifelse(x > 1, 1, 0)),
      .SDcols = aspectCols
    ]
  }

  return(dailyAspectsCount)
}

dailyAspectsAddPolarity <- function(dailyAspects, aspectsPolarity) {
  aspectsPolarityIndex <- matrix(
    aspectsPolarity, nrow = 1, ncol = length(aspectsPolarity),
    byrow = T, dimnames = list(c('polarity'), aspects)
  )

  dailyAspects[, poldir := aspectsPolarityIndex['polarity', as.character(aspect)]]
  dailyAspects[poldir == 0, polarity := "neg"]
  dailyAspects[poldir == 1, polarity := "pos"]

  return(dailyAspects)
}

# PlanetY aspect activation count by polarity.
dailyPlanetYActivationPolarityCount <- function(
  dailyAspects = NULL,
  orbLimit = 2,
  pxSelect = c(),
  pySelect = c(),
  aspectSelect = c(),
  polarityFunction = dailyAspectsAddPolarity
) {
  if (is.null(dailyAspects)) {
    idCols <- c('Date', 'Hour')
    setClassicAspectsSet8()
    setPlanetsMOMEVESUMAJUNNSAURNEPL()
    hourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
    dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols, orbLimit)
  }

  dailyAspects <- polarityFunction(dailyAspects)
  dailyAspects$filter <- F
  dailyAspects[p.x %ni% pxSelect, filter := T]
  dailyAspects[p.y %ni% pySelect, filter := T]
  dailyAspects[aspect %ni% aspectSelect, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  if (nrow(dailyAspects) == 0) {
    return(NULL)
  }

  # Convert numeric aspects to categorical (factors).
  dailyAspects <- dailyAspects[, aspect := as.character(paste("a", aspect, sep = ""))]
  dailyAspects <- dailyAspects[, p.y := as.character(paste(p.y, "Y", sep = ""))]

  # Arrange aspects factors as table wide format.
  dailyAspectsCount <- dcast(
    dailyAspects,
    Date ~ p.y + polarity,
    fun.aggregate = length,
    value.var = "polarity",
    fill = 0
  )
  setDT(dailyAspectsCount)

  return(dailyAspectsCount)
}

# Filter daily aspects rows within custom orb per aspect.
selectAspectsWithCustomOrb <- function(dailyAspects, customAspectsOrb = NULL) {
  if (is.null(customAspectsOrb)) {
    customAspectsOrb <- deforbs
  }

  aspectOrbsIndex <- matrix(
    customAspectsOrb, nrow = 1, ncol = length(customAspectsOrb),
    byrow = T, dimnames = list(c('orb'), aspects)
  )

  # Calculate max and proportional energy.
  dailyAspects$filter <- F
  dailyAspects[orb > aspectOrbsIndex['orb', as.character(aspect)], filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  return(dailyAspects)
}

selectSeparativeAspectsWithCustomOrb <- function(dailyAspects, customOrb = 1) {
  dailyAspects$filter <- F
  dailyAspects[type == 'S' & orb > customOrb, filter := T]
  dailyAspects <- dailyAspects[filter != T,]

  return(dailyAspects)
}

selectPlanetXAspects <- function(dailyAspects, px, aspectsSelect = c()) {
  dailyAspects$filter <- F
  dailyAspects[p.x == px & aspect %in% aspectsSelect]
  dailyAspects <- dailyAspects[filter != T,]

  return(dailyAspects)
}

prepareDailyAspectsCountPlanetYActivationCount <- function(
  dailyAspectsRows,
  orbLimit,
  pxSelect,
  pySelect,
  aspectSelect
) {
  dailyAspectsGeneralizedCount <- dailyAspectsGeneralizedCount(
    dailyAspects = dailyAspectsRows,
    orbLimit = orbLimit,
    pxSelect = pxSelect,
    pySelect = pySelect,
    aspectSelect = aspectSelect
  )

  if (is.null(dailyAspectsGeneralizedCount)) {
    return(NULL)
  }

  dailyPlanetYActivationCount <- dailyPlanetYActivationCount(
    dailyAspects = dailyAspectsRows,
    orbLimit = orbLimit,
    pxSelect = pxSelect,
    pySelect = pySelect,
    aspectSelect = aspectSelect
  )

  if (is.null(dailyPlanetYActivationCount)) {
    return(NULL)
  }

  dailyAspects <- merge(dailyAspectsGeneralizedCount, dailyPlanetYActivationCount, date = "Date")

  return(dailyAspects)
}
