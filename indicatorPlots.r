library(grid)
library(cowplot)
source("./analysis.r")
todayDate <- as.Date(Sys.Date())

analyzeSecurity <- function(symbol) {
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

  aspectsTheme <- function() {
    # theme(axis.text.x = element_text(angle = 90, size = 8)) +
    list(
      theme_black(),
      theme(panel.spacing = unit(c(0, 0, 0, 0), "null")),
      theme(plot.margin = unit(c(0, 0, 0, 0), "null")),
      theme(panel.grid = element_blank()),
      theme(panel.border = element_blank()),
      theme(plot.margin = unit(c(0, 0, 0, 0), "null")),
      theme(panel.spacing = unit(c(0, 0, 0, 0), "null")),
      theme(axis.ticks = element_blank()),
      theme(axis.text = element_blank()),
      theme(axis.title = element_blank()),
      theme(axis.line = element_blank()),
      theme(legend.position = "none"),
      theme(axis.ticks.length = unit(0, "null")),
      theme(axis.ticks.margin = unit(0, "null")),
      theme(legend.margin = unit(0, "null")),
      theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()),
      theme(axis.title.y = element_blank()))
  }

  aspectsLines <- function() {
    list(geom_hline(yintercept = 165, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 155, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 145, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 140, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 130, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 125, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 115, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 100, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 80, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 65, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 55, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 50, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 40, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 35, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 25, linetype = "longdash", color = "salmon2", size = 1, alpha = 0.7),
         geom_hline(yintercept = 15, linetype = "longdash", color = "magenta2", size = 1, alpha = 0.7),
         geom_vline(xintercept = currentDates, linetype = "dashed", color = "white", size = 0.6, alpha = 0.7),
         geom_vline(xintercept = as.Date(datesHighs), linetype = "dashed", color = "green", size = 0.6, alpha = 0.7),
         geom_vline(xintercept = as.Date(datesLows), linetype = "dashed", color = "red", size = 0.6, alpha = 0.7),
         scale_y_continuous(breaks = seq(0, 180, by = 10)),
         scale_x_date(date_breaks = dateBreaks, limits = chartPeriod))
  }

  uranusIndicators <- function() {
    p <- ggplot(data = dailyPlanetsResearch) +
      geom_point(aes(x = Date, y = SUURLON, size = 1), colour = "yellow", alpha = 0.5, size = 1) +
      geom_point(aes(x = Date, y = MOURLON, size = 1), colour = "black", alpha = 0.6, size = 1) +
      geom_point(aes(x = Date, y = MEURLON, size = 1), colour = "orange", alpha = 0.5, size = 1) +
      geom_point(aes(x = Date, y = VEURLON, size = 1), colour = "pink", alpha = 0.7, size = 1) +
      geom_point(aes(x = Date, y = MAURLON, size = 1), colour = "red", alpha = 0.6, size = 1) +
      geom_point(aes(x = Date, y = JUURLON, size = 1), colour = "palegreen3", alpha = 0.6, size = 1) +
      geom_point(aes(x = Date, y = SAURLON, size = 1), colour = "gray", alpha = 0.6, size = 1) +
      geom_point(aes(x = Date, y = NNURLON, size = 1), colour = "mediumaquamarine", alpha = 0.6, size = 1) +
      aspectsLines() +
      aspectsTheme()
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
  dailySpeed <- melt(dailyPlanets, id.var = idCols, variable.name = 'origin', value.name = 'sp', measure.var = planetsSpCols)
  dailySpeed[, spn := normalize(sp), by = c('origin')]
  # dailyPlanets[, c(planetsSpCols) := lapply(.SD, normalize), .SDcols=planetsSpCols]
  dailySpeedX <- copy(dailySpeed)
  dailySpeedX[, p.x := substr(origin, 1, 2)]
  dailySpeedX[, sp.x := round(sp / 24, 3)]
  dailySpeedX[, spn.x := spn]
  # Merge daily speed.
  dailySpeedY <- copy(dailySpeed)
  dailySpeedY[, p.y := substr(origin, 1, 2)]
  dailySpeedY[, sp.y := round(sp / 24, 3)]
  dailySpeedY[, spn.y := spn]
  selectColsY <- c(idCols, 'p.y', 'sp.y', 'spn.y')
  dailyAspects <- merge(dailyAspects, dailySpeedY[, ..selectColsY], by = c(idCols, 'p.y'))
  selectColsX <- c(idCols, 'p.x', 'sp.x', 'spn.x')
  dailyAspects <- merge(dailyAspects, dailySpeedX[, ..selectColsX], by = c(idCols, 'p.x'))

  return(dailyAspects)
}

dailyAspectsAddOrbs <- function(dailyAspects, dailyPlanets, idCols = c('Date')) {
  # Melt orbs.
  dailyAspectsOrbs <- melt(
    dailyPlanets, id.var = idCols, variable.name = 'origin',
    value.name = 'orb', measure.var = planetsCombOrb
  )

  dailyAspectsOrbs[, orb := round(orb, 2)]
  dailyAspectsOrbs[, origin := substr(origin, 1, 4)]
  #aspects.day.long[, orbdir := sign(orb - Lag(orb)), by=c('lon', 'origin', 'aspect')]
  # Join aspects & orbs.
  dailyAspects <- merge(dailyAspects, dailyAspectsOrbs, by = c(idCols, 'origin'))

  # Calculate orb direction (applicative, separative).
  dailyAspects[, orbdir := round(orb - Lag(orb), 2), by = c('origin', 'aspect')]
  dailyAspects[, type := cut(orbdir, c(-100, 0, 100), labels = (c('applicative', 'separative')))]

  return(dailyAspects)
}

dailyAspectsAddLongitude <- function(dailyAspects, dailyPlanets, idCols = c('Date')) {
  # Melt longitudes.
  dailyLongitudes <- melt(dailyPlanets, id.var = idCols, variable.name = 'origin',
                          value.name = 'lon', measure.var = planetsLonCols)

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
  selectColsY <- c(idCols, 'p.y', 'lon.y')
  dailyAspects <- merge(dailyAspects, dailyLongitudesY[, ..selectColsY], by = c(idCols, 'p.y'))
  selectColsX <- c(idCols, 'p.x', 'lon.x')
  dailyAspects <- merge(dailyAspects, dailyLongitudesX[, ..selectColsX], by = c(idCols, 'p.x'))
}

dailyAspectsAddEnergy <- function(dailyAspects, speedDecay = 0.6) {
  # For aspects: c( 0 , 30 , 45 , 60 , 90 , 120 , 135 , 150 , 180)
  aspectsEnergyIndex <- matrix(
    aspectsEnergy, nrow = 1, ncol = length(aspectsEnergy),
    byrow = T, dimnames = list(c('energy'), aspects)
  )

  # Calculate max and proportional energy.
  dailyAspects[, enmax := aspectsEnergyIndex['energy', as.character(aspect)]]
  #dailyAspects[, enmax := 1]
  dailyAspects[, ennow := energyDecay(enmax, orb, speedDecay)]

  return(dailyAspects)
}

dailyAspectsAddCumulativeEnergy <- function(dailyAspects, securityTrain, idCols = c('Date')) {
  # Merge daily security prices with aspects.
  dailyAspectsPriceResearch <- merge(dailyAspects, securityTrain[, c('Date', 'diffPercent')], by = c('Date'))

  # Calculate the historical mean aspect effect.
  aspectsEffect <- dailyAspectsPriceResearch[
    orb <= 1,
    list(round(mean(diffPercent), 4), round(median(diffPercent), 4)),
    by = c('origin', 'aspect', 'type')
  ]
  setnames(aspectsEffect, c('origin', 'aspect', 'type', 'diffMean', 'diffMedian'))
  #dailyAspects[, diffMean := setAspectEffect(.SD), by=c('Date', 'origin', 'aspect', 'type')]
  dailyAspectsPriceResearch <- merge(dailyAspectsPriceResearch, aspectsEffect, by = c('origin', 'aspect', 'type'))
  dailyAspects <- merge(dailyAspects, aspectsEffect, by = c('origin', 'aspect', 'type'))

  # Melt involved planets current energy for body strength calculation.
  dailyAspectsPlanetsEnergy <- melt(
    dailyAspects, id.var = c(idCols, 'ennow'),
    variable.name = 'origin', value.name = 'planet', measure.var = c('p.x', 'p.y')
  )
  dailyAspectsCumulativeEnergy <- dailyAspectsPlanetsEnergy[, round(sum(ennow), 2), by = c(idCols, 'planet')]

  # Merge cumulative planets energy.
  setnames(dailyAspectsCumulativeEnergy, c(idCols, 'p.x', 'encum.x'))
  dailyAspects <- merge(dailyAspects, dailyAspectsCumulativeEnergy, by = c(idCols, 'p.x'))
  setnames(dailyAspectsCumulativeEnergy, c(idCols, 'p.y', 'encum.y'))
  dailyAspects <- merge(dailyAspects, dailyAspectsCumulativeEnergy, by = c(idCols, 'p.y'))

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

dailyAspectsEffectIndex <- function(dailyAspects, idCols = c('Date')) {
  # Daily aspects effect index.
  dailyAspectsIndex <- dailyAspects[, round(sum(effect), 2), by = idCols]
  dailyAspectsIndex[, diff := round(Delt(V1, k = 1), 2)]
  setnames(dailyAspectsIndex, c('Date', 'effect', 'diff'))
  dailyAspectsIndex[, effectMA := SMA(effect, 3)]

  return(dailyAspectsIndex)
}

predictSecurityModelReport <- function(dailyAspects, dailyAspectsIndex, securityTest) {
  cat("Today aspects:", format(todayDate, "%Y-%m-%d"), "\n")
  print(dailyAspects[Date == todayDate,][order(-entot)], topn = 200)
  cat("\n")

  cat("Tomorrow aspects:", format(todayDate + 1, "%Y-%m-%d"), "\n")
  print(dailyAspects[Date == todayDate + 1,][order(-entot)], topn = 200)
  cat("\n")

  cat("Past tomorrow aspects:", format(todayDate + 2, "%Y-%m-%d"), "\n")
  print(dailyAspects[Date == todayDate + 2,][order(-entot)], topn = 200)
  cat("\n")

  dailyAspectsIndexProjected <- dailyAspectsIndex[Date > todayDate - 8,][0:60]
  cat("Daily aspects effect index:\n")
  print(dailyAspectsIndexProjected, topn = 100)
  cat("\n")

  cat("Daily test period:\n")
  modelTest <- merge(dailyAspectsIndex, securityTest[, c('Date', 'Mid', 'diffPercent')], by = c('Date'))
  print(modelTest)

  p1 <- ggplot(data = modelTest) +
    geom_line(aes(x = Date, y = Mid), colour = "white", alpha = 0.8) +
    theme_black()

  p2 <- ggplot(data = modelTest) +
    geom_line(aes(x = Date, y = effect), colour = "white", alpha = 0.8) +
    geom_line(aes(x = Date, y = effectMA), colour = "yellow", alpha = 0.8) +
    theme_black()

  p3 <- ggplot(data = modelTest) +
    geom_point(aes(x = effect, y = abs(diffPercent)), colour = "white", alpha = 0.8) +
    theme_black()

  p4 <- ggplot(data = modelTest) +
    geom_point(aes(x = diff, y = diffPercent), colour = "white", alpha = 0.8) +
    theme_black()

  p5 <- ggplot(data = dailyAspectsIndexProjected) +
    geom_line(aes(x = Date, y = effect), colour = "white", alpha = 0.8) +
    geom_line(aes(x = Date, y = effectMA), colour = "yellow", alpha = 0.8) +
    scale_x_date(date_breaks = '3 days', date_labels = "%Y-%m-%d") +
    theme_black()

  pgridTop <- plot_grid(p1, p3, p2, p4)
  pgrid <- plot_grid(pgridTop, p5, ncol = 1, rel_heights = c(1.7, 1))
  print(pgrid)
  cat("\nCORRELATION EFFECT / MOVE RANGE: ", cor(modelTest$effect, abs(modelTest$diffPercent), method = "pearson"), "\n")
  cat("\nCORRELATION EFFECT / PRICE: ", cor(modelTest$effect, modelTest$Mid, method = "pearson"), "\n")
  cat("\nCORRELATION EFFECT MA / PRICE: ", cor(modelTest$effectMA, modelTest$Mid, method = "pearson"), "\n")
}

dailyAspectsNameCols <- function(dailyAspects) {
  # Set more convenient order for analysis.
  colsOrder <- c('Date', 'origin', 'p.x', 'lon.x', 'sp.x', 'spn.x', 'p.y', 'lon.y', 'sp.y', 'spn.y', 'aspect', 'type', 'orb', 'orbdir', 'enmax', 'ennow', 'encum.x', 'encum.y', 'entot', 'effect', 'diffMean', 'diffMedian')
  setcolorder(dailyAspects, colsOrder)

  return(dailyAspects)
}

dailyHourlyAspectsTablePrepare <- function(dailyHourlyPlanets, idCols) {
  dailyHourlyPlanetsRange <- dailyHourlyPlanets[Date >= as.Date("2015-01-01") & Date <= as.Date("2023-01-01")]
  # Melt aspects.
  dailyAspects <- melt(
    dailyHourlyPlanetsRange, id.var = idCols,
    variable.name = 'origin', value.name = 'aspect',
    value.factor = T, measure.var = planetsCombAsp, na.rm = T
  )

  dailyAspects[, origin := substr(origin, 1, 4)]
  dailyAspects <- dailyAspectsAddOrbs(dailyAspects, dailyHourlyPlanets, idCols)
  dailyAspects <- dailyAspectsAddLongitude(dailyAspects, dailyHourlyPlanets, idCols)
  dailyAspects <- dailyAspectsAddSpeed(dailyAspects, dailyHourlyPlanets, idCols)

  return(dailyAspects)
}

dailyAspectsTablePrepare <- function(dailyPlanets) {
  dailyPlanetsRange <- dailyPlanets[Date >= as.Date("2015-01-01") & Date <= as.Date("2023-01-01")]
  # Melt aspects.
  dailyAspects <- melt(dailyPlanetsRange, id.var = c('Date'), variable.name = 'origin',
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
predictSecurityModelA <- function(symbol) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet()
  setPlanetsMEVESUMACEJUSAURNEPL()
  #setPlanetsMEVESUMAJUSAURNEPL()
  security <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
  securityTrain <- security[Date <= as.Date("2020-06-30"),]
  securityTest <- security[Date > as.Date("2020-06-30"),]
  dailyPlanets <<- openPlanets('planets_10', clear = F)

  dailyAspects <- dailyAspectsTablePrepare(dailyPlanets)
  dailyAspects <- dailyAspectsAddEnergy(dailyAspects, 0.5)
  dailyAspects <- dailyAspectsAddCumulativeEnergy(dailyAspects, securityTrain)
  dailyAspects <- dailyAspectsAddEffectM1(dailyAspects)
  #dailyAspects <- dailyAspectsAddEffectM2(dailyAspects)
  dailyAspects <- dailyAspectsNameCols(dailyAspects)
  dailyAspectsIndex <- dailyAspectsEffectIndex(dailyAspects)

  predictSecurityModelReport(dailyAspects, dailyAspectsIndex, securityTest)
  dailyAspectsPriceEffect <- merge(dailyAspects, security[, c('Date', 'Mid', 'diffPercent')], by = c('Date'))

  return(dailyAspectsPriceEffect)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set.
# - Don't include MO, CE and NN.
# - Use common daily aspects true energy disregard the historical security effect.
# - Increase strength of 90 aspects energy by 2x.
predictSecurityModelB <- function(symbol) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet()
  setPlanetsMEVESUMAJUSAURNEPL()
  security <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
  securityTrain <- security[Date <= as.Date("2020-06-30"),]
  securityTest <- security[Date > as.Date("2020-06-30"),]
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  idCols <- c('Date', 'Hour')

  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.5)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects, securityTrain, idCols)
  hourlyAspects <- dailyAspectsAddEffectM1(hourlyAspects)

  cat("\nHourly aspects index: \n")
  hourlyAspectsIndex <- hourlyAspectsEffectIndex(hourlyAspects)
  print(hourlyAspectsIndex[Date > todayDate-1, ][0:100], topn = 100)

  # Calculate aspects effect indexes.
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)
  predictSecurityModelReport(hourlyAspects, dailyAspectsIndex, securityTest)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set.
# - Don't include CE, and include all the planets and MO.
# - Use common daily aspects true energy disregard the historical security effect.
# - Increase strength of 90 aspects energy by 2x.
predictSecurityModelC <- function(symbol) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet()
  #setPlanetsMOMEVESUMAJUSAURNEPL()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  security <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
  securityTrain <- security[Date <= as.Date("2020-06-30"),]
  securityTest <- security[Date > as.Date("2020-06-30"),]
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  idCols <- c('Date', 'Hour')

  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.5)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects, securityTrain, idCols)
  hourlyAspects <- dailyAspectsAddEffectM1(hourlyAspects)

  cat("\nHourly aspects index: \n")
  hourlyAspectsIndex <- hourlyAspectsEffectIndex(hourlyAspects)
  print(hourlyAspectsIndex[Date > todayDate-1, ][0:100], topn = 100)

  # Calculate aspects effect indexes.
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)
  predictSecurityModelReport(hourlyAspects, dailyAspectsIndex, securityTest)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set but limiting energy to a set: 45, 90, 120 and 150.
# - Increase strength of 90 and 150 aspects energy by 2x.
# - Don't include CE, and include all the planets and MO.
# - Use common daily aspects true energy disregard the historical security effect.
predictSecurityModelD <- function(symbol) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet2()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  security <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
  securityTrain <- security[Date <= as.Date("2020-06-30"),]
  securityTest <- security[Date > as.Date("2020-06-30"),]
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  idCols <- c('Date', 'Hour')

  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.5)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects, securityTrain, idCols)
  hourlyAspects <- dailyAspectsAddEffectM1(hourlyAspects)

  cat("\nHourly aspects index: \n")
  hourlyAspectsIndex <- hourlyAspectsEffectIndex(hourlyAspects)
  print(hourlyAspectsIndex[Date > todayDate-1, ][0:100], topn = 100)

  # Calculate aspects effect indexes.
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)
  predictSecurityModelReport(hourlyAspects, dailyAspectsIndex, securityTest)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set with energy polarity.
# - Increase strength of 90 aspects energy by 2x.
# - Don't include CE, and include all the planets and MO.
# - Use common daily aspects true energy disregard the historical security effect.
predictSecurityModelE <- function(symbol) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet3()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  security <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
  securityTrain <- security[Date <= as.Date("2020-06-30"),]
  securityTest <- security[Date > as.Date("2020-06-30"),]
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  idCols <- c('Date', 'Hour')

  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.5)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects, securityTrain, idCols)
  hourlyAspects <- dailyAspectsAddEffectM1(hourlyAspects)

  cat("\nHourly aspects index: \n")
  hourlyAspectsIndex <- hourlyAspectsEffectIndex(hourlyAspects)
  print(hourlyAspectsIndex[Date > todayDate-1, ][0:100], topn = 100)

  # Calculate aspects effect indexes.
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)
  predictSecurityModelReport(hourlyAspects, dailyAspectsIndex, securityTest)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set with energy polarity.
# - Increase strength of 90 aspects energy by 2x.
# - Effect is the proportion to the orb from the received aspect energy accounting with the original polarity.
# - Don't include CE, and include all the planets and MO.
# - Use common daily aspects true energy disregard the historical security effect.
predictSecurityModelF <- function(symbol) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet3()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  security <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
  securityTrain <- security[Date <= as.Date("2020-06-30"),]
  securityTest <- security[Date > as.Date("2020-06-30"),]
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  idCols <- c('Date', 'Hour')

  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.5)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects, securityTrain, idCols)
  hourlyAspects <- dailyAspectsAddEffectM3(hourlyAspects)

  cat("\nHourly aspects index: \n")
  hourlyAspectsIndex <- hourlyAspectsEffectIndex(hourlyAspects)
  print(hourlyAspectsIndex[Date > todayDate-1, ][0:100], topn = 100)

  # Calculate aspects effect indexes.
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)
  predictSecurityModelReport(hourlyAspects, dailyAspectsIndex, securityTest)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set with energy polarity.
# - Increase strength of 90 aspects energy by 2x.
# - Effect is the sum of the cumulative and own energy.
# - Don't include CE, and include all the planets and MO.
# - Use common daily aspects true energy disregard the historical security effect.
predictSecurityModelG <- function(symbol) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet3()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  security <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
  securityTrain <- security[Date <= as.Date("2020-06-30"),]
  securityTest <- security[Date > as.Date("2020-06-30"),]
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  idCols <- c('Date', 'Hour')

  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.5)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects, securityTrain, idCols)
  hourlyAspects <- dailyAspectsAddEffectM4(hourlyAspects)

  cat("\nHourly aspects index: \n")
  hourlyAspectsIndex <- hourlyAspectsEffectIndex(hourlyAspects)
  print(hourlyAspectsIndex[Date > todayDate-1, ][0:100], topn = 100)

  # Calculate aspects effect indexes.
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)
  predictSecurityModelReport(hourlyAspects, dailyAspectsIndex, securityTest)
}

# This model uses:
# - Hourly aspects & prices.
# - Classical aspects set with energy polarity.
# - Increase strength of 90 aspects energy by 2x.
# - Effect is the proportion to the orb from the received aspects cumulative energy accounting with the original polarity.
# - Don't include CE, and include all the planets and MO.
# - Orb decay energy speed is slower to 0.2.
# - MO energy only accounts as cumulative for other aspects.
# - Use common daily aspects true energy disregard the historical security effect.
predictSecurityModelH <- function(symbol) {
  # Best effect correlation when using classic aspects only.
  setClassicAspectsSet3()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  security <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
  securityTrain <- security[Date <= as.Date("2020-06-30"),]
  securityTest <- security[Date > as.Date("2020-06-30"),]
  dailyHourlyPlanets <<- openHourlyPlanets('planets_11', clear = F)
  idCols <- c('Date', 'Hour')

  hourlyAspects <- dailyHourlyAspectsTablePrepare(dailyHourlyPlanets, idCols)
  hourlyAspects <- dailyAspectsAddEnergy(hourlyAspects, 0.2)
  hourlyAspects <- dailyAspectsAddCumulativeEnergy(hourlyAspects, securityTrain, idCols)
  # MO only contribute to the cumulative effect but is not a major indicator.
  hourlyAspects <- hourlyAspects[ p.x != 'MO', ]
  hourlyAspects <- dailyAspectsAddEffectM3(hourlyAspects)

  cat("\nHourly aspects index: \n")
  hourlyAspectsIndex <- hourlyAspectsEffectIndex(hourlyAspects)
  print(hourlyAspectsIndex[Date > todayDate-1, ][0:100], topn = 100)

  # Calculate aspects effect indexes.
  dailyAspectsIndex <- dailyAspectsEffectIndex(hourlyAspects)
  predictSecurityModelReport(hourlyAspects, dailyAspectsIndex, securityTest)
}