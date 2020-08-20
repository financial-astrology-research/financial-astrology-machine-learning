library(grid)
source("./analysis.r")
setClassicAspectsSet()
#setModernAspectsSet()
setPlanetsMOMEVESUMACEJUSAURNEPL()
#getMySymbolsData("working")
todayDate <- as.Date(Sys.Date())
span <- 22

analyzeSecurity <- function(symbol) {

  drawSecurityPriceSerie <- function() {
    securityPeriod <- security[Date >= chartPeriod[1] & Date <= chartPeriod[2],]
    p <- ggplot(data = securityPeriod) +
      geom_line(aes(x = Date, y = Mid), colour = "white", alpha = 0.7) +
      geom_vline(xintercept = as.Date(datesHighs), linetype = "dashed", color = "green", size = 0.6, alpha = 0.7) +
      geom_vline(xintercept = as.Date(datesLows), linetype = "dashed", color = "red", size = 0.6, alpha = 0.7) +
      scale_x_date(date_breaks = dateBreaks, date_labels = "%Y-%m-%d", limits = chartPeriod) +
      scale_y_log10() +
      theme_black() +
      theme(panel.margin = unit(c(0, 0, 0, 0), "null")) +
      theme(plot.margin = unit(c(0, 0, 0, 0), "null")) +
      theme(panel.grid = element_blank()) +
      theme(panel.border = element_blank()) +
      theme(plot.margin = unit(c(0, 0, 0, 0), "null")) +
      theme(panel.margin = unit(c(0, 0, 0, 0), "null")) +
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
      theme(panel.margin = unit(c(0, 0, 0, 0), "null")),
      theme(plot.margin = unit(c(0, 0, 0, 0), "null")),
      theme(panel.grid = element_blank()),
      theme(panel.border = element_blank()),
      theme(plot.margin = unit(c(0, 0, 0, 0), "null")),
      theme(panel.margin = unit(c(0, 0, 0, 0), "null")),
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
  # Max/Min speed normalization.
  # dailyPlanets[, c(planetsSpCols) := lapply(.SD, normalize), .SDcols=planetsSpCols]
  dailyPlanets <<- dailyPlanets

  dailyPlanetsResearch <- dailyPlanets[Date > as.Date('2017-01-01') & Date < as.Date('2021-01-01'),]
  chartPeriod <- c(as.Date("2018-01-10"), as.Date("2020-12-31"))
  currentDates <- c(todayDate, todayDate + 1)
  dateBreaks <- "7 days"
  security <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
  dailyPlanetPriceResearch <- merge(dailyPlanets, security, by = c('Date'))

  # Melt aspects.
  dailyAspects <- melt(dailyPlanets, id.var = c('Date'), variable.name = 'origin',
                       value.name = 'aspect', value.factor = T, measure.var = planetsCombAsp, na.rm = T)
  dailyAspects[, origin := substr(origin, 1, 4)]

  # Melt orbs.
  dailyAspectsOrbs <- melt(dailyPlanets, id.var = c('Date'), variable.name = 'origin', value.name = 'orb',
                           measure.var = planetsCombOrb)
  dailyAspectsOrbs[, origin := substr(origin, 1, 4)]
  #aspects.day.long[, orbdir := sign(orb - Lag(orb)), by=c('lon', 'origin', 'aspect')]

  # For aspects: c( 0 , 30 , 45 , 60 , 90 , 120 , 135 , 150 , 180)
  #aspectsEnergy <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
  #aspectsEnergyIndex <- matrix(aspectsEnergy, nrow = 1, ncol = length(aspectsEnergy), byrow = T,
                               #dimnames = list(c('energy'), aspects))
  # Join aspects & orbs.
  dailyAspects <- merge(dailyAspects, dailyAspectsOrbs, by = c('Date', 'origin'))

  # Calculate orb direction (applicative, separative).
  dailyAspects[, orbdir := round(orb - Lag(orb), 2), by = c('origin', 'aspect')]
  dailyAspects[, type := cut(orbdir, c(-100, 0, 100), labels = (c('applicative', 'separative')))]

  # Calculate max and proportional energy.
  dailyAspects[, enmax := 1]
  dailyAspects[, ennow := energyGrowth(enmax, orb, 0.3)]

  # Merge daily security prices with aspects.
  dailyAspectsPriceResearch <- merge(dailyAspects, security[, c('Date', 'diffPercent')], by = c('Date'))

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

  # Melt longitudes.
  dailyLongitudes <- melt(dailyPlanets, id.var = c('Date'), variable.name = 'origin',
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
  dailyAspects <- merge(dailyAspects, dailyLongitudesY[, c('Date', 'p.y', 'lon.y')], by = c('Date', 'p.y'))
  dailyAspects <- merge(dailyAspects, dailyLongitudesX[, c('Date', 'p.x', 'lon.x')], by = c('Date', 'p.x'))

  # Melt speeds.
  dailySpeed <- melt(dailyPlanets, id.var = c('Date'), variable.name = 'origin', value.name = 'sp', measure.var = planetsSpCols)
  dailySpeed[, spn := normalize(sp), by = c('origin')]
  # dailyPlanets[, c(planetsSpCols) := lapply(.SD, normalize), .SDcols=planetsSpCols]
  dailySpeedX <- copy(dailySpeed)
  dailySpeedX[, p.x := substr(origin, 1, 2)]
  dailySpeedX[, sp.x := round(sp/24, 3)]
  dailySpeedX[, spn.x := spn]
  # Merge daily speed.
  dailySpeedY <- copy(dailySpeed)
  dailySpeedY[, p.y := substr(origin, 1, 2)]
  dailySpeedY[, sp.y := round(sp/24, 3)]
  dailySpeedY[, spn.y := spn]
  dailyAspects <- merge(dailyAspects, dailySpeedY[, c('Date', 'p.y', 'sp.y', 'spn.y')], by = c('Date', 'p.y'))
  dailyAspects <- merge(dailyAspects, dailySpeedX[, c('Date', 'p.x', 'sp.x', 'spn.x')], by = c('Date', 'p.x'))

  # Melt involved planets current energy for body strength calculation.
  dailyAspectsPlanetsEnergy <- melt(
    dailyAspects, id.var = c('Date', 'ennow'),
    variable.name = 'origin', value.name = 'planet', measure.var = c('p.x', 'p.y')
  )
  dailyAspectsCumulativeEnergy <- dailyAspectsPlanetsEnergy[, sum(ennow), by = c('Date', 'planet')]

  # Merge cumulative planets energy.
  setnames(dailyAspectsCumulativeEnergy, c('Date', 'p.x', 'encum.x'))
  dailyAspects <- merge(dailyAspects, dailyAspectsCumulativeEnergy, by = c('Date', 'p.x'))
  setnames(dailyAspectsCumulativeEnergy, c('Date', 'p.y', 'encum.y'))
  dailyAspects <- merge(dailyAspects, dailyAspectsCumulativeEnergy, by = c('Date', 'p.y'))
  dailyAspects[, entot := round((encum.x + encum.y) * ennow, 0)]
  dailyAspects[, effect := round(((diffMean) * entot) * 100)]
  #dailyAspects[, effect := round((diffMedian * entot) * 100)]

  # Daily aspects effect index.
  dailyAspectsIndex <- dailyAspects[, sum(effect), by = c('Date')]
  dailyAspectsIndex[, diff := round(Delt(V1, k = 1), 2)]
  setnames(dailyAspectsIndex, c('Date', 'effect', 'diff'))

  # Set more convenient order for analysis.
  colsOrder <- c('Date', 'origin', 'p.x', 'lon.x', 'sp.x', 'spn.x', 'p.y', 'lon.y', 'sp.y', 'spn.y', 'aspect', 'type', 'orb', 'orbdir', 'enmax', 'ennow', 'encum.x', 'encum.y', 'entot', 'effect', 'diffMean', 'diffMedian')
  setcolorder(dailyAspects, colsOrder)

  cat("Today aspects:", format(todayDate, "%Y-%m-%d"), "\n")
  print(dailyAspects[Date == todayDate,][order(-entot)])
  cat("\n")

  cat("Tomorrow aspects:", format(todayDate + 1, "%Y-%m-%d"), "\n")
  print(dailyAspects[Date == todayDate + 1,][order(-entot)])
  cat("\n")

  cat("Past tomorrow aspects:", format(todayDate + 2, "%Y-%m-%d"), "\n")
  print(dailyAspects[Date == todayDate + 2,][order(-entot)])
  cat("\n")

  cat("Daily aspects effect index:\n")
  print(dailyAspectsIndex[Date > todayDate-8 ,][0:30])
  cat("\n")

  # Summary of price moves.
  cat("Analysis for symbol: ", symbol, " last date: ", format(max(security$Date), "%Y-%m-%d"), "\n")
  securityLastDay <- security[Date == todayDate,]
  cat("Last day priceDiffPercent:\n")
  print(securityLastDay)

  cat("1 day daily price diff percentages\n")
  print(summary(abs(Delt(security$Mid, k = 1))))

  cat("3 day daily price diff percentages:\n")
  print(summary(abs(Delt(security$Mid, k = 3))))

  #cat("Relevant peak dates:\n")
  datesHighs <- security$Date[peaks(security$Mid, span)]
  datesLows <- security$Date[peaks(-security$Mid, span)]

  # Indicator / Price charts.
  # p1 <- uranusIndicators()
  # p2 <- drawSecurityPriceSerie()
  # grid.newpage()
  # grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
  # drawSlowIndicators()
  # securityPeaksValleys(security)

  return(dailyAspectsPriceResearch)
}

