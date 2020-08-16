library(grid)
source("./analysis.r")
setClassicAspectsSet()
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
  dailyAspects[, p.x := substr(origin, 1, 2)]
  dailyAspects[, p.y := substr(origin, 3, 4)]

  # Melt longitudes.
  dailyLongitudes <- melt(dailyPlanets, id.var = c('Date'), variable.name = 'origin',
                          value.name = 'lon', measure.var = planetsLonCols)
  dailyLongitudes[, p.x := substr(origin, 1, 2)]
  dailyLongitudes[, p.y := substr(origin, 1, 2)]
  dailyAspects <- merge(dailyAspects, dailyLongitudes[, c('Date', 'p.x', 'lon')], by = c('Date', 'p.x'))
  dailyAspects <- merge(dailyAspects, dailyLongitudes[, c('Date', 'p.y', 'lon')], by = c('Date', 'p.y'))

  # Melt orbs.
  dailyAspectsOrbs <- melt(dailyPlanets, id.var = c('Date'), variable.name = 'origin', value.name = 'orb',
                           measure.var = planetsCombOrb)
  dailyAspectsOrbs[, origin := substr(origin, 1, 4)]
  #aspects.day.long[, orbdir := sign(orb - Lag(orb)), by=c('lon', 'origin', 'aspect')]

  # For aspects: c( 0 , 30 , 45 , 60 , 90 , 120 , 135 , 150 , 180)
  aspectsEnergy <- c(12, 3, 4, 4, 8, 5, 4, 4, 12)
  aspectsEnergyIndex <- matrix(aspectsEnergy, nrow = 1, ncol = length(aspectsEnergy), byrow = T,
                               dimnames = list(c('energy'), aspects))
  print(aspectsEnergyIndex)
  # Join aspects & orbs.
  dailyAspects <- merge(dailyAspects, dailyAspectsOrbs, by = c('Date', 'origin'))

  # Calculate orb direction (applicative, separative).
  dailyAspects[, orbdir := round(orb - Lag(orb), 2), by = c('origin', 'aspect')]
  dailyAspects[, type := cut(orbdir, c(-100, 0, 100), labels = (c('applicative', 'separative')))]

  # Calculate max and proportional energy.
  dailyAspects[, enmax := aspectsEnergyIndex['energy', as.character(aspect)]]
  dailyAspects[, ennow := energyGrowth(enmax, orb, 0.3)]

  # Merge daily security prices with aspects.
  dailyAspectsPriceResearch <- merge(dailyAspects, security[, c('Date', 'diffPercent')], by = c('Date'))

  # Calculate the historical mean aspect effect.
  aspectsEffect <- dailyAspectsPriceResearch[orb <= 2, list(mean(diffPercent), median(diffPercent)), by = c('origin', 'aspect', 'type')]
  setnames(aspectsEffect, c('origin', 'aspect', 'type', 'diffMean', 'diffMedian'))
  #dailyAspects[, diffMean := setAspectEffect(.SD), by=c('Date', 'origin', 'aspect', 'type')]
  dailyAspectsPriceResearch <- merge(dailyAspectsPriceResearch, aspectsEffect, by = c('origin', 'aspect', 'type'))
  dailyAspects <- merge(dailyAspects, aspectsEffect, by = c('origin', 'aspect', 'type'))

  # Set aspect energy column.
  cat("Last day aspects\n")
  print(dailyAspectsPriceResearch[Date == max(Date),][order(-ennow)][0:20])
  cat("\n")

  cat("Today aspects:", format(todayDate, "%Y-%m-%d"), "\n")
  print(dailyAspects[Date == todayDate,][order(-ennow)][0:20])
  cat("\n")

  cat("Tomorrow aspects:", format(todayDate + 1, "%Y-%m-%d"), "\n")
  print(dailyAspects[Date == todayDate + 1,][order(-ennow)][0:20])
  cat("\n")

  cat("Past tomorrow aspects:", format(todayDate + 2, "%Y-%m-%d"), "\n")
  print(dailyAspects[Date == todayDate + 2,][order(-ennow)][0:20])
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
  p1 <- uranusIndicators()
  p2 <- drawSecurityPriceSerie()
  grid.newpage()
  grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
  # drawSlowIndicators()
  # securityPeaksValleys(security)

  return(dailyAspectsPriceResearch)
}

