library(grid)
library(cowplot)
library(dismo)
library(paramtest)
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
  dailySpeed <- melt(
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
  dailyDeclination <- melt(
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
  dailyAspectsOrbs <- melt(
    dailyPlanets, id.var = idCols, variable.name = 'origin',
    value.name = 'orb', measure.var = planetsCombOrb
  )

  dailyAspectsOrbs[, orb := round(orb, 2)]
  dailyAspectsOrbs[, origin := substr(origin, 1, 4)]
  # Join aspects & orbs.
  dailyAspects <- merge(dailyAspects, dailyAspectsOrbs, by = c(idCols, 'origin'))
}

dailyAspectsAddOrbsDir <- function(dailyAspects) {
  # Calculate orb direction (applicative, separative).
  dailyAspects[, orbdir := round(orb - Lag(orb), 2), by = c('origin', 'aspect')]
  dailyAspects[, type := cut(orbdir, c(-100, 0, 100), labels = (c('A', 'S')))]
}

dailyAspectsAddLongitude <- function(dailyAspects, dailyPlanets, idCols = c('Date')) {
  # Melt longitudes.
  dailyLongitudes <- melt(dailyPlanets, id.var = idCols, variable.name = 'origin',
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

dailyAspectsAddAspectsCount <- function(dailyAspects) {
  aspCols <- paste("a", aspects, sep = "")
  # Calculate the proportion of positive / negative daily aspects.
  dailyPlanetAspects <- melt(
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
    value.var = "N"
  )
  setDT(dailyAspectsPlanetCountWide)

  # Merge aspects count once per each former planets.
  setnames(dailyAspectsPlanetCountWide, c('Date', 'p.x', paste(aspCols, 'x', sep = ".")))
  dailyAspects <- merge(dailyAspects, dailyAspectsPlanetCountWide, by = c("Date", "p.x"))
  aspColsX <- paste(aspCols, 'x', sep = ".")
  dailyAspects[, c(aspColsX) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = aspColsX]

  setnames(dailyAspectsPlanetCountWide, c('Date', 'p.y', paste(aspCols, 'y', sep = ".")))
  dailyAspects <- merge(dailyAspects, dailyAspectsPlanetCountWide, by = c("Date", "p.y"))
  aspColsY <- paste(aspCols, 'y', sep = ".")
  dailyAspects[, c(aspColsY) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = aspColsY]

  # Daily total aspects count.
  dailyAspectsCount <- dailyPlanetAspects[, data.table(table(aspect)), by = list(Date)]
  dailyAspectsCount[, aspect := paste("a", aspect, sep = "")]
  dailyAspectsCountWide <- dcast(
    dailyAspectsCount,
    Date ~ factor(aspect, levels = aspCols),
    value.var = "N"
  )
  setDT(dailyAspectsCountWide)
  setnames(dailyAspectsCountWide, c('Date', paste(aspCols, 'g', sep = ".")))
  dailyAspects <- merge(dailyAspects, dailyAspectsCountWide, by = c("Date"))
  aspColsT <- paste(aspCols, 'g', sep = ".")
  dailyAspects[, c(aspColsT) := lapply(.SD, function(x) ifelse(is.na(x), 0, x)), .SDcols = aspColsT]

  return(dailyAspects)
}

dailyAspectsAddEnergy <- function(dailyAspects, speedDecay = 0.6) {
  aspectsEnergyIndex <- matrix(
    aspectsEnergy, nrow = 1, ncol = length(aspectsEnergy),
    byrow = T, dimnames = list(c('energy'), aspects)
  )

  # Calculate max and proportional energy.
  dailyAspects[, enmax := aspectsEnergyIndex['energy', as.character(aspect)]]
  dailyAspects[, ennow := energyDecay(enmax, orb, speedDecay)]

  return(dailyAspects)
}

dailyAspectsAddEnergy2 <- function(dailyAspects, speedDecay = 0.6, aspectsEnergyCustom) {
  aspectsEnergyIndex <- matrix(
    aspectsEnergyCustom, nrow = 1, ncol = length(aspectsEnergyCustom),
    byrow = T, dimnames = list(c('energy'), aspects)
  )

  # Calculate max and proportional energy.
  dailyAspects[, enmax := aspectsEnergyIndex['energy', as.character(aspect)]]
  dailyAspects[, ennow := enmax]

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
  dailyAspectsPlanetsEnergy <- melt(
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
  dailyAspectsIndexProjected <- dailyAspectsIndex[Date > todayDate - 8,][0:60]
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
    scale_x_date(date_breaks = '3 days', date_labels = "%Y-%m-%d") +
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

dailyHourlyAspectsTablePrepare <- function(hourlyPlanets, idCols) {
  hourlyPlanetsRange <- hourlyPlanets[Date >= as.Date("2017-01-01") & Date <= as.Date("2021-06-30")]
  # Melt aspects.
  hourlyAspects <- melt(
    hourlyPlanetsRange, id.var = idCols,
    variable.name = 'origin', value.name = 'aspect',
    value.factor = T, measure.var = planetsCombAsp, na.rm = T
  )

  hourlyAspects[, origin := substr(origin, 1, 4)]
  setkey(hourlyAspects, 'Date', 'Hour')

  hourlyAspects <- dailyAspectsAddOrbs(hourlyAspects, hourlyPlanetsRange, idCols)
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
  dailyAspects <- dailyAspects[, spr := sp.x / sp.y]
  # Declination x/y diff, product and ratio.
  dailyAspects <- dailyAspects[, dcd := dc.x - dc.y]
  dailyAspects <- dailyAspects[, dcp := dc.x * dc.y]
  dailyAspects <- dailyAspects[, dcr := dc.x / dc.y]

  # Filter aspects within 2 degrees of orb for cumulative aspects count.
  dailyAspects <- dailyAspects[orb <= 1,]
  dailyAspects <- dailyAspectsAddAspectsCount(dailyAspects)

  # Aggregate X/Y aspects count by type.
  dailyAspects[, a0 := a0.x + a0.y]
  dailyAspects[, a30 := a30.x + a30.y]
  dailyAspects[, a45 := a45.x + a45.y]
  dailyAspects[, a60 := a60.x + a60.y]
  dailyAspects[, a90 := a90.x + a90.y]
  dailyAspects[, a120 := a120.x + a120.y]
  dailyAspects[, a135 := a135.x + a135.y]
  dailyAspects[, a150 := a150.x + a150.y]
  dailyAspects[, a180 := a180.x + a180.y]

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

  # Add week day.
  dailyAspects[, wd := as.numeric(format(Date, "%w")) + 1]
  # Add zodiacal signs.
  dailyAspects[, zx := ceiling((lon.x + 0.01) / 30)]
  dailyAspects[, zy := ceiling((lon.y + 0.01) / 30)]

  # Remove other redundant cols.
  filterCols <- c('orbdir', 'p.y')
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
  # crossValidateModelReport("modelH1A", dailyAspectsIndex, security)

  return(medianFit)
}
