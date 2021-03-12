# Title     : Planets aspects energy index explored varitions:
#             1) No smoothing
#             2) ME in px planets (not much difference).
#             3) ME, asteroids and SN (sometimes align better, others worst, CE, VS and CH seems relevant).
#             4) Minor aspects: quintile, septile (enhance some turning points transitions).
#             5) Slower energy decay speed: 0.5 (not much difference).
#             6) Faster energy decay speed: 0.7 (not much difference).
#             7) Reduced orb to 2 degrees
#             8) Modern aspect set7 with quintile, septile and orbs according to harmonics.

library(caret)
library(magrittr)
library(psych)
library(plyr)
library(rattle)
library(gvlma)
library(arm)
library(glmulti)
library(stringr)

source("./analysis.r")
source("./indicatorPlots.r")

modelId <- "LFA"

prepareHourlyAspectsModelLF <- function() {
  idCols <- c('Date', 'Hour')
  #setClassicAspectsSet6()
  setModernAspectsSet7()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  #setPlanetsMOMEVESUMACEVSJUNNSAURCHNEPL()
  hourlyPlanets <- openHourlyPlanets('planets_12', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols)
  # Filter aspects within 2 degrees of orb for cumulative aspects count.
  #dailyAspects$filter <- F
  #dailyAspects <- dailyAspects[p.x %in% c('MO', 'ME'), filter := T]
  #dailyAspects <- dailyAspects[orb >= 4, filter := T]
  #dailyAspects <- dailyAspects[filter == F,]
  dailyAspects <- dailyApects[p.x %ni% c('MO'),]
  dailyAspects <- dailyAspectsAddEnergy(dailyAspects, 0.6)
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

dailyAspectPlanetCumulativeEnergy <- prepareHourlyAspectsModelLF()

trainModel <- function(symbol) {
  cat("Training", symbol, "model\n")
  securityData <- mainOpenSecurity(
    symbol, 14, 28, "%Y-%m-%d",
    "2010-01-01", "2020-06-30"
  )

  # Filter the extreme outliers.
  cat(paste("Original days rows: ", nrow(securityData)), "\n")
  securityData <- securityData[zdiffPercent < 3 & zdiffPercent > -3,]
  hist(securityData$zdiffPercent)
  cat(paste("Total days rows: ", nrow(securityData)), "\n")

  aspectView <- merge(
    securityData[, c('Date', 'zdiffPercent')],
    dailyAspectPlanetCumulativeEnergy,
    by = "Date"
  )

  #aspectView[, zdiffPercent := abs(zdiffPercent)]
  varCorrelations <- aspectView[, -c('Date')] %>%
    cor() %>%
    round(digits = 2)
  finalCorrelations <- sort(varCorrelations[, 1])
  print(finalCorrelations)

  buyVarNames <- names(
    finalCorrelations[finalCorrelations > 0.03 & finalCorrelations < 0.9]
  )

  sellVarNames <- names(
    finalCorrelations[finalCorrelations < -0.03]
  )

  varCorrelations <- aspectView[, -c('Date')] %>%
    cor() %>%
    round(digits = 2)
  finalCorrelations <- sort(varCorrelations[, 1])
  print(finalCorrelations)

  totalCols <- count(finalCorrelations)
  selectCols <- unique(c(
    "Date", names(finalCorrelations)[c(seq(1, 15), seq(totalCols - 15, totalCols - 1))]
  ))

  modelSearch <- glmulti(
    y = "zdiffPercent",
    xr = selectCols[-1],
    data = aspectView,
    #exclude=c("sp.y", "sp.x", "dc.x", "dc.y"),
    #minsize = 15,
    level = 1, marginality = F, intercept = T, crit = "aicc",
    method = "g", plotty = F,
    popsize = 200
    #mutrate = 0.01, sexrate = 0.1, imm = 0.1,
  )

  plot(modelSearch, type = "s")
  print(modelSearch@objects[[1]]$formula)

  # Review the best fit.
  modelFit <- lm(
    modelSearch@objects[[1]]$formula,
    data = aspectView
  )

  modelFit %>% summary() %>% print()
  modelFit %>% plot()
  modelFit %>% coefplot()

  # Validate with reserved data.
  securityDataTest <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2020-07-01")
  aspectViewValidate <- dailyAspectPlanetCumulativeEnergy[, ..selectCols]
  aspectViewValidate$diffPredict <- predict(modelFit, aspectViewValidate)
  aspectViewValidate$diffPredictSmooth <- aspectViewValidate$diffPredict
  aspectViewValidate[, Signal := round(normalize(diffPredict * 100) * 100)]
  signalString <- aspectViewValidate[Date >= as.Date("2018-01-01")]$Signal %>%
    round(digits = 2) %>%
    str_flatten(collapse = ",")
  signalData <- paste0('string ', str_replace(symbol, '-USD', ''), ' = "', signalString, '"')
  symbolSignalExport(signalData, symbol)

  # Dsiplay projected prediction in chart
  energyPlot <- ggplot(data = aspectViewValidate[Date >= Sys.Date() - 150,]) +
    geom_line(aes(x = Date, y = diffPredictSmooth), colour = "black", alpha = 0.7) +
    scale_x_date(date_breaks = "2 days", date_labels = "%Y-%m-%d") +
    labs(title = paste(symbol, "planets energy index", modelId)) +
    theme(axis.text.x = element_text(angle = 90, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank())
  print(energyPlot)
  aspectViewValidate <- merge(securityDataTest[, c('Date', 'zdiffPercent')], aspectViewValidate, by = "Date")
  aspectViewValidate[, c('Date', 'zdiffPercent', 'diffPredict')]
  plot(aspectViewValidate$zdiffPercent, aspectViewValidate$diffPredict)
  cor(aspectViewValidate$zdiffPercent, aspectViewValidate$diffPredict) %>% print()
  with(aspectViewValidate, mean((zdiffPercent - diffPredict)^2)) %>% sqrt()
  #plot(aspectViewValidate$a180_SU, type = "l")
  #fwrite(aspectView, paste("~/Desktop/", symbol, "cumenergy.csv", sep = "-"))
  return(signalData)
}

listFilePath <- npath(paste("~/Sites/own/astro-trading/hisdata/symbols/working.csv", sep = ""))
symbolsList <- read.csv(listFilePath, header = F, stringsAsFactors = F)
allSignals <- lapply(symbolsList$V1, trainModel)
