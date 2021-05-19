# Title     : Planets aspects energy index explored varitions:
#             1) Model LFA with KKNN regression.
#             2) Use all features with correlation of 0.02 or more.

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
  dailyAspects <- dailyAspects[p.x %ni% c('MO'),]
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
    finalCorrelations[finalCorrelations > 0.02 & finalCorrelations < 0.9]
  )

  sellVarNames <- names(
    finalCorrelations[finalCorrelations < -0.02]
  )

  selectFeatures <- c(buyVarNames, sellVarNames)
  selectColsTrain <- unique(c("zdiffPercent", selectFeatures))
  cat("Selected features: ", selectColsTrain, "\n")

  control <- trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 5,
    savePredictions = "all",
    verboseIter = T,
    allowParallel = T,
    trim = F
  )

  modelFit <- train(
    formula(zdiffPercent ~ .),
    data = aspectView[, ..selectColsTrain],
    method = "kknn",
    metric = "Rsquared",
    trControl = control,
    tuneGrid = expand.grid(
      kmax = 7,
      distance = 2,
      kernel = "optimal"
    )
  )

  modelFit %>% print()
  modelFit %>% summary() %>% print()

  # Validate with reserved data.
  securityDataTest <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2020-07-01")
  selectColsValidate <- unique(c("Date", selectFeatures))
  aspectViewValidate <- dailyAspectPlanetCumulativeEnergy[, ..selectColsValidate]
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
  return(signalData)
}

listFilePath <- npath(paste("./symbols/working.csv", sep = ""))
symbolsList <- read.csv(listFilePath, header = F, stringsAsFactors = F)
allSignals <- lapply(symbolsList$V1, trainModel)
