# Title     : Planets aspects energy index including ME.
# Created by: pablocc
# Created on: 28/09/2020

library(caret)
library(magrittr)
library(psych)
library(plyr)
library(rattle)
library(gvlma)
library(arm)
library(glmulti)
source("./analysis.r")
source("./indicatorPlots.r")

prepareHourlyAspectsModelLF <- function() {
  idCols <- c('Date', 'Hour')
  setClassicAspectsSet6()
  setPlanetsMOMEVESUMAJUNNSAURNEPL()
  #setPlanetsMOMEVESUMACEVSJUNNSAURCHNEPL()
  hourlyPlanets <- openHourlyPlanets('planets_12', clear = F)
  dailyAspects <- dailyHourlyAspectsTablePrepare(hourlyPlanets, idCols)
  # Filter aspects within 2 degrees of orb for cumulative aspects count.
  dailyAspects <- dailyAspects[p.x %ni% c('MO') & orb <= 4,]
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

dailyAspectPlanetCumulativeEnergy <- prepareHourlyAspectsModelLF()

symbol <- "BTC-USD"
securityData <- mainOpenSecurity(
  symbol, 14, 28, "%Y-%m-%d",
  "2010-01-01", "2020-06-30"
)

# Filter the extreme outliers.
cat(paste("Original days rows: ", nrow(securityData)), "\n")
securityData <- securityData[zdiffPercent < 3 & zdiffPercent > -3,]
hist(securityData$zdiffPercent)
cat(paste("Total days rows: ", nrow(securityData)), "\n")

aspectView <- merge( securityData[, c('Date', 'zdiffPercent')],
  dailyAspectPlanetCumulativeEnergy, by = "Date"
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
  "Date", names(finalCorrelations)[c(seq(1, 15), seq(totalCols-15, totalCols-1))]
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

modelFit %>% summary()
modelFit %>% plot()
modelFit %>% coefplot()

# Validate with reserved data.
securityDataTest <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2020-07-01")
aspectViewValidate <- dailyAspectPlanetCumulativeEnergy[, ..selectCols]
aspectViewValidate$diffPredict <- predict(modelFit, aspectViewValidate)
aspectViewValidate$diffPredictSmooth <- SMA(aspectViewValidate$diffPredict, 3)
# Dsiplay projected prediction in chart
ggplot(data = aspectViewValidate[Date >= Sys.Date() - 80,]) +
  geom_line(aes(x = Date, y = diffPredictSmooth), colour = "black", alpha = 0.7) +
  scale_x_date(date_breaks = "7 days", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank())
aspectViewValidate <- merge(securityDataTest[, c('Date', 'zdiffPercent')], aspectViewValidate, by = "Date")
aspectViewValidate[, c('Date', 'zdiffPercent', 'diffPredict')]
plot(aspectViewValidate$zdiffPercent, aspectViewValidate$diffPredict)
cor(aspectViewValidate$zdiffPercent, aspectViewValidate$diffPredict) %>% print()
with(aspectViewValidate, mean((zdiffPercent - diffPredict)^2)) %>% sqrt()
#plot(aspectViewValidate$a180_SU, type = "l")
#fwrite(aspectView, paste("~/Desktop/", symbol, "cumenergy.csv", sep = "-"))
