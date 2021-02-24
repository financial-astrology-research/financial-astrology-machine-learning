# Title     : Aspects harmonics oscillator price estimation model.
# Created by: pablocc
# Created on: 24/02/2021

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

idCols <- c('Date', 'Hour')
setClassicAspectsSet6()
setPlanetsMOMEVESUMAJUNNSAURNEPL()
hourlyPlanets <- openHourlyPlanets('planets_11', clear = F)
dailyPlanets <- hourlyPlanets[,
  lapply(.SD, mean), .SDcols = planetsCombLon, by = "Date"
]

planetsCombLonH2 <- paste0(planetsCombLon, 'H2')
dailyPlanets[,
   c(planetsCombLonH2) :=
    lapply(.SD, function(x) distanceHarmonic(x,  2)), .SDcols = planetsCombLon
]

planetsCombLonH4 <- paste0(planetsCombLon, 'H4')
dailyPlanets[,
  c(planetsCombLonH4) :=
    lapply(.SD, function(x) distanceHarmonicAbs(x,  4)), .SDcols = planetsCombLon
]

planetsCombLonH6 <- paste0(planetsCombLon, 'H6')
dailyPlanets[,
  c(planetsCombLonH6) :=
    lapply(.SD, function(x) distanceHarmonicAbs(x,  6)), .SDcols = planetsCombLon
]

planetsCombLonH8 <- paste0(planetsCombLon, 'H8')
dailyPlanets[,
  c(planetsCombLonH8) :=
    lapply(.SD, function(x) distanceHarmonicAbs(x,  8)), .SDcols = planetsCombLon
]

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

selectCols <- c('Date', planetsCombLonH4, planetsCombLonH6, planetsCombLonH8)
aspectView <- merge(
  securityData[, c('Date', 'zdiffPercent')],
  dailyPlanets[, ..selectCols],
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

#aspectView[, buy := rowSums(.SD), .SDcols=buyVarNames]
#aspectView[, sell := rowSums(.SD), .SDcols=sellVarNames]
#aspectView[, buypow := buy - sell]
#aspectView[, sellpow := sell - buy]
#dailyAspectPlanetCumulativeEnergy[, buy := rowSums(.SD), .SDcols=buyVarNames]
#dailyAspectPlanetCumulativeEnergy[, sell := rowSums(.SD), .SDcols=sellVarNames]
#dailyAspectPlanetCumulativeEnergy[, buypow := buy - sell]
#dailyAspectPlanetCumulativeEnergy[, sellpow := sell - buy]
#plot(aspectView$buypow, aspectView$zdiffPercent)
#plot(aspectView$sellpow, aspectView$zdiffPercent)

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
dailyPlanets[, Date := as.Date(Date)]
aspectViewValidate <- dailyPlanets[Date >= Sys.Date() - 100 & Date <= Sys.Date() + 200, ..selectCols]
aspectViewValidate$diffPredict <- predict(modelFit, aspectViewValidate)
aspectViewValidate$diffPredictSmooth <- SMA(aspectViewValidate$diffPredict, 2)
# Dsiplay projected prediction in chart
ggplot(data = aspectViewValidate) +
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

