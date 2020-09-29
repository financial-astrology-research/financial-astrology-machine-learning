# Title     : Single row price/aspects per day modeling.
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

dailyAspectPlanetCumulativeEnergy <- prepareHourlyAspectsModelLC()

symbol <- "LINK-USD"
securityData <- mainOpenSecurity(
  symbol, 14, 28, "%Y-%m-%d",
  "2010-01-01", "2020-06-30"
)

aspectView <- merge( securityData[, c('Date', 'diffPercent')],
  dailyAspectPlanetCumulativeEnergy, by = "Date"
)

#aspectView[, diffPercent := abs(diffPercent)]
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

aspectView[, buy := rowSums(.SD), .SDcols=buyVarNames]
aspectView[, sell := rowSums(.SD), .SDcols=sellVarNames]
aspectView[, buypow := buy - sell]
aspectView[, sellpow := sell - buy]
dailyAspectPlanetCumulativeEnergy[, buy := rowSums(.SD), .SDcols=buyVarNames]
dailyAspectPlanetCumulativeEnergy[, sell := rowSums(.SD), .SDcols=sellVarNames]
dailyAspectPlanetCumulativeEnergy[, buypow := buy - sell]
dailyAspectPlanetCumulativeEnergy[, sellpow := sell - buy]

varCorrelations <- aspectView[, -c('Date')] %>%
  cor() %>%
  round(digits = 2)
finalCorrelations <- sort(varCorrelations[, 1])
print(finalCorrelations)

totalCols <- count(finalCorrelations)
selectCols <- unique(c(
  "Date", names(finalCorrelations)[c(seq(1, 15), seq(totalCols-15, totalCols-1))]
))
#selectCols <- c("Date", "buy", "sell", "buypow", "sellpow")
modelSearch <- glmulti(
  y = "diffPercent",
  xr = selectCols[-1],
  data = aspectView,
  #exclude=c("sp.y", "sp.x", "dc.x", "dc.y"),
  #minsize = 15,
  level = 1, marginality = F, intercept = T, crit = "aicc",
  method = "g", plotty = F,
  popsize = 300
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
  theme(axis.text.x = element_text(angle = 90, size = 10), axis.title.x = element_blank(), axis.title.y = element_blank())
aspectViewValidate <- merge(securityDataTest[, c('Date', 'diffPercent')], aspectViewValidate, by = "Date")
aspectViewValidate[, c('Date', 'diffPercent', 'diffPredict')]
plot(aspectViewValidate$diffPercent, aspectViewValidate$diffPredict)
cor(aspectViewValidate$diffPercent, aspectViewValidate$diffPredict) %>% print()
with(aspectViewValidate, mean((diffPercent - diffPredict)^2)) %>% sqrt()
#plot(aspectViewValidate$a180_SU, type = "l")
#fwrite(aspectView, paste("~/Desktop/", symbol, "cumenergy.csv", sep = "-"))
