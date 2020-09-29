# Title     : Single row price/aspects (column aspect/planet cumulative energy) per day.
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

# Filter the extreme outliers.
cat(paste("Original days rows: ", nrow(securityData)), "\n")
securityData <- securityData[zdiffPercent < 3 & zdiffPercent > -3,]
securityData <- securityData[zdiffPercent > 2.5, zdiffPercent := 2.5]
securityData <- securityData[zdiffPercent < -2.5, zdiffPercent := -2.5]
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
  finalCorrelations[finalCorrelations > 0.02 & finalCorrelations < 0.9]
)

sellVarNames <- names(
  finalCorrelations[finalCorrelations < -0.02]
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
  "Date", names(finalCorrelations)[c(seq(1, 10), seq(totalCols-10, totalCols-1))]
))

modelSearch <- glmulti(
  y = "zdiffPercent",
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
aspectViewValidate <- merge(securityDataTest[, c('Date', 'zdiffPercent')], aspectViewValidate, by = "Date")
aspectViewValidate[, c('Date', 'zdiffPercent', 'diffPredict')]
plot(aspectViewValidate$zdiffPercent, aspectViewValidate$diffPredict)
cor(aspectViewValidate$zdiffPercent, aspectViewValidate$diffPredict) %>% print()
with(aspectViewValidate, mean((zdiffPercent - diffPredict)^2)) %>% sqrt()
#plot(aspectViewValidate$a180_SU, type = "l")
#fwrite(aspectView, paste("~/Desktop/", symbol, "cumenergy.csv", sep = "-"))

# CONCLUSIONS:
# - The individual planet/aspect variables correlate as high as +/- 0.10 with price diff.
# - Aggregating positive correlated column variables with higher correlation of 0.02 results in
#   new variable "buy" correlated around 0.22 with price diff.
# - The opposite happens when aggregating negative correlated variables in new "sell" variable.
# - Calculating the diff or buy and sell results in buypower and opposite calculation results in sellpower
#   this variables has a 0.27 correlation with price diff.
# - Unfortunately when modeling using the aspect/planet energy variables and the buy/sell aggregated ones
#   results in very poor variance explained of only R2 = 0.10 and very noisy fit with 0.03 correlation
#   on test data prediction.
# - Trying to adjust the orb and energy decay speed of used aspects don't provide relevant improvements.
# - Filtering the fast aspects MO and ME improves the prediction correlation to 0.14 and keep explained
#   variance near to R2 = 0.10 but still the fit is very poor.
# - Filtering p.x former planet aspects to different subsets don't resulted in any relevant improvement.
# - Normalizing and centering diffPrice with zcores (scale) helped to increase test data prediction correlation
#   from 0.14 to 0.21 due the high skew of pricess diff cause issues.
# - Filtering the most extreme diff price observations don't improved the test data price diff correlation
#   it was reduced from 0.21 to 0.20 using zcore +/- 3 as the filter threshold.
# - Reducing use of max of 20 most correlated varas improved test price diff predict correlation to 0.24
#   but reduce the explained variance of the model to R2 = 0.075.
# - Replacing the filtering of extreme values by trimming to zscore +/- 3 decreased test predict correlation to 0.16.
# - Filtering extreme price diff values zscore +/- 3 with trimming to 2.5 produces sames results as just filtering.

# NEXT STEPS:
# - Limit the target p.y (slow) planet aspects.
# - Limit few p.x (fast) planets agains few p.x planets.
# - Experiment different orbs.
# - Experiment different planet weights.
# - Explore logistic regression using price diff 5 quartiles.
# - Aggregate energy by aspects and by planets using linear model.
# - Experiment cumulative p.x - p.y cumulative aspect count differences similar as used in ModelLA.