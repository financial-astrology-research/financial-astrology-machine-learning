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

symbol <- "BNB-USD"
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

totalCols <- length(finalCorrelations)
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
ggplot(data = aspectViewValidate[Date >= Sys.Date() - 150,]) +
  geom_line(aes(x = Date, y = diffPredictSmooth), colour = "black", alpha = 0.7) +
  scale_x_date(date_breaks = "7 days", date_labels = "%Y-%m-%d") +
  labs(title = paste(symbol, "planets energy index")) +
  theme(axis.text.x = element_text(angle = 90, size = 12), axis.title.x = element_blank(), axis.title.y = element_blank())
aspectViewValidate <- merge(securityDataTest[, c('Date', 'zdiffPercent')], aspectViewValidate, by = "Date")
aspectViewValidate[, c('Date', 'zdiffPercent', 'diffPredict')]
plot(aspectViewValidate$zdiffPercent, aspectViewValidate$diffPredict)
cor(aspectViewValidate$zdiffPercent, aspectViewValidate$diffPredict) %>% print()
with(aspectViewValidate, mean((zdiffPercent - diffPredict)^2)) %>% sqrt()
#plot(aspectViewValidate$a180_SU, type = "l")
#fwrite(aspectView, paste("./predictions/", symbol, "cumenergy.csv", sep = "-"))

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
# - Introducing again buy/sell power variables after zscore filter and normalization confirmed that this variables
#   overfit the model and generalize worst, test data predictions correlation fall from 0.24 to 0.06.
# - Incrementing the used daily aspects orbs filter from 4 to 5 or 6 don't produce significant improvements.
# - Decreasing the aspect energy speed decay from 0.6 to 0.3 caused test data prediction correlation fall from 0.24 to 0.07.
# - Decreasing the aspect energy speed decay from 0.6 to 0.59 helps to increase explained variance a bit to 0.074.
# - Remove price diff zscore trimming +/- 2.5 don't produced any effect so keep without it.
# - Increasing the max variables to model to 30 decrease test predict correlation to 0.20 but fit better the
#   qqplot of the model and increase explained R2 to 0.087.
# - Removing few target planets (JU, NE, PL) p.y in aspects don't caused any relevant improvement in the model
#   however, removing others like (VE, MA, PL) cause a decrease in the test predictions correlation.

# NEXT STEPS:
# - Limit the target p.y (slow) planet aspects.
# - Limit few p.x (fast) planets agains few p.x planets.
# - Experiment different orbs.
# - Experiment different planet weights.
# - Aggregate energy by aspects and by planets using linear model.
# - Experiment cumulative p.x - p.y cumulative aspect count differences similar as used in ModelLA.
# - Explore logistic regression using price diff 5 quartiles.
# - Experiment decision tree (basic and RF).
# - Experiment with k-nearest model.
# - Experiment with basic single hidden layer neural network.