# Title     : Fit multi linear aspect modelK.
# Created by: pablocc
# Created on: 25/09/2020

library(caret)
library(magrittr)
library(psych)
library(plyr)
library(rattle)
library(gvlma)
library(arm)
library(glmulti)
source("./indicatorPlots.r")

dailyAspects <- prepareHourlyAspectsModelL()
symbol <- "LINK-USD"
securityData <- mainOpenSecurity(
  symbol, 14, 28, "%Y-%m-%d",
  "2010-01-01", "2020-06-30"
)

aspectsT <- paste("a", aspects, sep = "")
aspectsX <- paste("a", aspects, ".x", sep = "")
aspectsY <- paste("a", aspects, ".y", sep = "")
aspectsG <- paste("a", aspects, ".g", sep = "")
aspectsAll <- c(aspectsT, aspectsX, aspectsY, aspectsG)
planetsAll <- c(
  "ME.x", "VE.x", "SU.x", "MA.x", "JU.x", "NN.x", "SA.x", "UR.x",
  "MO", "SU", "ME", "VE", "MA", "JU", "SA", "NE", "PL",
  "ME.y", "VE.y", "SU.y", "MA.y", "JU.y", "NN.y", "SA.y", "UR.y"
)
#dailyAspectsNorm <- dailyAspects[, c(planetsAll) := lapply(.SD, function(x) ifelse(x > 0, 1, 0)), .SDcols = planetsAll]

selectCols <- c(
  "Date",
  aspectsAll,
  planetsAll,
  "sp.x", "spi.x", "sp.y", "spi.y",
  "dc.y", "dc.x",
  "retx", "rety",
  "zx", "zy",
  "spd", "spdi", "spp", "spr", "spri",
  "dcd", "dcdi", "dcp", "dcr", "dcri",
  "acx", "acy", "agt",
  "orb", "apl", "sep", "wd"
)

# Fit a90 aspects model.
aspectViewRaw <- dailyAspects[p.x == "SU" & aspect == 120]
aspectView <- aspectViewRaw[, ..selectCols]
aspectView <- merge(securityData[, c('Date', 'diffPercent')], aspectView, by = "Date")
# trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.80, list = FALSE)

modelSearch <- glmulti(
  y = "diffPercent",
  xr = c(
    aspectsT,
    #aspectsX,
    #aspectsY,
    "orb",
    #"apl", "sep",
    "spd", "spdi",
    "dcd", "dcdi",
    #"dcp", "dcr", "dcri",
    #"dcr", "dcri",
    "retx", "rety"
    #"MO"
    #"sp.x", "sp.y",
    #"spi.x", "spi.y"
    # "dc.y", "dc.x"
    # "zx", "zy", "wd"
    # Adding planets to the model increase R2 but overfit the model and unstabilize predictions.
    # "MO", "SU", "ME", "VE", "MA", "JU", "SA", "NE", "NN.x", "NN.y", "PL"
    # "MO", "SU", "ME", "VE", "MA", "JU", "SA", "NE", "PL"
    # "ME.x", "VE.x", "SU.x", "MA.x", "JU.x", "NN.x", "SA.x", "UR.x"
    # "ME.y", "VE.y", "SU.y", "MA.y", "JU.y", "NN.y", "SA.y", "UR.y"
  ),
  data = aspectView,
  #exclude=c("sp.y", "sp.x", "dc.x", "dc.y"),
  #minsize = 15,
  level = 1, marginality = F, intercept = T, crit = "aicc",
  method = "g", plotty = F,
  popsize = 300
  #mutrate = 0.01, sexrate = 0.1, imm = 0.1,
)

plot(modelSearch, type = "s")
summary(modelSearch@objects[[1]])

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
aspectViewValidate <- aspectViewRaw[, ..selectCols]
aspectViewValidate$diffPredict <- predict(modelFit, aspectViewValidate)
aspectViewValidate <- merge(securityDataTest[, c('Date', 'diffPercent')], aspectViewValidate, by = "Date")
aspectViewValidate[, c('Date', 'diffPercent', 'diffPredict')]
cor(aspectViewValidate$diffPercent, aspectViewValidate$diffPredict) %>% print()
with(aspectViewValidate, mean((diffPercent - diffPredict)^2)) %>% sqrt()

# CONCLUSION: The about 30% of the variance of aspect polarity is explained by
# the difference of cumulative aspects from X/Y former planets,
# the difference on speed and declination, retrograde motion and aspect orb.
# Is very likely that the explained variance could be explained using the effect decay
# instead of simple boolean flag that account for aspect active or not.
# The order of feature importance is: orb, aspects, speed diff, retrograde and declination diff.

# Specific SU90 aspect test
# orb 0.2-2 BIC / MSE = 0.061, a120: 0.045, a135: 0.076, a150: NA, a180: 0.149
# orb 0.2-2 AIC / MSE = 0.042, a120: 0.055, a135: 0.082, a150: 0.058, a180: 0.164
# orb 0.2-2 AICC / MSE = 0.065, a120: 0.050, a135: 0.076, a150: 0.058, a180: 0.139
# orb 0.2-1 AICC / MSE = 0.019, a120: 0.060, a135: 0.085, a150: 0.054, a180: 0.213
# orb 0.1-2 AICC / MSE = 0.012, a120: 0.051, a135: 0.073, a150: 0.058, a180: 0.077

# Specific aspect test
# SU120 - Using spd and dcd = Cor: 0.11 MSE: 0.051
# SU120 - Not using spd and dcd = Cor: 0.54 MSE: 0.041
# SU120 - Not using spd and dcd and using MO = Cor: 0.62 MSE: 0.039
# SU90 - Not using spd and dcd and using MO = Cor: -0.95 MSE: 0.064
