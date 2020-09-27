# Title     : Generalization of aspects model per planet.
# Created by: pablocc
# Created on: 27/09/2020

library(caret)
library(magrittr)
library(psych)
library(plyr)
library(rattle)
library(gvlma)
library(arm)
library(glmulti)
source("./indicatorPlots.r")

dailyAspects <- prepareHourlyAspectsModelLA()
symbol <- "LINK-USD"
securityData <- mainOpenSecurity(
  symbol, 14, 28, "%Y-%m-%d",
  "2010-01-01", "2020-06-30"
)

aspectType <- paste("ast", aspects, sep = "")
aspectsT <- paste("a", aspects, sep = "")
aspectsD <- paste("a", aspects, ".d", sep = "")
aspectsX <- paste("a", aspects, ".x", sep = "")
aspectsY <- paste("a", aspects, ".y", sep = "")
aspectsG <- paste("a", aspects, ".g", sep = "")
aspectsAll <- c(aspectsT, aspectsD, aspectsX, aspectsY, aspectsG)
planetsAll <- c(
  "ME.x", "VE.x", "SU.x", "MA.x", "JU.x", "NN.x", "SA.x", "UR.x",
  "MO", "SU", "ME", "VE", "MA", "JU", "SA", "NE", "PL",
  "ME.y", "VE.y", "SU.y", "MA.y", "JU.y", "NN.y", "SA.y", "UR.y"
)
aspectsCombined <- c(paste("asc", seq(1, 10, by=1), sep=""))
#dailyAspectsNorm <- dailyAspects[, c(planetsAll) := lapply(.SD, function(x) ifelse(x > 0, 1, 0)), .SDcols = planetsAll]

selectCols <- c(
  "Date",
  aspectType,
  aspectsCombined,
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
aspectViewRaw <- dailyAspects[p.x %in% c('SU') & aspect %in% c('0', '90', '120', '150', '180')]
aspectView <- aspectViewRaw[, ..selectCols]
aspectView <- merge(securityData[, c('Date', 'diffPercent')], aspectView, by = "Date")
hist(aspectView$diffPercent)
# trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.80, list = FALSE)

modelSearch <- glmulti(
  y = "diffPercent",
  xr = c(
  #aspectsT
  aspectsD,
  #aspectsCombined,
  aspectType,
  #aspectsX,
  #aspectsY
  "orb"
  #"apl", "sep",
  #"spd", "spdi",
  #"dcd", "dcdi",
  #"dcp", "dcr", "dcri",
  #"dcr", "dcri",
  #"MO", "SU", "ME", "VE", "MA", "JU", "SA", "NE", "PL",
  #"retx", "rety"
  #"MO", "ME"
  #"sp.x", "sp.y",
  #"spi.x", "spi.y"
  #"dc.y", "dc.x"
  # "zx", "zy", "wd"
  # Adding planets to the model increase R2 but overfit the model and unstabilize predictions.
  # "MO", "SU", "ME", "VE", "MA", "JU", "SA", "NE", "NN.x", "NN.y", "PL"
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
aspectViewValidate <- aspectViewRaw[, ..selectCols]
aspectViewValidate$diffPredict <- predict(modelFit, aspectViewValidate)
aspectViewValidate <- merge(securityDataTest[, c('Date', 'diffPercent')], aspectViewValidate, by = "Date")
aspectViewValidate[, c('Date', 'diffPercent', 'diffPredict')]
plot(aspectViewValidate$diffPercent, aspectViewValidate$diffPredict)
cor(aspectViewValidate$diffPercent, aspectViewValidate$diffPredict) %>% print()
with(aspectViewValidate, mean((diffPercent - diffPredict)^2)) %>% sqrt()

# CONCLUSION:
# - Generalized SU aspects models have demonstrated that price change effect variance
#   is explained at R2 = 0.097 and similar results are found for ME, MA, JU. The only exception
#   was VE that none of the aspects can explain the variance so I'm concluding that there is no
#   influence of VE on the explored security.
# - When removed NN resulted in decreased prediction accuracy of 0.01 and correlation
#   decrease of 0.22.
# - MO aspects don't fit due the fact that during a day there are too many that make imposible
#   differentiate between multiples aspects effect using daily security price data.
# - Only major aspects 0, 90, 120, 150 and 180 have strong effect in order to identify the effect signal,
#   however the minor ones contribute indirectly to the force of the primary aspects.
# - Part of the variance can be explained for major aspects isolated by planet: SU, ME, MA, JU.
# - When a single aspect is isolated i.e. a90 for all planets except MO, VE, variance cannot be explained.