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

dailyAspects <- prepareHourlyAspectsModelL()
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
aspectViewRaw <- dailyAspects[p.x %in% c('SU')]
aspectView <- aspectViewRaw[, ..selectCols]
aspectView <- merge(securityData[, c('Date', 'diffPercent')], aspectView, by = "Date")
hist(aspectView$diffPercent)
# trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.80, list = FALSE)

modelSearch <- glmulti(
  y = "diffPercent",
  xr = c(
  #aspectsT,
  aspectsD,
  #aspectsCombined,
  aspectType,
  #aspectsX,
  #aspectsY,
  #"orb",
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
