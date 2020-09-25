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

dailyAspects <- prepareHourlyAspectsModelK()
symbol <- "LINK-USD"
securityData <- mainOpenSecurity(
  symbol, 14, 28, "%Y-%m-%d",
  "2010-01-01", "2020-08-31"
)

aspectsX <- paste("a", aspects, ".x", sep = "")
selectCols <- c(
  "Date",
  aspectsX,
  "sp.y", "sp.x", "dc.y", "dc.x",
  "MO", "SU", "ME", "VE", "MA", "JU", "SA", "NE", "UR.y", "PL"
  #"zx", "zy", "aspect", "spd", "spp", "acx", "acy", "agt",
)

# Evaluate polarity effect on SU90 aspect.
aspectViewRaw <- dailyAspects[p.x != "MO" & aspect == 90]
aspectView <- aspectViewRaw[, ..selectCols]
aspectView <- merge(securityData[, c('Date', 'diffPercent')], aspectView, by = "Date")
trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.80, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewTest <- aspectView[-trainIndex,]

modelSearch <- glmulti(
  y = "diffPercent",
  xr = c(
    "sp.y", "sp.x", "dc.x", "dc.y",
    "a45.x", "a60.x", "a90.x", "a120.x", "a135.x",
    "ME", "MA", "JU", "SA", "UR.y", "PL" # R2 = 0.38 - SIGNIFICANT
    #"MO", "SU", "ME", "VE", "MA", "JU", "SA", "UR.y", "NE", "PL" # R2 = 0.38
  ),
  data = aspectViewTrain,
  exclude=c("sp.y", "sp.x", "dc.x", "dc.y"),
  level = 2, marginality = F, intercept = F, crit = "aicc",
  method = "g", plotty = F,
  popsize = 500, mutrate = 0.01, sexrate = 0.1, imm = 0.1,
)

# plot(modelSearch, type = "s")
summary(modelSearch@objects[[1]])

# Review the best fit.
modelFit <- lm(
  modelSearch@objects[[1]]$formula,
  data = aspectView
)

modelFit %>% summary()
modelFit %>% plot()
modelFit %>% coefplot()

# Validate with test subset.
aspectViewTest$diffPredict <- predict(modelFit, aspectViewTest)
aspectViewTest[, c('Date', 'diffPercent', 'diffPredict')]

# Validate with reserved data.
securityDataTest <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2020-09-01")
aspectViewValidate <- aspectViewRaw[, ..selectCols]
aspectViewValidate <- merge(securityDataTest[, c('Date', 'diffPercent')], aspectViewValidate, by = "Date")
aspectViewValidate$diffPredict <- predict(modelFit, aspectViewValidate)
aspectViewValidate[, c('Date', 'diffPercent', 'diffPredict')]
