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

modelSearch <- glmulti(
  y = "diffPercent",
  xr = c(
    "sp.y", "sp.x", "dc.x",
    "a30.x", "a45.x", "a60.x", "a90.x", "a120.x", "a135.x", "a150.x",
    "MA", "JU", "SA", "UR.y", "PL"
    #"MO", "SU", "ME", "VE", "MA", "JU", "SA", "UR.y", "NE", "PL" # R2 = 0.38
  ),
  data = aspectView,
  exclude=c("sp.y", "sp.x", "dc.x", "dc.y"),
  level = 2, marginality = F,intercept = F, crit = "aicc",
  # minK = 0.2, maxK = 0.5,
  minsize = 5, maxsize = 20,
  confsetsize = 100,
  method = "g", plotty = F,
  popsize = 300, mutrate = 0.01, sexrate = 0.1, imm = 0.1,
)

plot(modelSearch, type = "s")
#plot(modelSearch)
top <- weightable(modelSearch)
top <- top[top$aic <= min(top$aic) + 2,]

summary(modelSearch@objects[[1]])

# Review the best fit.
modelFit <- lm(
  modelSearch@objects[[1]]$formula,
  data = aspectView
)

modelFit %>% summary()
modelFit %>% plot()
modelFit %>% coefplot()
