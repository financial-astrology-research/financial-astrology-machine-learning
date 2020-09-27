# Title     : Generalized aspect per planet with cumulative energy diff instead of simple counts.
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
source("./analysis.r")
source("./indicatorPlots.r")

dailyAspects <- prepareHourlyAspectsModelLB()
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
aspectsAll <- c(aspectsT, aspectsD, aspectsX, aspectsY)
#dailyAspectsNorm <- dailyAspects[, c(planetsAll) := lapply(.SD, function(x) ifelse(x > 0, 1, 0)), .SDcols = planetsAll]

selectCols <- c(
  "Date",
  aspectType,
  aspectsAll,
  "sp.x", "spi.x", "sp.y", "spi.y",
  "dc.y", "dc.x",
  "retx", "rety",
  "spd", "spdi", "spp", "spr", "spri",
  "dcd", "dcdi", "dcp", "dcr", "dcri",
  "orb", "apl", "sep"
)

# Fit a90 aspects model.
aspectViewRaw <- dailyAspects[p.x %in% c('JU') & aspect %in% c(0, 90, 120, 150, 180)]
aspectView <- aspectViewRaw[, ..selectCols]
aspectViewTrain <- merge(securityData[, c('Date', 'diffPercent')], aspectView, by = "Date")
hist(aspectViewTrain$diffPercent)
# trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.80, list = FALSE)

modelSearch <- glmulti(
  y = "diffPercent",
  xr = c(
    aspectsD
    #aspectsT
    #aspectType,
    #aspectsX,
    #aspectsY
    #"apl", "sep",
    #"spd", "spdi", "spr", "spri", "spp",
    #"dcd", "dcdi", "dcp", "dcr", "dcri"
    #"retx", "rety"
    #"sp.x", "sp.y",
    #"spi.x", "spi.y",
    #"dc.y", "dc.x"
  ),
  data = aspectViewTrain,
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
  data = aspectViewTrain
)

modelFit %>% summary()
modelFit %>% plot()
modelFit %>% coefplot()

# Validate with reserved data.
securityDataTest <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2020-07-01")
aspectViewValidate <- aspectViewRaw[, ..selectCols]
aspectViewValidate$diffPredict <- predict(modelFit, aspectViewValidate)
aspectViewValidate$diffPredictSmooth <- SMA(aspectViewValidate$diffPredict, 3)
# Dsiplay projected prediction in chart
ggplot(data = aspectViewValidate[Date >= Sys.Date() - 30,]) +
  geom_line(aes(x = Date, y = diffPredictSmooth), colour = "black", alpha = 0.7) +
  scale_x_date(date_breaks = "7 days", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, size = 10), axis.title.x = element_blank(), axis.title.y = element_blank())
aspectViewValidate <- merge(securityDataTest[, c('Date', 'diffPercent')], aspectViewValidate, by = "Date")
aspectViewValidate[, c('Date', 'diffPercent', 'diffPredict')]
plot(aspectViewValidate$diffPercent, aspectViewValidate$diffPredict)
cor(aspectViewValidate$diffPercent, aspectViewValidate$diffPredict) %>% print()
with(aspectViewValidate, mean((diffPercent - diffPredict)^2)) %>% sqrt()

# CONCLUSION:
# Cumulative energy difference can explain only R2 0.05-0.08 variance on ME, SU, MA
# don't fit at all for VE nor MO (which is expected) but the predicted results has a good
# correlation ME 0.30, SU 0.40 and MA 0.99 on test predicted data.
# The speed as part of energy calculation fit well on SU and MA but deviated the results from ME
# the other variables speed, retrograde, declination and aspect type perfomed very bad.
# Seems that different data organization is needed where single observation per day
# with cumulative energy for each planet / aspect could be analyzed.
# Removed a135 aspect don't caused lost of model fit, seems is not relevant.
# Attempt to remove a30 caused that ME aspects don't fit anymore.