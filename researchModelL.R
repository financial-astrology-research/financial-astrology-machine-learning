# Title,  : Daily aspects ModelL energy research.
# Objective : Research model generalizing the aspects per former planet.
# Created by: pablocc
# Created on: 24/09/2020

library(caret)
library(magrittr)
library(parallel)
library(psych)
library(plyr)
library(randomForest)
library(rattle)
library(tidyverse)
library(gvlma)
library(arm)
source("./indicatorPlots.r")

dailyAspects <- prepareHourlyAspectsModelK()
symbol <- "LINK-USD"
securityData <- mainOpenSecurity(
  symbol, 14, 28, "%Y-%m-%d",
  "2010-01-01", "2020-08-31"
)

#aspectViewRaw <- dailyAspects[p.x != "MO"]
aspectsT <- paste("a", aspects, sep = "")
aspectsX <- paste("a", aspects, ".x", sep = "")
aspectsY <- paste("a", aspects, ".y", sep = "")
aspectsG <- paste("a", aspects, ".g", sep = "")
aspectsCols <- c(
  aspectsX, aspectsY,
  "sp.y", "sp.x", "dc.y", "dc.x", "spd", "spp", "acx", "acy", "agt",
  "ME.x", "VE.x", "SU.x", "MA.x", "JU.x", "NN.x", "SA.x", "UR.x",
  "MO", "NE", "PL",
  "ME.y", "VE.y", "SU.y", "MA.y", "JU.y", "NN.y", "SA.y", "UR.y",
  "zx", "zy", "aspect"
)

selectCols <- c("Date", aspectsCols)

#  Evaluate polarity effect on VE60 aspects.
aspectViewRaw <- dailyAspects[p.x == "VE" & aspect == 60,]
aspectView <- aspectViewRaw[, ..selectCols]
aspectView <- merge(securityData[, c('Date', 'diffPercent')], aspectView, by = "Date")

varCorrelations <- aspectView[, -c('Date')] %>%
  cor() %>%
  round(digits = 2)
finalCorrelations <- sort(varCorrelations[, 1])
print(finalCorrelations)

# The effect polarity of an aspects seems to depend on the 150 angle that is neutral
# and apply differently depending on other active aspect polarities when JU and MA
# form part of that interactions.
modelFit <- lm(
  diffPercent ~
    SU.x +
      MO +
      JU.y +
      MA.y +
      a135.y +
      a150.y +
      a180.y +
      a60.x +
      a150.x +
      a90.y,
  data = aspectView
)
modelFit %>% summary()
modelFit %>% plot()
modelFit %>% coefplot()
modelFit %>% gvlma() %>% summary()

# Evaluate polarity effect on VE90 aspect.
aspectViewRaw <- dailyAspects[p.x == "VE" & aspect == 90,]
aspectView <- aspectViewRaw[, ..selectCols]
aspectView <- merge(securityData[, c('Date', 'diffPercent')], aspectView, by = "Date")

varCorrelations <- aspectView[, -c('Date')] %>%
  cor() %>%
  round(digits = 2)
finalCorrelations <- sort(varCorrelations[, 1])
print(finalCorrelations)

lm(
  diffPercent ~
    dc.x +
      UR.x +
      SU.y +
      SU.x +
      SA.x +
      a90.x +
      a135.x,
  data = aspectView
) %>% summary()

# Evaluate polarity effect on ME90 aspect.
aspectViewRaw <- dailyAspects[p.x == "ME" & aspect == 90,]
aspectView <- aspectViewRaw[, ..selectCols]
aspectView <- merge(securityData[, c('Date', 'diffPercent')], aspectView, by = "Date")

varCorrelations <- aspectView[, -c('Date')] %>%
  cor() %>%
  round(digits = 2)
finalCorrelations <- sort(varCorrelations[, 1])
print(finalCorrelations)

lm(
  diffPercent ~
    spp +
      agt +
      MA.y +
      MO +
      VE.x +
      a0.x +
      a60.y +
      a45.y +
      a90.y +
      a120.y,
  data = aspectView
) %>% summary()

# Evaluate polarity effect on SU90 aspect.
aspectViewRaw <- dailyAspects[p.x == "SU" & aspect == 90,]
aspectView <- aspectViewRaw[, ..selectCols]
aspectView <- merge(securityData[, c('Date', 'diffPercent')], aspectView, by = "Date")

varCorrelations <- aspectView[, -c('Date')] %>%
  cor() %>%
  round(digits = 2)
finalCorrelations <- sort(varCorrelations[, 1])
print(finalCorrelations)

modelFit <- lm(
  diffPercent ~
    dc.y +
      JU.y +
      SA.x +
      SA.y +
      a0.y +
      a120.x +
      a135.y +
      a90.x,
  data = aspectView
)

modelFit %>% summary()
# modelFit %>% confint()
modelFit %>% plot()
modelFit %>% coefplot()
modelFit %>% gvlma() %>% summary()

# Evaluate polarity effect on SU120 aspect.
aspectViewRaw <- dailyAspects[p.x == "SU" & aspect == 120,]
aspectView <- aspectViewRaw[, ..selectCols]
aspectView <- merge(securityData[, c('Date', 'diffPercent')], aspectView, by = "Date")

varCorrelations <- aspectView[, -c('Date')] %>%
  cor() %>%
  round(digits = 2)
finalCorrelations <- sort(varCorrelations[, 1])
print(finalCorrelations)

modelFit <- lm(
  diffPercent ~
    JU.x +
      MO +
      NE +
      a45.x +
      a135.x +
      a135.y +
      a30.x +
      a90.x +
      dc.x,
  data = aspectView
)

modelFit %>% summary()
modelFit %>% plot()
modelFit %>% coefplot()
modelFit %>% gvlma() %>% summary()

# Evaluate polarity effect on generalized SU aspects.
# As expected, this cannot fit well because polarity rules are quite different per aspect.
aspectViewRaw <- dailyAspects[p.x == "SU",]
aspectView <- aspectViewRaw[, ..selectCols]
aspectView <- merge(securityData[, c('Date', 'diffPercent')], aspectView, by = "Date")

varCorrelations <- aspectView[, -c('Date')] %>%
  cor() %>%
  round(digits = 2)
finalCorrelations <- sort(varCorrelations[, 1])
print(finalCorrelations)

modelFit <- lm(
  diffPercent ~
    a150.x +
      a30.x +
      UR.x +
      a45.x +
      a45.y +
      a180.y +
      a180.x +
      a60.x,
  data = aspectView
)
modelFit %>% summary()
modelFit %>% plot()
modelFit %>% coefplot()
modelFit %>% gvlma() %>% summary()
