# Title     : Benchmark of currencies purchase energy models.
# Created by: pablocc
# Created on: 27/08/2020

source("./indicatorPlots.r")

symbol <- "EOS-USD"
securityData <- mainOpenSecurity(symbol, 14, 28, "%Y-%m-%d", "2010-01-01")
securityData[, fold := kfold(securityData, k = 5)]

dailyAspects <- predictSecurityModelA(securityData)
dailyAspects <- predictSecurityModelB(securityData)
dailyAspects <- predictSecurityModelC(securityData)
dailyAspects <- predictSecurityModelD(securityData)

# The direction of the trend seems more accurate with EffectM1 calculation than EffectM3.
dailyAspects <- predictSecurityModelE(securityData)

# This uses EffectM3 that result in incorrect direction in last August 2020 observations.
dailyAspects <- predictSecurityModelF(securityData)

dailyAspects <- predictSecurityModelG(securityData)

# We lost effect magnitude that makes trend chart seems flat in some periods although correlation is very high.
dailyAspects <- predictSecurityModelH(securityData)

# The trend seems flat in some periods and is lagged for few few days but correlation is high.
dailyAspects <- predictSecurityModelI(securityData)

# Combine characteristics from Model E & H to analyze more important hyperparameters:
# The EffectM1 formula provides more accuracy and better magnitude on the plot against price.
# The effect decay speed influence a lot on the trend lagging effect.
# MO just as cumulative effect removes daily micro-trend resolution in favor clear weekly trend.
dailyAspects <- predictSecurityModelJ(securityData)

# In cross validation ModelH has demonstrated better generalization so next models are variations
# to try to imporove the accuracy of the energy index.
dailyAspects <- predictSecurityModelH1(securityData)
# Test using all the aspects set with small orbs to avoid collisions.
dailyAspects <- predictSecurityModelH2(securityData)
# Test complete list of modern aspects that needs reduced orbs to don't overlap,
# the observations point to a reduction of accuracy when using minor second scale aspects.
dailyAspects <- predictSecurityModelH3(securityData)

hourlyAspects <- prepareHourlyAspectsModelH1()
hourlyAspectsPrice <- merge(hourlyAspects, securityData[, c('Date', 'diffPercent')], by = "Date")
params <- c(-3, -2, 0, -1, 3, -1, -1, 3, 0)
predictSecurityModelH1A(params, securityData, hourlyAspects)