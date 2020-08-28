# Title     : Benchmark of currencies purchase energy models.
# Created by: pablocc
# Created on: 27/08/2020

source("./indicatorPlots.r")

dailyAspects <- predictSecurityModelA("EOS-USD")
dailyAspects <- predictSecurityModelB("EOS-USD")
dailyAspects <- predictSecurityModelC("EOS-USD")
dailyAspects <- predictSecurityModelD("EOS-USD")

# The direction of the trend seems more accurate with EffectM1 calculation than EffectM3.
dailyAspects <- predictSecurityModelE("EOS-USD")

# This uses EffectM3 that result in incorrect direction in last August 2020 observations.
dailyAspects <- predictSecurityModelF("EOS-USD")

dailyAspects <- predictSecurityModelG("EOS-USD")

# We lost effect magnitude that makes trend chart seems flat in some periods although correlation is very high.
dailyAspects <- predictSecurityModelH("EOS-USD")

# The trend seems flat in some periods and is lagged for few few days but correlation is high.
dailyAspects <- predictSecurityModelI("EOS-USD")

# Combine characteristics from Model E & H to analyze more important hyperparameters:
# The EffectM1 formula provides more accuracy and better magnitude on the plot against price.
# The effect decay speed influence a lot on the trend lagging effect.
# MO just as cumulative effect removes daily micro-trend resolution in favor clear weekly trend.
dailyAspects <- predictSecurityModelJ("EOS-USD")
