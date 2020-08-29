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

# TODO: Chart predictSecurityModelH3 energy index with this params:
# ~ 0.35 correlation
# PARAMS -  Speed:  0.001 en0: -1 en30: 0 en45: 0 en51: 1 en60: -1 en72: 1 en90: 0
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: -1 en51: 1 en60: 0 en72: 1 en90: 0
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: 0 en51: 1 en60: -1 en72: 0 en90: 0
# PARAMS -  Speed:  0.001 en0: -1 en30: 0 en45: -1 en51: 1 en60: 0 en72: 1 en90: 0
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: -1 en51: 0 en60: 0 en72: -1 en90: 1
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: -1 en51: 1 en60: 0 en72: -1 en90: 1
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: 0 en51: 1 en60: 1 en72: -1 en90: 1
# ~ 0.4 correlation
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: 0 en51: 1 en60: -1 en72: 0 en90: 1
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: -1 en51: 1 en60: -1 en72: 0 en90: 1
# PARAMS -  Speed:  0.001 en0: -1 en30: -1 en45: -1 en51: 1 en60: 0 en72: 0 en90: 1
# PARAMS -    -1   -1   -1    1    0    0    1

# en0.test en30.test en45.test en51.test en60.test en72.test en90.test en103.test en120.test en135.test en144.test en150.test en180.test    output
#    1       -1        -1         0         1         0         0         1         -1         -1         -1         -1          1          0 0.3998239
#    1       -1        -1         1         0         0         0         1         -1         -1          0          0          1          0 0.3922397
