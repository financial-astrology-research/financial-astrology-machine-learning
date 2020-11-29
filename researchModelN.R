# Title,  : Daily aspects ModelN energy research.
# Objective : Research different calculation of daily price change.

library(caret)
library(magrittr)
library(parallel)
library(psych)
library(plyr)
library(rattle)
library(tidyverse)
library(gvlma)
library(arm)
library(glmulti)
source("./indicatorPlots.r")

symbol <- "LINK-USD"
securityData <- mainOpenSecurity(
  symbol, 2, 4, "%Y-%m-%d",
  "2010-01-01", "2020-09-30"
)

# Open to HL mid difference with real price and log price.
with(securityData, plot(Date, diffOxHL))
with(securityData, plot(Date, difflogOxHL))

# Open to HLC mid difference with real price and log price.
with(securityData, plot(Date, diffOxHLC))
with(securityData, plot(Date, difflogOxHLC))

# Open to HLC mid difference with real price and log price.
with(securityData, plot(Date, diffPercent))
with(securityData, plot(Date, difflogOHLC))

# HL true directional difference with real price and log price.
with(securityData, plot(Date, diffHxL))
with(securityData, plot(Date, difflogHxL))

cat("Compare fast/slow MAs change VS OHLC today change effects\n\n")
with(securityData, table(Eff2, HLCMomEff))
with(securityData, table(Eff2, HLMomEff))
with(securityData, table(Eff2, OxHLEff))
with(securityData, table(Eff2, OxHLCEff))
with(securityData, table(Eff2, HxLEff))
cat("\n")

cat("Compare previous to today OHLC Mid price VS OHLC today change effects\n\n")
with(securityData, table(Actbin, HLCMomEff))
with(securityData, table(Actbin, HLMomEff))
with(securityData, table(Actbin, OxHLEff))
with(securityData, table(Actbin, OxHLCEff))
# TODO: Try to use diffHxL for regression and HxLEff for ensamble classification.
with(securityData, table(Actbin, HxLEff))
cat("\n")

fwrite(securityData, paste("~/Desktop/", symbol, "-extended-daily-price", ".csv", sep = ""))
