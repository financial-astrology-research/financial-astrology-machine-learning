# Title,  : Daily aspects ModelN energy research.
# Objective : Research different calculation of daily price change.

library(car)
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

symbol <- "BAT-USD"
securityData <- mainOpenSecurity(
  symbol, 2, 4, "%Y-%m-%d",
  "2010-01-01", "2020-09-30"
)

# Open to HL mid difference with real price and log price.
with(securityData, plot(Date, diffOxHL))
with(securityData, qqPlot(diffOxHL))
with(securityData, plot(Date, diffsqOxHL))
with(securityData, qqPlot(diffsqOxHL))
with(securityData, plot(Date, zdiffOxHL))
with(securityData, qqPlot(zdiffOxHL))
with(securityData, plot(Date, difflogOxHL))
with(securityData, qqPlot(difflogOxHL))

# Open to HLC mid difference with real price and log price.
with(securityData, plot(Date, diffOxHLC))
with(securityData, qqPlot(diffOxHLC))
with(securityData, plot(Date, difflogOxHLC))
with(securityData, qqPlot(difflogOxHLC))

# Open to HLC mid difference with real price and log price.
with(securityData, plot(Date, diffPercent))
with(securityData, plot(Date, difflogOHLC))
with(securityData, qqPlot(difflogOHLC))

# HL true directional difference with real price and log price.
zscoreCut <- 2
with(securityData, plot(Date, diffHxL))
with(securityData, qqPlot(diffHxL))
with(securityData[abs(zdiffHxL) <= zscoreCut], plot(Date, diffHxL))
with(securityData[abs(zdiffHxL) <= zscoreCut], qqPlot(diffHxL))
with(securityData, plot(Date, difflogHxL))
with(securityData, qqPlot(difflogHxL))
with(securityData, qqPlot(zdifflogHxL))
with(securityData, plot(Date, diffsqHxL))
with(securityData, qqPlot(diffsqHxL))
with(securityData[abs(zdifflogHxL) <= zscoreCut], plot(Date, difflogHxL))
with(securityData[abs(zdifflogHxL) <= zscoreCut], qqPlot(difflogHxL))
with(securityData, hist(diffHxL))
with(securityData, hist(difflogHxL))
with(securityData, hist(diffsqHxL))
# Filter outliers to compare histograms.
with(securityData[abs(zdiffHxL) <= zscoreCut], hist(diffHxL))
with(securityData[abs(zdifflogHxL) <= zscoreCut], hist(difflogHxL))
with(securityData[abs(zdifflogHLMASxF) <= zscoreCut], hist(difflogHLMASxF))
with(securityData[abs(zdifflogHLMASxF) <= zscoreCut], qqPlot(difflogHLMASxF))
with(securityData[abs(zdifflogHLCMASxF) <= zscoreCut], hist(difflogHLCMASxF))
with(securityData[abs(zdifflogHLCMASxF) <= zscoreCut], qqPlot(difflogHLCMASxF))
with(securityData, qqPlot(difflogHxL2))
with(securityData[abs(zdifflogHxL2) <= zscoreCut], qqPlot(difflogHxL2))
with(securityData[abs(zdifflogHxL2) <= zscoreCut], hist(difflogHxL2))
with(securityData, hist(difflogHxLBuy))

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
with(securityData, table(Actbin, HxLEff))
cat("\n")

fwrite(securityData, paste("./predictions/", symbol, "-extended-daily-price", ".csv", sep = ""))
