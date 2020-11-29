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

