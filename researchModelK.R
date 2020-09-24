# Title     : Research for model K using Random Forest trees.
# Objective : Evaluate how well fits the RF model to multiples symbols.
# Created by: pablocc
# Created on: 08/09/2020

library(caret)
library(magrittr)
library(parallel)
library(psych)
library(plyr)
library(randomForest)
library(rattle)
library(tidyverse)
source("./indicatorPlots.r")

dailyAspects <- prepareHourlyAspectsModelK()
symbol <- "LINK-USD"
securityData <- mainOpenSecurity(
  symbol, 14, 28, "%Y-%m-%d",
  "2010-01-01", "2020-08-15"
)

# Experiment with Random Forest model.
aspectViewRaw <- dailyAspects[p.x == "MO"]
aspectsT <- paste("a", aspects, sep = "")
aspectsX <- paste("a", aspects, ".x", sep = "")
aspectsY <- paste("a", aspects, ".y", sep = "")
#selectCols <- c("result", "acx", aspectsX, "spp", "dcp", "zx", "zy", "MO", "ME", "VE", "SU", "MA", "JU", "SA")
#selectCols <- c("result", aspectsX, "ME.x", "VE.x", "MA.x", "JU.x", "SA.x", "NN.x")
aspectsCols <- c(aspectsX, aspectsY, "acx", "acy")
selectCols <- c("Date", aspectsCols)
aspectView <- aspectViewRaw[, ..selectCols]
aspectView <- aspectView[, lapply(.SD, sum), by=Date, .SDcols=aspectsCols]
aspectView <- merge(securityData[, c('Date', 'diffPercent')], aspectView, by = "Date")
aspectView[, result := cut(diffPercent, c(-100, 0, 100), c("sell", "buy"))]

selectColsFiltered <- c("result", aspectsCols)
aspectViewFiltered <- aspectView[, ..selectColsFiltered]
trainIndex <- createDataPartition(aspectViewFiltered$result, p = 0.70, list = FALSE)
aspectViewTrain <- aspectViewFiltered[trainIndex,]
aspectViewTest <- aspectViewFiltered[-trainIndex,]

control <- trainControl(
  method = "repeatedcv",
  number = 20,
  repeats = 5,
  search = "random",
  allowParallel = T
)

tree1 = train(
  formula(result ~ .),
  data = aspectViewTrain,
  method = "rf",
  metric = "Accuracy",
  tuneLength = 3,
  ntree = 100,
  trControl = control,
  importance = F
)
#summary(tree1)

effect_p <- tree1 %>% predict(newdata = aspectViewTrain)
# Prediction results on train.
table(
  actualclass = aspectViewTrain$result,
  predictedclass = effect_p
) %>%
  confusionMatrix() %>%
  print()

effect_p <- tree1 %>% predict(newdata = aspectViewTest)
# Prediction results on test.
table(
  actualclass = aspectViewTest$result,
  predictedclass = effect_p
) %>%
  confusionMatrix() %>%
  print()

#saveRDS(tree1, "./models/LINK_MO_general_rf4.rds")

selectCols2 <- selectCols[selectCols != "result"]
futureAspects <- dailyAspects[Date >= as.Date("2020-08-20") & p.x == "MO",]
futureAspectsFeatures <- futureAspects[, ..selectCols2]
effect_p <- tree1 %>% predict(newdata = futureAspectsFeatures)
futureAspects$effect_p <- mapvalues(effect_p, from = c("down", "up"), to = c("sell", "buy"))
marketPrediction <- futureAspects[, c('Date', "effect_p")]
setnames(marketPrediction, c('Date', 'Action'))
fwrite(marketPrediction[Date <= Sys.Date() + 60], paste("~/Desktop/ml", symbol, "daily.csv", sep = "-"))
