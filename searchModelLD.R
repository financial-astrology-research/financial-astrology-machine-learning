# Title     : Search candidate ModelLD parameters for different securities.
# Created by: pablocc
# Created on: 04/10/2020

library(boot)
library(caret)
library(psych)
library(gbm)
library(glmulti)
source("./analysis.r")
source("./indicatorPlots.r")

dailyAspects <- dailyCombPlanetAspectsFactorsTable()

symbol <- "BAT-USD"
securityData <- mainOpenSecurity(
  symbol, 2, 4, "%Y-%m-%d",
  "2010-01-01", "2020-06-30"
)

# Filter the extreme outliers.
cat(paste("Original days rows: ", nrow(securityData)), "\n")
securityData <- securityData[zdiffPercent < 3 & zdiffPercent > -3,]
hist(securityData$zdiffPercent)
cat(paste("Total days rows: ", nrow(securityData)), "\n")

aspectView <- merge(
  securityData,
  dailyAspects, by = "Date"
)

trainIndex <- createDataPartition(aspectView$diffPercent, p = 0.90, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

selectCols <- c(
  'Eff', 'diffPercent'
  , 'MOME', 'MOVE', 'MOSU', 'MOMA', 'MOJU'
  , 'MOSA', 'MOUR', 'MONE', 'MOPL', 'MONN'
  , 'MEVE', 'MESU', 'MEMA', 'MEJU', 'MESA', 'MEUR', 'MENE', 'MEPL', 'MENN'
  , 'SUMA', 'SUJU', 'SUUR', 'SUNE', 'SUPL', 'SUNN'
  #,'MAJU', 'MASA', 'MAUR', 'MANE', 'MAPL'
  #,'JUSA'
)

modelSearch <- glmulti(
  y = "Eff",
  xr = selectCols[c(-1, -2)],
  #exclude=c("sp.y", "sp.x", "dc.x", "dc.y"),
  data = aspectViewTrain[, ..selectCols],
  fitfunction = glm,
  family = binomial,
  level = 1,
  marginality = F,
  intercept = T,
  crit = "aicc",
  confsetsize = 10,
  method = "g",
  plotty = F,
  popsize = 100
  #mutrate = 0.01, sexrate = 0.1, imm = 0.1,
)

plot(modelSearch, type = "s")
print(modelSearch@formulas)

control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 3,
  search = "random",
  savePredictions = "final",
  classProbs = T,
  allowParallel = T
)

# BAT predictors
# 'MEVE', 'MEUR', 'MENE', 'MEPL', 'VESU', 'VENE'
# Eff~1+MOVE+MEVE+MESU+MENE+SUMA+SUPL
# MOME+MOVE+MEVE
# MOME', 'MOVE', 'MENE', 'MEPL', 'VESU'
# MOME+MOVE+MOSU+MEVE+MENE+SUMA+SUPL+MASA+MANE+MAPL
# Eff~1+MOME+MOVE+MEVE+MESU+MENE+SUMA+SUPL+MASA+MANE+MAPL
# Eff~1+MOME+MOVE+MEVE+MESU+SUMA+SUPL+MASA+MANE+MAPL+JUSA
# Eff~1+MOME+MOVE+MEVE+MESU+MENE+SUMA+SUPL
predictorCols <- c('Eff', 'MOME', 'MOVE', 'MEVE', 'MESU', 'SUMA', 'SUPL')

logisticModel <- train(
  formula(Eff ~ .),
  data = aspectViewTrain[, ..predictorCols],
  method = "glm",
  trControl = control,
  tuneLength = 3
)

logisticModel %>% print()
#logisticModel1 %>% summary()

aspectViewTrain$EffPred <- predict(logisticModel, aspectViewTrain, type = "raw")
table(
  actualclass = as.character(aspectViewTrain$Eff),
  predictedclass = as.character(aspectViewTrain$EffPred)
) %>%
  confusionMatrix(positive = "up") %>%
  print()

# Validate data predictions.
aspectViewValidate$EffPred <- predict(logisticModel, aspectViewValidate, type = "raw")

table(
  actualclass = as.character(aspectViewValidate$Eff),
  predictedclass = as.character(aspectViewValidate$EffPred)
) %>%
  confusionMatrix(positive = "up") %>%
  print()


control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 1,
  search = "random",
  allowParallel = T
)

rfModel = train(
  formula(Eff ~ .),
  data = aspectViewTrain[, ..predictorCols],
  method = "rf",
  metric = "Accuracy",
  tuneLength = 3,
  #ntree = 100,
  trControl = control,
  importance = F
)

rfModel %>% print()

aspectViewTrain$EffPred <- predict(rfModel, aspectViewTrain, type = "raw")
table(
  actualclass = as.character(aspectViewTrain$Eff),
  predictedclass = as.character(aspectViewTrain$EffPred)
) %>%
  confusionMatrix(positive = "up") %>%
  print()

# Validate data predictions.
aspectViewValidate$EffPred <- predict(rfModel, aspectViewValidate, type = "raw")

table(
  actualclass = as.character(aspectViewValidate$Eff),
  predictedclass = as.character(aspectViewValidate$EffPred)
) %>%
  confusionMatrix(positive = "up") %>%
  print()

knnModel <- train(
  formula(Eff ~ .),
  data = aspectViewTrain[, ..predictorCols],
  method = "knn",
  trControl = control,
  tuneLength = 3
)

knnModel %>% print()
#logisticModel1 %>% summary()

aspectViewTrain$EffPred <- predict(knnModel, aspectViewTrain, type = "raw")
table(
  actualclass = as.character(aspectViewTrain$Eff),
  predictedclass = as.character(aspectViewTrain$EffPred)
) %>%
  confusionMatrix(positive = "up") %>%
  print()

# Validate data predictions.
aspectViewValidate$EffPred <- predict(knnModel, aspectViewValidate, type = "raw")

table(
  actualclass = as.character(aspectViewValidate$Eff),
  predictedclass = as.character(aspectViewValidate$EffPred)
) %>%
  confusionMatrix(positive = "up") %>%
  print()
