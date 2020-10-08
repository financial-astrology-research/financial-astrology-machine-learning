# Title     : Experiment multiples models with caret train.
# Created by: pablocc
# Created on: 07/10/2020

library(boot)
library(caret)
library(psych)
source("./analysis.r")
source("./indicatorPlots.r")

dailyAspects <- dailyCombPlanetAspectsFactorsTable()

symbol <- "LINK-USD"
securityData <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2010-01-01", "2020-07-31"
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

# TODO: Experiment partition by price diff and increase to 0.80.
trainIndex <- createDataPartition(aspectView$Actbin, p = 0.70, list = FALSE)
aspectViewTrain <- aspectView[trainIndex,]
aspectViewValidate <- aspectView[-trainIndex,]

#  Reserved data for final test, skip a week to avoid timeserie memory.
securityDataTest <- mainOpenSecurity(
  symbol, 2, 4,
  "%Y-%m-%d", "2020-08-08"
)

aspectViewTest <- merge(
  securityDataTest,
  dailyAspects,
  by = "Date"
)

selectCols <- c(
  'Actbin'
  , 'MOME', 'MOVE', 'MOSU', 'MOMA', 'MOJU', 'MOSA', 'MOUR', 'MONE', 'MOPL', 'MONN'
  , 'MEVE', 'MESU', 'MEMA', 'MEJU', 'MESA', 'MEUR', 'MENE', 'MEPL', 'MENN'
  , 'VESU', 'VEMA', 'VEJU', 'VESA', 'VEUR', 'VENE', 'VEPL', 'VENN'
  , 'SUMA', 'SUJU', 'SUSA', 'SUUR', 'SUNE', 'SUPL', 'SUNN'
)

control <- trainControl(
  #method = "cv", # 2 - fast
  #method = "boot", # 2 - slow
  #method = "boot632", # 2 - slow
  #method = "optimism_boot", # 2 - very slow
  #method = "boot_all", # 2 - slow
  #method = "LOOCV", # 2 - very slow
  method = "LGOCV", # 2 - fast
  #method = "none", # 2 - very fast
  #method = "timeslice", # 2 - very slow
  #initialWindow = 30,
  #horizon = 10,
  number = 10,
  savePredictions = "final",
  classProbs = T,
  allowParallel = T,
  verboseIter = T
)

fitModel <- train(
  formula(Actbin ~ .),
  data = aspectViewTrain[, ..selectCols],
  # method = "AdaBag", # 0.51
  # method = "BstLm", # N/A predict error
  # method = "C5.0", # 0.52
  # method = "C5.0Cost", # 0.52
  # method = "C5.0Rules", # 0.52
  # method = "C5.0Tree", # 0.52
  # method = "Linda", # 0.53
  # method = "LogitBoost", # 0.50
  # method = "ORFlog", # 0.53
  # method = "ORFpls", # 0.50
  # method = "ORFridge", # 0.5
  # method = "ORFsvm", # N/A
  # method = "PenalizedLDA", # Needs penalty function.
  # method = "QdaCov", # N/A failed resample error.
  # method = "RFlda", # 0.52
  # method = "RRF", # 0.53
  # method = "RRFglobal",
  # method = "Rborist", # 0.60
  # method = "ada", # 0.44
  # method = "adaboost", # 0.55
  # method = "amdai", # package N/A 3.6.3
  # method = "avNNet", # 0.54
  # method = "awnb", # N/A failed are factors not true error.
  # method = "awtan", # N/A failed are factors not true error.
  # method = "bag", # N/A needs bagControl function.
  # method = "bagEarth", # 0.47
  # method = "bagEarthGCV", # 0.46
  method = "bagFDA",
  # method = "bartMachine",
  # method = "bayesglm",
  # method = "binda", # 0.52
  # method = "blackboost",
  # method = "blasso",
  # method = "blassoAveraged",
  # method = "bridge",
  # method = "brnn",
  # method = "bstSm",
  # method = "bstTree",
  # method = "cforest", # 0.52
  # method = "chaid",
  # method = "ctree",
  # method = "ctree2",
  # method = "cubist",
  # method = "dda",
  # method = "deepboost",
  # method = "dwdLinear",
  # method = "dwdPoly",
  # method = "dwdRadial",
  # method = "earth",
  # method = "evtree",
  # method = "fda",
  # method = "gamboost",
  # method = "gbm",
  # method = "gbm_h2o",
  # method = "gcvEarth",
  # method = "glmStepAIC",
  # method = "glmboost",
  # method = "hda",
  # method = "hdda",
  # method = "hdrda",
  # method = "lda",
  # method = "lda2",
  # method = "lmStepAIC",
  # method = "loclda",
  # method = "logicBag",
  # method = "logreg", # 0.54
  # method = "manb",
  # method = "mda",
  # method = "mlpKerasDecayCost",
  # method = "mlpKerasDropoutCost",
  # method = "multinom",
  # method = "naive_bayes",
  # method = "nb",
  # method = "nbDiscrete",
  # method = "nbSearch",
  # method = "nnet",
  # method = "ordinalRF", # N/A
  # method = "parRF", # 0.55
  # method = "pcaNNet",
  # method = "pda",
  # method = "pda2",
  # method = "polr",
  # method = "ppr",
  # method = "qda",
  # method = "qrf",
  # method = "randomGLM",
  # method = "ranger", # 0.51
  # method = "rda",
  # method = "rf", # 0.52
  # method = "rfRules",
  # method = "rlda",
  # method = "rmda",
  # method = "rml",
  # method = "rpart",
  # method = "rpart1SE",
  # method = "rpart2",
  # method = "rpartCost",
  # method = "rpartScore",
  # method = "sda",
  # method = "slda",
  # method = "smda",
  # method = "sparseLDA",
  # method = "spikeslab",
  # method = "stepLDA",
  # method = "stepQDA",
  # method = "svmLinearWeights",
  # method = "svmLinearWeights2",
  # method = "svmRadialWeights",
  # method = "tan",
  # method = "tanSearch",
  # method = "treebag",
  # method = "vbmpRadial",
  # method = "vbmpRadial",
  # method = "vglmContRatio",
  # method = "vglmCumulative",
  # method = "wsrf",
  # method = "xgbDART",
  # method = "xgbLinear",
  # method = "xgbTree",
  # method = "extraTrees", # N/A JAVA errors
  # TODO: Continue with 7.0.9 (Discrete Weighted Discriminant)
  #mtry = 10,
  trControl = control,
  tuneLength = 3
)

fitModel$finalModel %>% summary()
fitModel %>% summary()
fitModel %>% print()
fitModel %>% plot()
fitModel %>% varImp()

cat("--VALIDATE MODEL--\n\n")
# Validate test data accuracy.
validateActbinPred <- predict(fitModel, aspectViewValidate, type = "raw")
validateResult <- table(
  actualclass = as.character(aspectViewValidate$Actbin),
  predictedclass = as.character(validateActbinPred)
) %>%
  confusionMatrix(positive = "buy")
print(validateResult)

cat("--TEST MODEL--\n\n")
# Validate test data accuracy.
testActbinPred <- predict(fitModel, aspectViewTest, type = "raw")
testResult <- table(
  actualclass = as.character(aspectViewTest$Actbin),
  predictedclass = as.character(testActbinPred)
) %>%
  confusionMatrix(positive = "buy")
print(testResult)
