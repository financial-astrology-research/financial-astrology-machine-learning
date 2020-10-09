# Title     : Experiment multiples models with caret train.
# Created by: pablocc
# Created on: 07/10/2020

library(boot)
library(caret)
library(psych)
source("./analysis.r")
source("./indicatorPlots.r")

aspectFilter <- c()
pxFilter <- c()
# dailyAspects <- dailyCombPlanetAspectsFactorsTable(orbLimit = 2, aspectFilter =  aspectFilter)
# dailyAspects <- dailyCombPlanetAspectsFactorsTableLE(orbLimit = 2.5, aspectFilter =  aspectFilter)
# dailyAspects <- dailyAspectsGeneralizedCount(orbLimit = 2.5)
# dailyAspects <- dailyAspectsGeneralizedOrbsMean(orbLimit = 2, pxFilter = pxFilter)
# dailyAspects <- dailyAspectsGeneralizedEnergySum(orbLimit = 2, pxFilter = pxFilter)
# dailyAspects <- dailyAspectsPlanetXGeneralizedCount(orbLimit = 2)
# The planet receiver aspects count seems significant.
# dailyAspects <- dailyAspectsPlanetYGeneralizedCount(orbLimit = 2)
# dailyAspects <- dailyAspectsPlanetCombGeneralizedCount(orbLimit = 2)
dailyAspects <- dailyAspectsPlanetCombGeneralizedEnergy(orbLimit = 2)

symbol <- "BNB-USD"
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
  securityData[, c('Date', 'Actbin')],
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

#selectCols <- c(
#  'Actbin'
#  , 'MOME', 'MOVE', 'MOSU', 'MOMA', 'MOJU', 'MOSA', 'MOUR', 'MONE', 'MOPL'
#  , 'MEVE', 'MESU', 'MEMA', 'MEJU', 'MESA', 'MEUR', 'MENE', 'MEPL'
#  , 'VESU', 'VEMA', 'VEJU', 'VESA', 'VEUR', 'VENE', 'VEPL'
#  #, 'SUMA', 'SUJU', 'SUSA', 'SUUR', 'SUNE', 'SUPL'
#  #, 'MAJU', 'MASA', 'MAUR', 'MANE', 'MAPL'
#)

selectCols <- names(aspectViewTrain)[-1]

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
  number = 5,
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
  # method = "ORFridge", # 0.50
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
  # method = "bagFDA", # 0.51
  # method = "bartMachine", # N/A incorrect number of dimensions error
  # method = "bayesglm", # 0.52
  # method = "binda", # 0.52
  # method = "blackboost", # 0.50
  # method = "blasso", # N/A only works for regression
  # method = "blassoAveraged", # N/A only works for regression
  # method = "bridge", # N/A only works for regression
  # method = "brnn", # N/A only works for regression
  # method = "bstSm", # N/A error in smooth.spline
  # method = "bstTree", # 0.52
  # method = "cforest", # 0.52
  # method = "chaid", # N/A not available for R 3.6.3
  # method = "ctree", # 0.50
  # method = "ctree2", # 0.50
  # method = "cubist", # N/A only works for regression
  # method = "dda", # N/A not available for R 3.6.3
  # method = "deepboost", # 0.54
  # method = "dwdLinear", # 0.53
  # method = "dwdPoly", # 0.53
  # method = "dwdRadial", # 0.54
  # method = "earth", # 0.52
  # method = "evtree", # N/A no split can be found
  # method = "fda", # 0.53
  # method = "gamboost", # N/A Cholesky factorization failed
  # method = "gbm", 0.56
  # method = "gbm_h2o", # N/A no active connection H20
  # method = "gcvEarth", # 0.50
  # method = "glmStepAIC", # N/A extremelly slow (stopped)
  method = "glmboost", # 0.52
  # method = "hda", # N/A extremelly slow (stopped)
  # method = "hdda",
  # method = "hdrda",
  # method = "lda", # 0.53
  # method = "lda2", # 0.52
  # method = "lmStepAIC",
  # method = "loclda", # N/A error in solve.default
  # method = "logicBag",
  # method = "logreg", # 0.54
  # method = "manb", # N/A are factors is not true
  # method = "mda", # 0.46
  # method = "mlpKerasDecayCost", # N/A Finished with exit code 124
  # method = "mlpKerasDropoutCost", # N/A Finished with exit code 124
  # method = "multinom", # 0.50
  # method = "naive_bayes", # 0.52
  # method = "nb", # N/A zero variance in one class error
  # method = "nbDiscrete", # N/A are factors is not true
  # method = "nbSearch", # N/A are factors is not true
  # method = "nnet", # 0.52
  # method = "ordinalRF", # N/A arguments imply differing number of rows
  # method = "parRF", # 0.55
  # method = "pcaNNet", # 0.53
  # method = "pda", # 0.53
  # method = "pda2", # 0.52
  # method = "polr", # N/A response must have 3 or more levels
  # method = "ppr", # N/A only for regression
  # method = "qda", # N/A rank deficiency in group "sell"
  # method = "qrf", # N/A only for regression
  # method = "randomGLM", # N/A very slow (stopped)
  # method = "ranger", # 0.51
  # method = "rda", # N/A error in solve default
  # method = "rf", # 0.52
  # method = "rfRules", # 0.53
  # method = "rlda", # Not available for R 3.6.3
  # method = "rmda", # N/A fit failed x must be an array of two dimensions
  # method = "rlm", # N/A only for regression
  # method = "rpart", # 0.51
  # method = "rpart1SE", # 0.51
  # method = "rpart2", # 0.52
  # method = "rpartCost", # N/A all equal responses
  # method = "rpartScore", # N/A no non-missing arguments to max error
  # method = "sda", # 0.53
  # method = "slda", # 0.52
  # method = "smda", # N/A extremelly slow (stopped)
  # method = "sparseLDA", # 0.51
  # method = "spikeslab", # Only for regression
  # method = "stepLDA", # 0.52
  # method = "stepQDA", # 0.52
  # method = "svmLinearWeights", # 0.48
  # method = "svmLinearWeights2", # 0.51
  # method = "svmRadialWeights", # 0.55
  # method = "tan", # N/A are factors is not true
  # method = "tanSearch", # N/A are factors is not true
  # method = "treebag", # 0.53
  # method = "vbmpRadial", # N/A error in tmean.quad.
  # method = "vglmContRatio", # N/A deprecated errors
  # method = "vglmCumulative", # N/A deprecated errors
  # method = "wsrf", # 0.53
  # method = "xgbDART", # 0.51
  # method = "xgbLinear", # 0.52
  # method = "xgbTree", # 0.45
  # method = "extraTrees", # N/A JAVA errors
  # TODO: Continue with 7.0.9 (Discrete Weighted Discriminant)
  trControl = control,
  tuneLength = 3,
  # tuneGrid = expand.grid(predFixed = 40, minNode = 15)
  #tuneGrid = expand.grid(
  #  num_iter = 50,
  #  tree_depth = 5,
  #  beta = 0.11,
  #  lambda = 0.11,
  #  loss_type = "e"
  #)
)

fitModel$finalModel %>% summary()
fitModel %>% summary()
fitModel %>% print()
#fitModel %>% plot()
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
