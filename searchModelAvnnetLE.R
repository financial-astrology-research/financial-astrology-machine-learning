# Title     : Experiment multiples models with caret train.
# Created by: pablocc
# Created on: 07/10/2020

library(boot)
library(caret)
library(psych)
library(plyr)
source("./analysis.r")
source("./indicatorPlots.r")

symbol <- "BAT-USD"
aspectFilter <- c()
pxSelect <- c(
  'ME',
  'VE',
  'SU',
  'MA'
)

# dailyAspects <- dailyCombPlanetAspectsFactorsTable(orbLimit = 2, aspectFilter =  aspectFilter)
# dailyAspects <- dailyCombPlanetAspectsFactorsTableLE(orbLimit = 2.5, aspectFilter =  aspectFilter)
dailyAspectsCount <- dailyAspectsGeneralizedCount(orbLimit = 2, pxSelect = c('MO', pxSelect))
dailyAspectsPlanetXCount <- dailyPlanetXActivationCount(orbLimit = 2, pxSelect = c('MO', pxSelect))
dailyAspectsPlanetYCount <- dailyPlanetYActivationCount(orbLimit = 2, pxSelect = pxSelect)
#dailyAspectsCombCount <- dailyAspectsPlanetCombGeneralizedCount(orbLimit = 2, pxSelect = pxSelect)
dailyFastPlanetsSpeed <- dailyFastPlanetsRetrograde()
dailySlowPlanetsSpeed <- dailySlowPlanetsRetrograde()
#dailyAspects <- merge(dailyAspectsCount, dailyAspectsCombCount, by = c('Date'))
#dailyAspects <- merge(dailyAspectsCount, dailyAspectsPlanetYCount, by = c('Date'))
#dailyAspects <- merge(dailyAspects, dailyAspectsPlanetXCount, by = c('Date'))
dailyAspects <- dailyAspectsCount
dailyAspects <- merge(dailyAspects, dailyAspectsPlanetYCount, by = c('Date'))
dailyAspects <- merge(dailyAspects, dailyAspectsPlanetXCount, by = c('Date'))
dailyAspects <- merge(dailyAspects, dailyFastPlanetsSpeed, by = c('Date'))
dailyAspects <- merge(dailyAspects, dailySlowPlanetsSpeed, by = c('Date'))
# dailyAspects <- dailyAspectsPlanetCombGeneralizedEnergy(orbLimit = 2)

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
  securityData[, c('Date', 'Eff', 'Actbin')],
  dailyAspects, by = "Date"
)

trainIndex <- createDataPartition(aspectView$Actbin, p = 0.80, list = FALSE)
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

# create own function so we can use "sensitivity" as our metric to maximise:
#Sensitivity.fc <- function (data, lev = levels(data$obs), model = NULL) {
#  out <- c(twoClassSummary(data, lev = levels(data$obs), model = NULL))
#  c(out, Sensitivity = out["Sens"])
#}

control <- trainControl(
  #method = "cv", # 2 - fast
  #method = "boot", # 2 - slow
  method = "cv", # 2 - slow
  #method = "optimism_boot", # 2 - slow
  #method = "boot_all", # 2 - slow
  #method = "LOOCV", # 2 - very slow
  #method = "LGOCV", # 2 - fast
  #method = "none", # 2 - very fast
  #method = "timeslice", # 2 - very slow
  #initialWindow = 30,
  #horizon = 10,
  number = 20,
  savePredictions = "final",
  returnResamp = "all",
  #summaryFunction = twoClassSummary,
  #selectionFunction = "tolerance",
  classProbs = T,
  allowParallel = T,
  verboseIter = T,
  trim = F
)

selectCols <- names(aspectViewTrain)[c(-1, -2)]
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
  method = "avNNet", # 0.54
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
  # method = "glmboost", # 0.52
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
  #metric = "ROC",
  metric = "Kappa",
  maximize = T,
  trControl = control,
  # tuneLength = 3,
  # tuneGrid = expand.grid(predFixed = 40, minNode = 15)
  #tuneGrid = expand.grid(
  #  num_iter = 50,
  #  tree_depth = 5,
  #  beta = 0.11,
  #  lambda = 0.11,
  #  loss_type = "e"
  #)
  tuneGrid = expand.grid(
    size = 8,
    decay = 0.05,
    bag = T
  ),
  maxit = 100,
  repeats = 50
  #censored = T,
  #softmax = T,
  #preProc = c("center", "scale")
)

fitModel$finalModel %>% summary()
fitModel %>% summary()
fitModel %>% print()
#fitModel %>% plot()
fitModel %>% varImp()

cat("--VALIDATE MODEL--\n\n")
# Validate test data accuracy.
validateActbinPred <- predict(fitModel, aspectViewValidate, type = "raw")
#validateActbinPred <- mapvalues(validateActbinPred, from = c("up", "down"), to = c("buy", "sell"))
validateResult <- table(
  actualclass = as.character(aspectViewValidate$Actbin),
  predictedclass = as.character(validateActbinPred)
) %>%
  confusionMatrix(positive = "buy")
print(validateResult)

cat("--TEST MODEL--\n\n")
# Validate test data accuracy.
testActbinPred <- predict(fitModel, aspectViewTest, type = "raw")
#testActbinPred <- mapvalues(testActbinPred, from = c("up", "down"), to = c("buy", "sell"))
testResult <- table(
  actualclass = as.character(aspectViewTest$Actbin),
  predictedclass = as.character(testActbinPred)
) %>%
  confusionMatrix(positive = "buy")
print(testResult)

finalActbinPred <- predict(fitModel, dailyAspects, type = "raw")
#finalActbinPred <- mapvalues(finalActbinPred, from = c("up", "down"), to = c("buy", "sell"))
dailyAspects[, finalPred := finalActbinPred]

#saveRDS(fitModel, paste("./models/", symbol, "_avnet4", ".rds", sep=""))
fwrite(dailyAspects, paste("./predictions/ml", symbol, "daily-avnnet.csv", sep = "-"))
