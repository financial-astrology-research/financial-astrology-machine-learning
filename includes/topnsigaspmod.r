####################################################################################################
# Top N significant points aspects model
####################################################################################################

topNSigAspectsModelCommon <- function(args) {
  args$tsdate <- as.Date(args$tsdate)
  args$tedate <- as.Date(args$tedate)
  args$vsdate <- as.Date(args$vsdate)
  args$vedate <- as.Date(args$vedate)
  # Init the GA min/max
  args <- paramsPolarityAspZodSiglonEnergy('gaMinMax', args)
  # open planets file and leave only needed cols for better speed
  planets <- openPlanets(args$planetsfile, deforbs, calcasps=F)
  args$planets <- planets[, c('Date', 'Year', 'wday', planetsLonCols), with=F]

  return(args)
}

####################################################################
# Variation One with CV sample split
####################################################################
cmpTopNSigAspectsModelOne <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setModernAspectsSet()
    args$model <- 'topNSigAspectsModel'
    args$paramsfunc <- 'paramsPolarityAspZodSiglonEnergy'
    args$fitfunc <- 'modelAspectsEnergy'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$conpolarity <- T
    #args$verbose <- T
    args <- topNSigAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

# compile the function to byte code
topNSigAspectsModelOne <- cmpfun(cmpTopNSigAspectsModelOne)

####################################################################
# Variation Two with CV year split
####################################################################
cmpTopNSigAspectsModelTwo <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setModernAspectsSet()
    args$model <- 'topNSigAspectsModel'
    args$paramsfunc <- 'paramsPolarityAspZodSiglonEnergy'
    args$fitfunc <- 'modelAspectsEnergy'
    args$datasplitfunc <- 'dataOptCVYearSplit'
    args$conpolarity <- F
    #args$verbose <- T
    args <- topNSigAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

# compile the function to byte code
topNSigAspectsModelTwo <- cmpfun(cmpTopNSigAspectsModelTwo)
