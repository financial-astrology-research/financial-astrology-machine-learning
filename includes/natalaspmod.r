#####################################################################################################
# Natal chart aspects model
####################################################################################################
natalAspectsModelCommon <- function(args) {
  args$tsdate <- as.Date(args$tsdate)
  args$tedate <- as.Date(args$tedate)
  args$vsdate <- as.Date(args$tsdate)
  args$vedate <- as.Date(args$tedate)
  # common settings
  args$fitfunc <- 'modelAspectsEnergy'
  args$datasplitfunc <- 'dataOptCVSampleSplit'
  args$conpolarity <- F
  args$enoperation <- '*'
  args$asptype <- 'all'
  args$engrowth <- F
  #args$verbose <- T
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
cmpNatalAspectsModelOne <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setModernAspectsSet()
    args$model <- 'natalAspectsModel'
    args$paramsfunc <- 'paramsPolarityAspZodSiglonEnergy'
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

# compile the function to byte code
natalAspectsModelOne <- cmpfun(cmpNatalAspectsModelOne)

####################################################################
# Variation Two with CV year split
####################################################################
cmpNatalAspectsModelTwo <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setModernAspectsSet()
    args$model <- 'natalAspectsModel'
    args$paramsfunc <- 'paramsPolarityAspZodSiglonEnergy'
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

# compile the function to byte code
natalAspectsModelTwo <- cmpfun(cmpNatalAspectsModelTwo)
