#####################################################################################################
# Natal chart aspects model
####################################################################################################
natalAspectsModelCommon <- function(args) {
  args$tsdate <- as.Date(args$tsdate)
  args$tedate <- as.Date(args$tedate)
  args$vsdate <- as.Date(args$tsdate)
  args$vedate <- as.Date(args$tedate)
  args$model <- 'natalAspectsModel'
  # common settings
  if (is.null(args$fitfunc)) args$fitfunc <- 'modelAspectsEnergy'
  if (is.null(args$datasplitfunc)) args$datasplitfunc <- 'dataOptCVSampleSplit'
  if (is.null(args$paramsfunc)) args$paramsfunc <- 'paramsPolarityAspZodSiglonEnergy'
  if (is.null(args$aspolarity)) args$aspolarity <- T
  if (is.null(args$enoperation)) args$enoperation <- '*'
  if (is.null(args$asptype)) args$asptype <- 'all'
  if (is.null(args$engrowth)) args$engrowth <- F
  if (is.null(args$verbose)) args$verbose <- F

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
    args$modelfunc <- 'natalAspectsModelOne'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
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
    args$modelfunc <- 'natalAspectsModelTwo'
    args$datasplitfunc <- 'dataOptCVYearSplit'
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

# compile the function to byte code
natalAspectsModelTwo <- cmpfun(cmpNatalAspectsModelTwo)

####################################################################
# Variation Three with CV sample split & energy growth
####################################################################
cmpNatalAspectsModelThree <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setModernAspectsSet()
    args$modelfunc <- 'natalAspectsModelThree'
    args <- natalAspectsModelCommon(args)
    args$engrowth <- T
    args$datasplitfunc <- 'dataOptCVSampleSplit'

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

# compile the function to byte code
natalAspectsModelThree <- cmpfun(cmpNatalAspectsModelThree)

####################################################################
# Variation Four with CV year split
####################################################################
cmpNatalAspectsModelFour <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setModernAspectsSet()
    args$modelfunc <- 'natalAspectsModelFour'
    args$fitfunc <- 'modelAspectsEnergyBackTest'
    args$enoperation <- '+'
    args$datasplitfunc <- 'dataOptCVYearSplit'
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

# compile the function to byte code
natalAspectsModelFour <- cmpfun(cmpNatalAspectsModelFour)

####################################################################
# Variation Five with CV sample split
####################################################################
cmpNatalAspectsModelFive <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setClassicAspectsSet()
    args$modelfunc <- 'natalAspectsModelFive'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

natalAspectsModelFive <- cmpfun(cmpNatalAspectsModelFive)

####################################################################
# Variation Six with CV sample split
####################################################################
cmpNatalAspectsModelSix <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setClassicAspectsSet()
    args$modelfunc <- 'natalAspectsModelSix'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$aspolarity <- T
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

natalAspectsModelSix <- cmpfun(cmpNatalAspectsModelSix)
