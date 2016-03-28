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

####################################################################
# Variation Seven with CV sample split, classical planets + SN and
# default aspects polarity.
####################################################################
cmpNatalAspectsModelSeven <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setClassicAspectsSet()
    setPlanetsSUMOMEVEMAJUNNSNSA()
    args$modelfunc <- 'natalAspectsModelSeven'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$aspolarity <- F
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

natalAspectsModelSeven <- cmpfun(cmpNatalAspectsModelSeven)

####################################################################
# Variation Eight with CV sample split, classical planets + CE and
# default aspects polarity.
####################################################################
cmpNatalAspectsModelEight <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setModernAspectsSet()
    setPlanetsSUMOMEVEMACEJUNNSA()
    args$modelfunc <- 'natalAspectsModelEight'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$aspolarity <- F
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

natalAspectsModelEight <- cmpfun(cmpNatalAspectsModelEight)

####################################################################
# Variation Nine with CV year split, classical planets and
# default aspects polarity.
####################################################################
cmpNatalAspectsModelNine <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setModernAspectsSet()
    setPlanetsSUMOMEVEMAJUNNSA()
    args$modelfunc <- 'natalAspectsModelNine'
    args$datasplitfunc <- 'dataOptCVYearSplit'
    args$aspolarity <- F
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

natalAspectsModelNine <- cmpfun(cmpNatalAspectsModelNine)

####################################################################
# Variation Ten with CV year split, classical planets,
# clasic aspects and default aspects polarity.
####################################################################
cmpNatalAspectsModelTen <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setClassicAspectsSet()
    setPlanetsSUMOMEVEMAJUNNSA()
    args$modelfunc <- 'natalAspectsModelTen'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$aspolarity <- F
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

natalAspectsModelTen <- cmpfun(cmpNatalAspectsModelTen)

####################################################################
# Variation Eleven with CV year split, classical planets,
# clasic aspects and optimization aspects polarity.
####################################################################
cmpNatalAspectsModelEleven <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setClassicAspectsSet()
    setPlanetsSUMOMEVEMAJUNNSA()
    args$modelfunc <- 'natalAspectsModelEleven'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$aspolarity <- T
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

natalAspectsModelEleven <- cmpfun(cmpNatalAspectsModelEleven)

####################################################################
# Variation Twelve with CV year split, classical planets,
# modern aspects and optimization aspects polarity.
####################################################################
cmpNatalAspectsModelTwelve <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setModernAspectsSet()
    setPlanetsSUMOMEVEMAJUNNSA()
    args$modelfunc <- 'natalAspectsModelTwelve'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$aspolarity <- T
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

natalAspectsModelTwelve <- cmpfun(cmpNatalAspectsModelTwelve)

####################################################################
# Variation Thirdteen with CV year split, modern planets
# clasical aspects and optimization aspects polarity.
####################################################################
cmpNatalAspectsModelThirdteen <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setClassicAspectsSet()
    setPlanetsSUMOMEVEMAJUNNSAURNEPL()
    args$modelfunc <- 'natalAspectsModelThirdteen'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$aspolarity <- T
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

natalAspectsModelThirdteen <- cmpfun(cmpNatalAspectsModelThirdteen)

#########################################################################
# Variation 14 with CV sample split, classical planets + SN + ES + EM and
# default aspects polarity.
#########################################################################
cmpNatalAspectsModelFourteen <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setClassicAspectsSet()
    setPlanetsSUMOMEVEMAJUNNSNSAESEM()
    args$modelfunc <- 'natalAspectsModelFourteen'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$aspolarity <- F
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

natalAspectsModelFourteen <- cmpfun(cmpNatalAspectsModelFourteen)

#########################################################################
# Variation 15 with CV sample split, classical planets + ES + EM and
# default aspects polarity.
#########################################################################
cmpNatalAspectsModelFifteen <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setClassicAspectsSet()
    setPlanetsSUMOMEVEMAJUNNSNSAESEM()
    args$modelfunc <- 'natalAspectsModelFifteen'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$aspolarity <- F
    args <- natalAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

natalAspectsModelFifteen <- cmpfun(cmpNatalAspectsModelFifteen)
