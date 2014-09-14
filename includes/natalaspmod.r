#####################################################################################################
# Natal chart aspects model
####################################################################################################
cmpNatalAspectsModel <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    args$tsdate <- as.Date(args$tsdate)
    args$tedate <- as.Date(args$tedate)
    args$vsdate <- as.Date(args$tsdate)
    args$vedate <- as.Date(args$tedate)
    # Init the GA min/max
    args <- paramsPolarityAspZodSiglonEnergy('gaMinMax', args)
    # open planets file and leave only needed cols for better speed
    planets <- openPlanets(args$planetsfile, deforbs, calcasps=F)
    args$planets <- planets[, c('Date', 'Year', 'wday', planetsLonCols), with=F]
    # model settings
    args$model <- 'natalAspectsModel'
    args$paramsfunc <- 'paramsPolarityAspZodSiglonEnergy'
    args$fitfunc <- 'modelAspectsEnergy'
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$conpolarity <- F
    args$verbose <- T
    # set the asptype to use to siglons
    args$strmodparams <- with(args, paste("#", tsdate, '-', tedate, 'OPTwCV -', fittype, 'fit -', mapricefs, '-', mapricesl, 'MAS'))

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

# compile the function to byte code
natalAspectsModel <- cmpfun(cmpNatalAspectsModel)
