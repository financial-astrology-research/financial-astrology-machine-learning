#####################################################################################################
# Natal chart aspects model
####################################################################################################
cmpNatalAspectsModel <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    args$tsdate <- as.Date(args$tsdate)
    args$tedate <- as.Date(args$tedate)
    # Init the GA min/max
    args <- paramsPolarityAspZodSiglonEnergy('gaMinMax', args)
    # open planets file and leave only needed cols for better speed
    planets <- openPlanets(args$planetsfile, deforbs, calcasps=F)
    args$planets <- planets[, c('Date', 'Year', 'wday', planetsLonCols), with=F]
    # set the asptype to use to siglons
    args$model <- 'natalAspectsModel'
    args$fitfunc <- 'modelAspectsEnergy'
    args$strmodparams <- with(args, paste("#", tsdate, '-', tedate, 'OPTwCV -', fittype, 'fit -', mapricefs, '-', mapricesl, 'MAS'))
    return(args)
  }

  # Use paramsPolarityAspZodSiglonEnergy with data sample split
  natalModelParamsOne <- function(args) {
    # Build the params sets
    if (!is.null(args$x)) args <- paramsPolarityAspZodSiglonEnergy('splitX', args)
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$conpolarity <- F
    args$verbose <- F
    return(args)
  }

  # Use paramsPolarityAspZodSiglonEnergy with data year split
  natalModelParamsTwo <- function(args) {
    # Build the params sets
    if (!is.null(args$x)) args <- paramsPolarityAspZodSiglonEnergy('splitX', args)
    args$datasplitfunc <- 'dataOptCVYearSplit'
    args$conpolarity <- F
    args$verbose <- T
    return(args)
  }

  testSolution <- function(args) {
    if (!hasArg('dateformat')) stop("A dateformat is needed.")
    # Build the params sets
    args <- bootstrapModel(args)
    args <- bootstrapSecurity(args$symbol, args)
    args <- paramsPolarityAspZodSiglonEnergy('setMatrix', args)
    args <- execfunc(args$initfunc, args)
    args$verbose <- T
    args$doplot <- T
    args$plotsol <- F
    # Create directory if do not exists
    if (!file.exists(dirname(args$predfile))) {
      dir.create(dirname(args$predfile), recursive=T)
    }

    args <- prepareParamsSolution(args)
    if (args$doplot) pdf(args$predfile, width=11, height=8, family='Helvetica', pointsize=12)
    res <- modelFit(args)
    if (args$doplot) dev.off()
    # Return a response
    return(res)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}

# compile the function to byte code
natalAspectsModel <- cmpfun(cmpNatalAspectsModel)
