#####################################################################################################
# Natal chart aspects model
####################################################################################################
cmpNatalAspectsModel <- function(func, ...) {
  modenv <- environment()
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

  modelFitExec <- function(x, args) {
    args$x <- x
    # Execute the appropriate params function
    args <- execfunc(args$paramsfunc, modenv, args)
    # Execute
    res <- modelFit(args)
    # Return only the fitness
    return(res$fitness)
  }

  modelFit <- function(args) {
    looptm <- proc.time()
    # calculate energy days
    energy.days <- dayAspectsEnergy(args)
    # Calculate prediction
    prediction <- calculateUpDownEnergy(energy.days)
    # join the security table with prediction and remove NAS caused by join
    planets.pred <- args$security[prediction]
    planets.pred <- planets.pred[!is.na(Mid),]
    # smoth the prediction serie and remove resulting NAS
    planets.pred[, predval := SMA(predRaw, args$mapredsm)]
    planets.pred <- planets.pred[!is.na(predval),]
    # determine a factor prediction response
    planets.pred[, predFactor := cut(predval, c(-10000, 0, 10000), labels=c('down', 'up'), right=F)]
    # Add the Year for projected predictions rows
    planets.pred[is.na(Year), Year := as.character(format(Date, "%Y"))]
    # Split data using the appropriate function
    samples <- get(args$datasplitfunc)(args, planets.pred)
    # Calculate Fitness
    fitness <- calculateSamplesFitness(args, samples)
    #cat("\t Predict execution/loop time: ", proc.time()-ptm, " - ", proc.time()-looptm, "\n\n")
    return(list(fitness=fitness, pred=planets.pred))
  }

  prepareParamsSolution <- function(...) {
    args <- list(...)
    if (!hasArg('dateformat')) stop("A dateformat is needed.")
    # Build the params sets
    args <- bootstrapModel(args)
    args <- bootstrapSecurity(args$symbol, args)
    args <- paramsPolarityAspZodSiglonEnergy('setMatrix', args)
    args <- execfunc(args$paramsfunc, modenv, args)
    args$verbose <- T
    args$doplot <- T
    args$plotsol <- F
    # Create directory if do not exists
    if (!file.exists(dirname(args$predfile))) {
      dir.create(dirname(args$predfile), recursive=T)
    }

    return(args)
  }

  testSolution <- function(...) {
    args <- prepareParamsSolution(...)
    if (args$doplot) pdf(args$predfile, width=11, height=8, family='Helvetica', pointsize=12)
    res <- modelFit(args)
    if (args$doplot) dev.off()
    # Return a response
    return(res)
  }

  optimizeGA <- function(...) {
    args <- bootstrapOptimization(modenv, ...)

    for (symbol in args$secsymbols) {
      # process arguments
      args <- bootstrapSecurity(symbol, args)
      cat("Starting GA optimization for ", args$symbol, " - ", args$sinkpathfile, "\n")

      gar <- ga("real-valued", popSize=1000, elitism=100, pcrossover=0.9, pmutation=0.1, maxiter=60, run=50,
                fitness=modelFitExec, parallel=T, min=args$gamin, max=args$gamax, monitor=gaMonitor,
                selection=gaint_rwSelection, mutation=gaint_raMutation, crossover=gaint_spCrossover, population=gaint_Population, args=args)

      loopSolutionGA(modenv, gar, args)
    }

    exitOptimization(args)
  }

  execfunc(get('func'), modenv, ...)
}

# compile the function to byte code
natalAspectsModel <- cmpfun(cmpNatalAspectsModel)
