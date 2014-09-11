#####################################################################################################
# Natal chart aspects model
####################################################################################################
cmpNatalAspectsModel <- function(func, ...) {
  modenv <- environment()
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    if (!is.null(args$tsdate)) args$tsdate <- as.Date(args$tsdate)
    if (!is.null(args$tedate)) args$tedate <- as.Date(args$tedate)
    if (!is.null(args$vsdate)) args$vsdate <- as.Date(args$vsdate)
    if (!is.null(args$vedate)) args$vedate <- as.Date(args$vedate)
    # open planets file and leave only needed cols for better speed
    planets <- openPlanets(args$planetsfile, deforbs, calcasps=F)
    args$planets <- planets[, c('Date', 'Year', 'wday', planetsLonCols), with=F]
    # set the asptype to use to siglons
    args$model <- 'natalAspectsModel'
    return(args)
  }

  # Build args list for aspects polarity, aspects energy, zodenergy, significant points energy
  processParamsPolarityAspZodSiglonEnergy <- function(x, args) {
    # build the parameters based on GA indexes
    co.e=2+length(deforbs)
    api.e=co.e+length(aspects)-1
    ae.e=api.e+length(aspects)
    pze.e=ae.e+lenZodEnergyMi
    # 14 natal points
    spe.e=pze.e+14

    args$mapredsm <- x[1]
    args$cusorbs <- x[2:(co.e-1)]
    args$aspectspolarity <- x[co.e:(api.e-1)]
    args$aspectsenergy <- adjustEnergy(x[api.e:(ae.e-1)])
    args$planetszodenergy <- adjustEnergy(x[ae.e:(pze.e-1)])
    args$sigpenergy <- adjustEnergy(x[pze.e:(spe.e-1)])

    # Conjunction energy as neutral
    args$aspectspolarity <- c(2, args$aspectspolarity)
    args$conpolarity <- F

    args <- setMatrixOrbsPolarityAspSZodSiglonEnergy(args)

    return(args)
  }

  # Use processParamsPolarityAspZodSiglonEnergy with data sample split
  natalModelParamsOne <- function(args) {
    # Build the params sets
    args <- processParamsPolarityAspZodSiglonEnergy(args$x, args)
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$verbose <- F
    return(args)
  }

  # Use processParamsPolarityAspZodSiglonEnergy with data year split
  natalModelParamsTwo <- function(args) {
    # Build the params sets
    args <- processParamsPolarityAspZodSiglonEnergy(args$x, args)
    args$datasplitfunc <- 'dataOptCVYearSplit'
    args$verbose <- F
    return(args)
  }

  relativeTrendExec <- function(x, args) {
    args$x <- x
    # Execute the appropriate params function
    args <- execfunc(args$paramsfunc, modenv, args)
    # Execute
    res <- relativeTrend(args)
    # Return only the fitness
    return(res$fitness)
  }

  relativeTrend <- function(args) {
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
    args$predfile <- paste("~/", args$predfile, ".pdf", sep="")

    # Create directory if do not exists
    if (!file.exists(dirname(args$predfile))) {
      dir.create(dirname(args$predfile), recursive=T)
    }

    # Build the params sets
    args <- bootstrapModel(args)
    args <- setMatrixOrbsPolarityAspSZodSiglonEnergy(args)
    # By default use conjunction neutral energy
    args$conpolarity <- F

    return(args)
  }

  testSolution <- function(...) {
    args <- prepareParamsSolution(...)
    if (args$doplot) pdf(args$predfile, width=11, height=8, family='Helvetica', pointsize=12)
    res <- relativeTrend(args)
    if (args$doplot) dev.off()
    # Return a response
    return(res)
  }

  testSolutionConPol <- function(...) {
    args <- prepareParamsSolution(...)
    args$conpolarity <- T
    if (args$doplot) pdf(args$predfile, width=11, height=8, family='Helvetica', pointsize=12)
    res <- relativeTrend(args)
    if (args$doplot) dev.off()
    # Return a response
    return(res)
  }

  optimizeRelativeTrend <- function(...) {
    cat("---------------------------- Initialize optimization ----------------------------------\n\n")
    orbsmin <- rep(0, length(deforbs))
    orbsmax <- deforbs
    polaritymin <- rep(0, length(aspects)-1)
    polaritymax <- rep(1, length(aspects)-1)
    aspectenergymin <- rep(0, length(aspects))
    aspectenergymax <- rep(30, length(aspects))
    planetzodenergymin <- rep(0, lenZodEnergyMi)
    planetzodenergymax <- rep(30, lenZodEnergyMi)
    # 14 natal points
    sigpenergymin <- rep(0, 14)
    sigpenergymax <- rep(30, 14)
    # min/max ranges
    minvals <- c( 2, orbsmin, polaritymin, aspectenergymin, planetzodenergymin, sigpenergymin)
    maxvals <- c(10, orbsmax, polaritymax, aspectenergymax, planetzodenergymax, sigpenergymax)

    args <- bootstrapOptimization(modenv, ...)

    for (symbol in args$secsymbols) {
      # process arguments
      args <- bootstrapOptimizationIteration(symbol, args)

      gar <- ga("real-valued", fitness=relativeTrendExec, parallel=T, monitor=gaMonitor, maxiter=60, run=50, min=minvals, max=maxvals,
                popSize=1000, elitism=100, pcrossover=0.9, pmutation=0.1,
                selection=gaint_rwSelection, mutation=gaint_raMutation, crossover=gaint_spCrossover, population=gaint_Population, args=args)

      loopSolutionGA(gar, args)
    }
  }

  execfunc(get('func'), modenv, ...)
}

# compile the function to byte code
natalAspectsModel <- cmpfun(cmpNatalAspectsModel)
