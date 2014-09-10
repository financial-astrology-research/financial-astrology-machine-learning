#####################################################################################################
# Natal chart aspects model
####################################################################################################
cmpNatalAspectsModel <- function(func, ...) {
  modenv <- environment()
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  setNatalLons <- function(args) {
    # build natal longitudes
    args$siglons <- buildNatalLongitudes(args$symbol)
    # set the asptype to use to siglons
    args$asptype <- 'natal'
    return(args)
  }

  setParamsPAPAEPZSP <- function(args) {
    # build matrix
    args$cusorbs <- matrix(args$cusorbs, nrow=1, ncol=length(aspects), byrow=T,
                           dimnames=list('orbs', aspects))

    args$aspectspolarity <- matrix(args$aspectspolarity, nrow=1, ncol=length(aspects), byrow=T,
                                   dimnames=list('polarity', aspects))

    args$aspectsenergy <- matrix(args$aspectsenergy, nrow=1, ncol=length(args$aspectsenergy), byrow=T,
                                 dimnames=list(c('energy'), aspects))

    args$sigpenergy <- matrix(args$sigpenergy, nrow=1, ncol=length(args$sigpenergy), byrow=T,
                              dimnames=list(c('energy'), args$siglons$lon))

    args$planetszodenergy <- matrix(args$planetszodenergy, nrow=length(planetsBaseCols), ncol=12, byrow=T,
                                    dimnames=list(planetsBaseCols, zodSignsCols))

    # Generate the string solution for for the given model parameters
    args$strsol <- with(args, paste("natalAspectsModel('testSolution'",
                            ", symbol=", shQuote(symbol),
                            ", securityfile=", shQuote(securityfile),
                            ", planetsfile=", shQuote(planetsfile),
                            ", predfile=", shQuote(predfile),
                            ", tsdate=", shQuote(tsdate), ", tedate=", shQuote(tedate),
                            ", mapredsm=", mapredsm, ", mapricefs=", mapricefs, ", mapricesl=", mapricesl,
                            ", cusorbs=c(", paste(cusorbs, collapse=", "), ")",
                            ", aspectsenergy=c(", paste(aspectsenergy, collapse=", "), ")",
                            ", sigpenergy=c(", paste(sigpenergy, collapse=", "), ")",
                            ", planetszodenergy=c(", paste(planetszodenergy, collapse=", "), ")",
                            ", aspectspolarity=c(", paste(aspectspolarity, collapse=", "), ")",
                            ", dateformat=", shQuote(dateformat), ", verbose=T", ", doplot=T, plotsol=F",
                            ", fittype=", shQuote(fittype), ")\n", sep=""))

    return(args)
  }

  # Build args list for aspects polarity, aspects energy, zodenergy, significant points energy
  processParamsPolarityAspZodSiglonsEnergy <- function(x, args) {
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

    args <- setModelData(args)
    args <- setNatalLons(args)
    args <- setParamsPAPAEPZSP(args)

    return(args)
  }

  # Use processParamsPolarityAspZodSiglonsEnergy with data sample split
  natalModelParamsOne <- function(args) {
    # Build the params sets
    args <- processParamsPolarityAspZodSiglonsEnergy(args$x, args)
    args$datasplitfunc <- 'dataOptCVSampleSplit'
    args$verbose <- T
    return(args)
  }

  # Use processParamsPolarityAspZodSiglonsEnergy with data year split
  natalModelParamsTwo <- function(args) {
    # Build the params sets
    args <- processParamsPolarityAspZodSiglonsEnergy(args$x, args)
    args$datasplitfunc <- 'dataOptCVYearSplit'
    args$verbose <- T
    return(args)
  }

  # Build the orbs, polarity, aspect energy, sigpoints energy and zodenergy matrix
  # that are parameters needed to build the daily aspects energy. With conjuntion
  # as a neutral.
  calculateAZSPEN <- function(args) {
    # Calculate daily aspects energy for predict dates
    energy.days <- dayAspectsEnergy(args)
    return(energy.days)
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
    energy.days <- calculateAZSPEN(args)
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

  prepareSolutionArgs <- function(...) {
    args <- list(...)
    if (!hasArg('dateformat')) stop("A dateformat is needed.")
    args$predfile <- paste("~/", args$predfile, ".pdf", sep="")

    # Create directory if do not exists
    if (!file.exists(dirname(args$predfile))) {
      dir.create(dirname(args$predfile), recursive=T)
    }

    # Build the params sets
    args <- setModelData(args)
    args <- setNatalLons(args)
    args <- setParamsPAPAEPZSP(args)
    # By default use conjunction neutral energy
    args$conpolarity <- F

    return(args)
  }

  testSolution <- function(...) {
    args <- prepareSolutionArgs(...)
    if (args$doplot) pdf(args$predfile, width=11, height=8, family='Helvetica', pointsize=12)
    res <- relativeTrend(args)
    if (args$doplot) dev.off()
    # Return a response
    return(res)
  }

  testSolutionConPol <- function(...) {
    args <- prepareSolutionArgs(...)
    args$conpolarity <- T
    if (args$doplot) pdf(args$predfile, width=11, height=8, family='Helvetica', pointsize=12)
    res <- relativeTrend(args)
    if (args$doplot) dev.off()
    # Return a response
    return(res)
  }

  prepareForGA <- function(...) {
    args <- list(...)
    args <-with(args, list(branch=branchName(),
                           paramsfunc=paramsfunc,
                           benchno=benchno,
                           sectype=sectype,
                           secsymbols=secsymbols,
                           planetsfile=planetsfile,
                           tsdate=tsdate,
                           tedate=tedate,
                           mapricefs=mapricefs,
                           mapricesl=mapricesl,
                           fittype=fittype,
                           dateformat=dateformat,
                           verbose=F,
                           doplot=F,
                           plotsol=F))

    # Clear the cache directory before start
    clearCache()
    # redirect the output to symbol sink file
    args$sinkpathfile <- with(args, npath(paste("~/trading/benchmarks/b", benchno, "_", sectype, ".txt", sep='')))

    # Redirect output to file
    #if (exists('sinkfile', envir=parent.frame())) {
    sink(args$sinkpathfile, append=T)
    cat("# version: ", args$branch, "\n")
    with(args, cat("#", tsdate, '-', tedate, 'OPTwCV -', fittype, 'fit -', mapricefs, '-', mapricesl, 'MAS', '\n\n'))
    cat("setModernAspectsSet()\n")
    cat("bt <- list()\n")
    sink()

    return(args)
  }

  loopParamsGA <- function(symbol, args) {
    # Restart timer for each symbol GA optimization
    ptm <<- proc.time()
    args$symbol <- symbol
    # buid securityfile and predfile paths
    args$securityfile <- with(args, paste(sectype, symbol, sep="/"))
    args$predfile <- with(args, paste('b', benchno, '/', symbol, '_', benchno, sep=""))
    cat("Starting GA optimization for ", args$symbol, " - ", args$sinkpathfile, "\n")

    return(args)
  }

  loopSolutionGA <- function(gar, args) {
    # output the solution string
    sink(args$sinkpathfile, append=T)
    args$x <- gar@solution[1,]
    # Execute the appropriate params function
    args <- execfunc(args$paramsfunc, modenv, args)
    # Print solution / fitness / BT call
    cat("res <-", args$strsol)
    cat("# Fitness=", gar@fitnessValue, "\n")
    with(args, cat("bt$", symbol, " <- testStrategy(openSecurityOnEnv(", shQuote(securityfile), "), ",
                   shQuote(benchno), ', ', shQuote(symbol), ", res$pred)\n\n", sep=''))
    sink()
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

    args <- prepareForGA(...)

    for (symbol in args$secsymbols) {
      # process arguments
      args <- loopParamsGA(symbol, args)

      gar <- ga("real-valued", fitness=relativeTrendExec, parallel=T, monitor=gaMonitor, maxiter=10, run=50, min=minvals, max=maxvals,
                popSize=100, elitism=100, pcrossover=0.9, pmutation=0.1,
                selection=gaint_rwSelection, mutation=gaint_raMutation, crossover=gaint_spCrossover, population=gaint_Population, args=args)

      loopSolutionGA(gar, args)
    }
  }

  execfunc(get('func'), modenv, ...)
}

# compile the function to byte code
natalAspectsModel <- cmpfun(cmpNatalAspectsModel)
