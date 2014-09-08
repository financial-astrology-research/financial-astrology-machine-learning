####################################################################################################
# Top N significant points aspects model
####################################################################################################
cmpTopNSigAspectsModel <- function(execfunc, ...) {
  if (!hasArg('execfunc')) stop("Provide function to execute")
  ptm <- proc.time()
  branch.name <- branchName()

  setSigLons <- function(args) {
    # build significant points vector
    siglons <- with(args, buildSignificantLongitudes(planets, security, degsplit, vsdate, vedate))
    # leave only the top N significant points
    args$siglons <- head(siglons, args$topn)
    # set the asptype to use to siglons
    args$asptype <- 'siglon'
    return(args)
  }

  setParamsPAPAEPZSP <- function(args) {
    # build matrix
    args$cusorbs <- matrix(args$cusorbs, nrow = 1, ncol = length(aspects), byrow = TRUE,
                           dimnames = list('orbs', aspects))
    # aspects polarities
    aspectspolarity <- c(2, args$aspectspolarity)
    args$aspectspolarity <- matrix(aspectspolarity, nrow = 1, ncol = length(aspects), byrow = TRUE,
                                   dimnames = list('polarity', aspects))

    args$aspectsenergy <- matrix(args$aspectsenergy, nrow = 1, ncol = length(args$aspectsenergy), byrow = TRUE,
                                 dimnames = list(c('energy'), aspects))

    args$sigpenergy <- matrix(args$sigpenergy, nrow = 1, ncol = length(args$sigpenergy), byrow = TRUE,
                              dimnames = list(c('energy'), args$siglons$lon))

    args$planetszodenergy <- matrix(args$planetszodenergy, nrow = length(planetsBaseCols), ncol = 12, byrow = TRUE,
                                    dimnames = list(planetsBaseCols, zodSignsCols))

    # Generate the string solution for for the given model parameters
    args$strsol <- with(args, paste("cmpTopNSigAspectsModel('testSolution'",
                                    ", securityfile=", shQuote(securityfile),
                                    ", planetsfile=", shQuote(planetsfile),
                                    ", predfile=", shQuote(predfile),
                                    ", tsdate=", shQuote(tsdate), ", tedate=", shQuote(tedate),
                                    ", vsdate=", shQuote(vsdate), ", vedate=", shQuote(vedate),
                                    ", mapredsm=", mapredsm, ", mapricefs=", mapricefs, ", mapricesl=", mapricesl, ", degsplit=", degsplit,
                                    ", cusorbs=c(", paste(cusorbs, collapse=", "), ")",
                                    ", aspectsenergy=c(", paste(aspectsenergy, collapse=", "), ")",
                                    ", sigpenergy=c(", paste(sigpenergy, collapse=", "), ")",
                                    ", planetszodenergy=c(", paste(planetszodenergy, collapse=", "), ")",
                                    ", aspectspolarity=c(", paste(aspectspolarity, collapse=", "), ")",
                                    ", dateformat=", shQuote(dateformat), ", verbose=F", ", doplot=T, plotsol=F",
                                    ", fittype=", shQuote(fittype), ", topn=", topn, ")\n", sep=""))

    return(args)
  }

  # Build args list for aspects polarity, aspects energy, zodenergy, significant points energy
  processParamsPAPAEPZSP <- function(x, securityfile, planetsfile, predfile, tsdate, tedate, vsdate, vedate,
                                     fittype, dateformat, mapricefs, mapricesl, topn) {
    # build the parameters based on GA indexes
    co.e = 3+length(deforbs)
    api.e = co.e+length(aspects)-1
    ae.e = api.e+length(aspects)
    pze.e = ae.e+lenZodEnergyMi
    spe.e = pze.e+topn

    # parameters vectors

    args <-list(securityfile=securityfile,
                planetsfile=planetsfile,
                predfile=predfile,
                tsdate=tsdate,
                tedate=tedate,
                vsdate=vsdate,
                vedate=vedate,
                mapricefs=mapricefs,
                mapricesl=mapricesl,
                topn=topn,
                fittype=fittype,
                dateformat=dateformat,
                verbose=F,
                doplot=F,
                plotsol=F,
                mapredsm=x[1],
                degsplit=x[2],
                cusorbs=x[3:(co.e-1)],
                aspectspolarity=x[co.e:(api.e-1)],
                aspectsenergy=adjustEnergy(x[api.e:(ae.e-1)]),
                planetszodenergy=adjustEnergy(x[ae.e:(pze.e-1)]),
                sigpenergy=adjustEnergy(x[pze.e:(spe.e-1)]))

    args <- setModelData(args)
    args <- setSigLons(args)
    args <- setParamsPAPAEPZSP(args)

    # Conjunction energy as neutral
    args$conpolarity <- FALSE

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

  relativeTrendExec <- function(x, ...) {
    # Build the params sets
    args <- processParamsPAPAEPZSP(x, ...)
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
    # join the security table with prediction
    planets.pred <- args$security[prediction]
    # smoth the prediction serie and remove resulting NAS
    # TODO: test correlation with lowess smooth
    #planets.pred[, predval := lowess(planets.pred$predRaw, f=1/200, delta=5)$y]
    planets.pred[, predval := SMA(predRaw, args$mapredsm)]
    planets.pred <- planets.pred[!is.na(predval),]
    # determine a factor prediction response
    planets.pred[, predFactor := cut(predval, c(-10000, 0, 10000), labels=c('down', 'up'), right=FALSE)]
    # Add the Year for projected predictions rows
    planets.pred[is.na(Year), Year := as.character(format(Date, "%Y"))]
    # helper function to process predictions by year
    pltitle <- paste('Yearly prediction VS price movement for ', args$securityfile)
    # Split data
    samples <- dataOptCVSampleSplit(args, planets.pred)
    # Calculate Fitness
    fitness <- calculateSamplesFitness(args, samples)
    #cat("\t Predict execution/loop time: ", proc.time()-ptm, " - ", proc.time()-looptm, "\n\n")
    return(list(fitness=fitness, pred=planets.pred))
  }

  optimizeRelativeTrend <- function(benchno, sectype, secsymbols, planetsfile, tsdate, tedate, vsdate, vedate, fittype,
                                    mapricefs, mapricesl, topn, dateformat) {
    cat("---------------------------- Initialize optimization ----------------------------------\n\n")
    orbsmin <- rep(0, length(deforbs))
    orbsmax <- deforbs
    polaritymin <- rep(0, length(aspects)-1)
    polaritymax <- rep(1, length(aspects)-1)
    aspectenergymin <- rep(0, length(aspects))
    aspectenergymax <- rep(30, length(aspects))
    planetzodenergymin <- rep(0, lenZodEnergyMi)
    planetzodenergymax <- rep(30, lenZodEnergyMi)
    sigpenergymin <- rep(0, topn)
    sigpenergymax <- rep(30, topn)

    minvals <- c( 2, 5, orbsmin, polaritymin, aspectenergymin, planetzodenergymin, sigpenergymin)
    maxvals <- c(10, 6, orbsmax, polaritymax, aspectenergymax, planetzodenergymax, sigpenergymax)

    # Clear the cache directory before start
    clearCache()

    # redirect the output to symbol sink file
    sinkpathfile <- npath(paste("~/trading/benchmarks/b", benchno, "_", sectype, ".txt", sep=''))
    # Redirect output to file
    #if (exists('sinkfile', envir=parent.frame())) {
    sink(sinkpathfile, append=T)
    cat("# version: ", branch.name, "\n")
    cat("#", tsdate, '-', tedate, 'FIT /', vsdate, '-', vedate, 'OPTwCV -', fittype, 'fit -', mapricefs, '-', mapricesl, 'MAS -', topn, 'TOPN', '\n\n')
    cat("setModernAspectsSet()\n")
    cat("bt <- list()\n")
    sink()

    for (symbol in secsymbols) {
      # Restart timer for each symbol GA optimization
      ptm <<- proc.time()
      cat("Starting GA optimization for ", symbol, " - ", sinkpathfile, "\n")

      # buid securityfile and predfile paths
      securityfile <- paste(sectype, symbol, sep="/")
      predfile <- paste('b', benchno, '/', symbol, '_', benchno, sep="")

      gar <- ga("real-valued", fitness=relativeTrendExec, parallel=TRUE, monitor=gaMonitor, maxiter=60, run=50, min=minvals, max=maxvals,
                popSize=1000, elitism = 100, pcrossover = 0.9, pmutation = 0.1,
                selection=gaint_rwSelection, mutation=gaint_raMutation, crossover=gaint_spCrossover, population=gaint_Population,
                topn=topn, securityfile=securityfile, planetsfile=planetsfile, predfile=predfile,
                tsdate=tsdate, tedate=tedate, vsdate=vsdate, vedate=vedate,
                fittype=fittype, mapricefs=mapricefs, mapricesl=mapricesl, dateformat=dateformat)

      # output the solution string
      sink(sinkpathfile, append=T)
      x <- gar@solution[1,]
      args <- processParamsPAPAEPZSP(x, securityfile, planetsfile, predfile, tsdate, tedate, vsdate, vedate, fittype, dateformat, mapricefs, mapricesl, topn)
      cat("res <-", args$strsol)
      cat("# Fitness = ", gar@fitnessValue, "\n")
      cat("bt$symbol <- testStrategy(openSecurityOnEnv(", shQuote(securityfile), "),", shQuote(benchno), shQuote(symbol), "res$pred)\n\n")
      sink()
    }
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
    args <- setSigLons(args)
    args <- setParamsPAPAEPZSP(args)
    # By default use conjunction neutral energy
    args$conpolarity <- FALSE

    return(args)
  }

  testSolution <- function(...) {
    args <- prepareSolutionArgs(...)
    if (args$doplot) pdf(args$predfile, width = 11, height = 8, family='Helvetica', pointsize=12)
    res <- relativeTrend(args)
    if (args$doplot) dev.off()
    # Return a response
    return(res)
  }

  testSolutionConPol <- function(...) {
    args <- prepareSolutionArgs(...)
    args$conpolarity <- TRUE
    if (args$doplot) pdf(args$predfile, width = 11, height = 8, family='Helvetica', pointsize=12)
    res <- relativeTrend(args)
    if (args$doplot) dev.off()
    # Return a response
    return(res)
  }

  execfunc <- get(get('execfunc'))
  execfunc(...)
}

# compile the function to byte code
topNSigAspectsModel <- cmpfun(cmpTopNSigAspectsModel)
