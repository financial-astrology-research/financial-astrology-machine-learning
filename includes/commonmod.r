#####################################################################################################
# Common models functions
####################################################################################################
bootstrapOptimization <- function(modenv, ...) {
  args <- list(...)
  args <- with(args, list(branch=branchName(),
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
  # Bootstrap model
  args <- get('bootstrapModel', envir=modenv)(args)
  # redirect the output to symbol sink file
  args$sinkpathfile <- with(args, npath(paste("~/trading/benchmarks/b", benchno, "_", sectype, ".txt", sep='')))

  # Redirect output to file
  #if (exists('sinkfile', envir=parent.frame())) {
  sink(args$sinkpathfile, append=T)
  cat("# version: ", args$branch, "\n")
  cat(args$strmodparams, "\n")
  cat("setModernAspectsSet()\n")
  cat("bt <- list()\n\n")
  sink()

  return(args)
}

# Executed when GA optimization finished
exitOptimization <- function(args) {
  # Print the BT execution lines
  sink(args$sinkpathfile, append=T)
  cat("\n# Backtest summary\n")
  cat("testStrategyAllMean(bt)\n");
  sink()
}

bootstrapSecurity <- function(symbol, args) {
  # Restart timer for each symbol GA optimization
  ptm <<- proc.time()
  args$symbol <- symbol
  # buid securityfile and predfile paths
  args$securityfile <- with(args, paste(sectype, symbol, sep="/"))
  args$predfile <- with(args, paste('~/b', benchno, '/', symbol, '_', benchno, sep=""))
  # build natal longitudes
  args$siglons <- buildNatalLongitudes(args$symbol)
  # load the security data and leave only needed cols
  security <- with(args, openSecurity(securityfile, mapricefs, mapricesl, dateformat, tsdate))
  args$security <- security[, c('Date', 'Year', 'Open', 'High', 'Low', 'Close', 'Mid', 'MidMAF', 'MidMAS', 'Eff'), with=F]

  return(args)
}

loopSolutionGA <- function(modenv, gar, args) {
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

# Build args list for aspects polarity, aspects energy, zodenergy, significant points energy
paramsPolarityAspZodSiglonEnergy <- function(func, args) {
  # build the parameters based on GA indexes
  splitX <- function(args) {
    co.e=2+length(deforbs)
    api.e=co.e+length(aspects)-1
    ae.e=api.e+length(aspects)
    pze.e=ae.e+lenZodEnergyMi
    # 14 natal points
    spe.e=pze.e+14

    args$mapredsm <- args$x[1]
    args$cusorbs <- args$x[2:(co.e-1)]
    args$aspectspolarity <- args$x[co.e:(api.e-1)]
    args$aspectsenergy <- adjustEnergy(args$x[api.e:(ae.e-1)])
    args$planetszodenergy <- adjustEnergy(args$x[ae.e:(pze.e-1)])
    args$sigpenergy <- adjustEnergy(args$x[pze.e:(spe.e-1)])

    # Conjunction energy as neutral
    args$aspectspolarity <- c(2, args$aspectspolarity)
    args <- setMatrix(args)

    return(args)
  }

  # Build the matrix parameters
  setMatrix <- function(args) {
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
                                    ", benchno=", shQuote(benchno),
                                    ", symbol=", shQuote(symbol),
                                    ", sectype=", shQuote(sectype),
                                    ", securityfile=", shQuote(securityfile),
                                    ", planetsfile=", shQuote(planetsfile),
                                    ", tsdate=", shQuote(tsdate), ", tedate=", shQuote(tedate),
                                    ", mapredsm=", mapredsm, ", mapricefs=", mapricefs, ", mapricesl=", mapricesl,
                                    ", cusorbs=c(", paste(cusorbs, collapse=", "), ")",
                                    ", aspectsenergy=c(", paste(aspectsenergy, collapse=", "), ")",
                                    ", sigpenergy=c(", paste(sigpenergy, collapse=", "), ")",
                                    ", planetszodenergy=c(", paste(planetszodenergy, collapse=", "), ")",
                                    ", aspectspolarity=c(", paste(aspectspolarity, collapse=", "), ")",
                                    ", dateformat=", shQuote(dateformat),
                                    ", paramsfunc=", shQuote(paramsfunc),
                                    ", fittype=", shQuote(fittype), ")\n", sep=""))

    return(args)
  }

  gaMinMax <- function(args) {
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
    args$gamin <- c( 2, orbsmin, polaritymin, aspectenergymin, planetzodenergymin, sigpenergymin)
    args$gamax <- c(10, orbsmax, polaritymax, aspectenergymax, planetzodenergymax, sigpenergymax)

    return(args)
  }

  get(get('func'))(args)
}
