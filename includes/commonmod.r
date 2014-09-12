#####################################################################################################
# Common models functions
####################################################################################################
bootstrapOptimization <- function(modenv, ...) {
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

bootstrapOptimizationIteration <- function(symbol, args) {
  # Restart timer for each symbol GA optimization
  ptm <<- proc.time()
  args$symbol <- symbol
  # buid securityfile and predfile paths
  args$securityfile <- with(args, paste(sectype, symbol, sep="/"))
  args$predfile <- with(args, paste('b', benchno, '/', symbol, '_', benchno, sep=""))
  # build natal longitudes
  args$siglons <- buildNatalLongitudes(args$symbol)
  # load the security data and leave only needed cols
  security <- with(args, openSecurity(securityfile, mapricefs, mapricesl, dateformat, tsdate))
  args$security <- security[, c('Date', 'Year', 'Open', 'High', 'Low', 'Close', 'Mid', 'MidMAF', 'MidMAS', 'Eff'), with=F]

  cat("Starting GA optimization for ", args$symbol, " - ", args$sinkpathfile, "\n")

  return(args)
}

loopSolutionGA <- function(gar, modenv, args) {
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

setMatrixOrbsPolarityAspSZodSiglonEnergy <- function(args) {
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
                                  ", paramsfunc=", shQuote(paramsfunc),
                                  ", fittype=", shQuote(fittype), ")\n", sep=""))

  return(args)
}
