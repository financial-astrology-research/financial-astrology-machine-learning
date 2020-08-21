#####################################################################################################
# Common models functions
####################################################################################################

optimizeGA <- function(args) {
  args <- bootstrapOptimization(args)

  for (symbol in args$secsymbols) {
    # process arguments
    args <- bootstrapSecurity(symbol, args)
    cat("Starting GA optimization for ", args$symbol, " - ", args$sinkpathfile, "\n")

    gar <- ga("real-valued", popSize=1000, elitism=100, pcrossover=0.9, pmutation=0.1, maxiter=100, run=50,
              fitness=modelFitExec, parallel=T, min=args$gamin, max=args$gamax, monitor=gaMonitor,
              selection=gaint_rwSelection, mutation=gaint_raMutation, crossover=gaint_spCrossover, population=gaint_Population, args=args)

    loopSolutionGA(gar, args)
  }

  exitOptimization(args)
}

bootstrapOptimization <- function(args) {
  args$branch=branchName()
  args$verbose=F
  args$doplot=F
  args$plotsol=F

  # Clear the cache directory before start
  clearCache()
  # Bootstrap model
  args <- get('bootstrapModel', envir=args$modenv)(args)
  # Build single line model params
  strmodparams <- '# '
  for (argname in names(args)) {
    if (typeof(args[[argname]]) %ni% c('list', 'environment') && length(args[[argname]])==1) {
      strmodparams <- paste(strmodparams, paste(argname, ' = ', args[[argname]], sep=''), sep=', ')
    }
  }

  # redirect the output to symbol sink file
  args$sinkpathfile <- with(args, npath(paste("~/trading/benchmarks/b", benchno, "_", sectype, ".txt", sep='')))

  # Redirect output to file
  #if (exists('sinkfile', envir=parent.frame())) {
  sink(args$sinkpathfile, append=T)
  cat(strmodparams, "\n")
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
  args$predfile <- with(args, paste('~/b', benchno, '/', symbol, '_', benchno, '.pdf', sep=""))
  # load the security data and leave only needed cols
  security <- with(args, openSecurity(securityfile, mapricefs, mapricesl, dateformat, tsdate))
  args$security <- security[, c('Date', 'Year', 'Open', 'High', 'Low', 'Close', 'Mid', 'MidMAF', 'MidMAS', 'Eff'), with=F]
  # load the security data in back testing format
  args$secdata <- openSecurityOnEnv(args$securityfile)

  if (args$model == 'natalAspectsModel') {
    # build natal longitudes
    args$siglons <- buildNatalLongitudes(args$symbol)
  }
  else if (args$model == 'topNSigAspectsModel') {
    # build topn significant longitudes
    siglons <- with(args, buildSignificantLongitudes(planets, security, degsplit, tsdate, tedate))
    # leave only the top N significant points
    args$siglons <- head(siglons, args$topn)
  }
  else if (args$model == 'daySignificantAspectsModel') {
    # No signlons for this model
    args$siglons <- c()
  }
  else {
    stop("Not valid model was provided.")
  }

  return(args)
}

loopSolutionGA <- function(gar, args) {
  # output the solution string
  sink(args$sinkpathfile, append=T)
  args$x <- gar@solution[1,]
  # Execute the appropriate params function
  args <- get(args$paramsfunc)('splitX', args)
  # Print solution / fitness / BT call
  cat("res <-", args$strsol)
  cat("# Fitness=", gar@fitnessValue, "\n")
  with(args, cat("bt$", symbol, " <- testStrategy(openSecurityOnEnv(", shQuote(securityfile), "), ",
                 shQuote(benchno), ', ', shQuote(symbol), ", res$pred)\n\n", sep=''))
  sink()
}

modelFitExec <- function(x, args) {
  args$x <- x
  # Execute the appropriate params function
  args <- get(args$paramsfunc)('splitX', args)
  # Execute
  res <- get(args$fitfunc)(args)
  # Return only the fitness
  return(res$fitness)
}

# process the daily aspects energy
dayAspectsEnergy <- function(args) {
  # Use the appropriate daily aspects
  if (args$model == 'topNSigAspectsModel') {
    # significant longitude points aspects
    aspects.day <- with(args, buildSignificantLongitudesAspects(planets, security, degsplit, tsdate, tedate, topn, F))
  }
  else if (args$model == 'natalAspectsModel') {
    # natal points aspects
    aspects.day <- with(args, buildNatalLongitudeAspects(symbol, planets, F))
  }
  else {
    stop("Not valid model was provided.")
  }

  planets.pred.aspen <- with(args, meltedAndMergedDayAspects(aspects.day, planets, security, vsdate, vedate))

  if (args$asptype == 'sep') {
    # Use only the separating aspects & applying with at much 1 deg of orb
    planets.pred.aspen <- planets.pred.aspen[orbdir == 1 | (orbdir == -1 & orb <= 1 ),]
  }
  else if (args$asptype == 'app') {
    # Use only the applying aspects & separating with at much 1 deg of orb
    planets.pred.aspen <- planets.pred.aspen[orbdir == -1 | (orbdir == 1 & orb <= 1 ),]
  }

  # Add the aspects polarity
  planets.pred.aspen[, polarity := args$aspectspolarity['polarity', aspect]]
  # Calculate the transit planet zoodiacal energy
  processAspEnergy <- function(asp.row, by.row) {
    args$planetszodenergy[by.row[[1]], asp.row[[1]]]
  }

  # Set columns with transit zodiacal energy / aspect energy / sigpoints energy
  planets.pred.aspen[, tenergy := processAspEnergy(.SD, .BY), by=c('origin'), .SDcols=c('tzsign')]
  planets.pred.aspen[, aenergy := args$aspectsenergy['energy', aspect], by=c('aspect')]
  planets.pred.aspen[, spenergy := args$sigpenergy['energy', as.character(lon)], by=c('lon')]

  # Calculate the energy considering significant point / transit / aspect energy
  if (args$enoperation == '*') {
    planets.pred.aspen[, energy :=  aenergy * tenergy * spenergy]
  }
  else if (args$enoperation == '+') {
    planets.pred.aspen[, energy :=  aenergy + tenergy + spenergy]
  }
  else {
    stop('Not valid enoperation configured.')
  }

  # use only aspects that are in the allowed orb for specific aspect
  # TODO: verify that the filtered aspects correspond to the maximum orb
  planets.pred.aspen <- planets.pred.aspen[orb <= args$cusorbs['orbs', aspect]]

  if (args$aspectspolarity['polarity', '0'] == 3) {
    # Adjust conjuntion polarity based on involved planets: MA, SA, PL are
    # considered as a negative, others as positive.
    planets.pred.aspen[polarity == 3 & origin %in% c('MA', 'SA', 'PL'), polarity := 0]
    planets.pred.aspen[polarity == 3 & origin %ni% c('MA', 'SA', 'PL'), polarity := 1]
  }
  else if (args$aspectspolarity['polarity', '0'] == 4) {
    planets.pred.aspen[polarity == 4 & origin %in% c('MA', 'SA', 'UR', 'PL'), polarity := 0]
    planets.pred.aspen[polarity == 4 & origin %ni% c('MA', 'SA', 'UR', 'PL'), polarity := 1]
  }
  else if (args$aspectspolarity['polarity', '0'] == 5) {
    planets.pred.aspen[polarity == 5 & origin %in% c('MA', 'SA', 'UR', 'NE', 'PL'), polarity := 0]
    planets.pred.aspen[polarity == 5 & origin %ni% c('MA', 'SA', 'UR', 'NE', 'PL'), polarity := 1]
  }

  if (args$engrowth) {
    # compute the given energy based on the aspect orb distance
    planets.pred.aspen[, energy := energyDecay(energy, orb, 0.2)]
  }

  # set energy up / down based on polarities
  planets.pred.aspen[polarity == 0, c('up', 'down') := list(0, energy)]
  planets.pred.aspen[polarity == 1, c('up', 'down') := list(energy, 0)]
  planets.pred.aspen[polarity == 2, c('up', 'down') := list(energy, energy)]

  return(planets.pred.aspen)
}

# Process the yearly predictions and calculate the sample fitness
calculateSamplesFitness <- function(args, samples) {
  # compute test predictions by year
  res.test <- samples$opt[, processPredictions(.SD), by=Year]
  resMean <- function(x) round(mean(x), digits=2)
  res.test.mean <- res.test[, list(correlation=resMean(correlation), volatility=resMean(volatility), matches.t=resMean(matches.t))]
  # compute confirmation predictions by year
  res.conf <- samples$cv[, processPredictions(.SD), by=Year]
  res.conf.mean <- res.conf[, list(correlation=resMean(correlation), volatility=resMean(volatility), matches.t=resMean(matches.t))]

  # use appropriate fitness type
  if (args$fittype == 'matches') {
    fitness <- round((res.test.mean$matches.t + res.conf.mean$matches.t) / 2, digits=0)
  }
  else if (args$fittype == 'sdmatches') {
    matches.mean <- mean(c(res.test$matches.t, res.conf$matches.t))
    matches.sd <- sd(c(res.test$matches.t, res.conf$matches.t))
    if (matches.sd == 0) {
      fitness <- -abs(1 / (matches.mean^2)) * 100
    }
    else {
      fitness <- -abs(matches.sd / (matches.mean^2)) * 100
    }
  }
  else if (args$fittype == 'matcor') {
    correlation <- round((res.test.mean$correlation + res.conf.mean$correlation) / 2, digits=3)
    matches <- round((res.test.mean$matches.t + res.conf.mean$matches.t) / 2, digits=3)
    fitness <- (matches + correlation) / 2
  }
  else {
    stop("No valid fittype provided")
  }

  if (args$verbose) {
    # plot solution snippet if doplot is enabled
    if (args$plotsol) {
      snippet <- paste(strwrap(sout, width=170), collapse="\n")
      plotSolutionSnippet(snippet)
    }

    mout <- with(args, capture.output(print(cusorbs),
                                      print(aspectspolarity),
                                      print(aspectsenergy),
                                      print(sigpenergy),
                                      print(planetszodenergy)))

    # print buffered output
    cat(args$strsol, mout, "\n", sep="\n")

    # print yearly summary
    apply(res.test, 1, printPredYearSummary, type="Optimization")
    with(res.test.mean, cat("\tvol =", volatility, " - cor =", correlation, " - matches.t =", matches.t, "\n"))
    apply(res.conf, 1, printPredYearSummary, type="Confirmation")
    with(res.conf.mean, cat("\tvol =", volatility, " - cor =", correlation, " - matches.t =", matches.t, "\n"))

    # totals and execution time
    cat("\n\t Totals: fitness = ", fitness, "\n")
    cat("\t Optimized and confirmed with: ", nrow(samples$opt) + nrow(samples$cv), " days", "\n")
  }

  return(fitness)
}

modelCalculateFitness <- function(args, prediction) {
  # join the security table with prediction and remove NAS caused by join
  planets.pred <- args$security[prediction]
  # smoth the prediction serie and remove resulting NAS
  planets.pred[, predval := SMA(predRaw, args$mapredsm)]
  planets.pred <- planets.pred[!is.na(predval),]
  # determine a factor prediction response
  planets.pred[, predFactor := cut(predval, c(-10000, 0, 10000), labels=c('down', 'up'), right=F)]
  # Add the Year for projected predictions rows
  planets.pred[is.na(Year), Year := as.character(format(Date, "%Y"))]
  # Split data using the appropriate function
  samples <- get(args$datasplitfunc)(args, planets.pred)
  # Remove the projected prediction data
  samples$cv <- samples$cv[!is.na(Mid),]
  planets.pred <- planets.pred[!is.na(Mid),]
  # Calculate Fitness
  fitness <- calculateSamplesFitness(args, samples)
  #cat("\t Predict execution/loop time: ", proc.time()-ptm, " - ", proc.time()-looptm, "\n\n")
  return(list(fitness=fitness, pred=planets.pred))
}

modelAspectsEnergy <- function(args) {
  looptm <- proc.time()
  # calculate energy days
  energy.days <- dayAspectsEnergy(args)
  # Calculate prediction
  prediction <- calculateUpDownEnergy(energy.days)
  # calculate fitness
  return(modelCalculateFitness(args, prediction))
}

predictionsBackTest <- function(planets.pred, args) {
  bt <- testYearStrategy(args$secdata, args$benchno, args$symbol, planets.pred, paste(min(planets.pred$Date), max(planets.pred$Date), sep='::'))
  return(list(fitness = bt$astro.valley.sma$cagr))
}

modelAspectsEnergyBackTest <- function(args) {
  looptm <- proc.time()
  # calculate energy days
  energy.days <- dayAspectsEnergy(args)
  # Calculate prediction
  prediction <- calculateUpDownEnergy(energy.days)

  # join the security table with prediction and remove NAS caused by join
  planets.pred <- args$security[prediction]
  # smoth the prediction serie and remove resulting NAS
  planets.pred[, predval := SMA(predRaw, args$mapredsm)]
  planets.pred <- planets.pred[!is.na(predval),]
  # Add the Year for projected predictions rows
  planets.pred[is.na(Year), Year := as.character(format(Date, "%Y"))]
  # Split data using the appropriate function
  samples <- get(args$datasplitfunc)(args, planets.pred)
  # Remove the projected prediction data
  samples$opt <- samples$opt[!is.na(Mid),]
  samples$cv <- samples$cv[!is.na(Mid),]

  # Calculate Fitness
  resMean <- function(x) round(mean(x), digits=2)
  # compute test predictions by year
  res.test <- samples$opt[, predictionsBackTest(.SD, args), by=Year]
  # compute confirmation predictions by year
  res.conf <- samples$cv[, predictionsBackTest(.SD, args), by=Year]

  # round fitness
  fitness <- round((mean(res.test$fitness) + mean(res.conf$fitness)) / 2, digits=3)

  if (args$verbose) {
    mout <- with(args, capture.output(print(cusorbs),
                                      print(aspectspolarity),
                                      print(aspectsenergy),
                                      print(sigpenergy),
                                      print(planetszodenergy)))

    # print buffered output
    cat(args$strsol, mout, "\n", sep="\n")

    # totals and execution time
    cat("\n\t Totals: fitness = ", fitness, "\n")
    cat("\t Optimized and confirmed with: ", nrow(samples$opt) + nrow(samples$cv), " days", "\n")
  }

  return(list(fitness=fitness, pred=planets.pred))
}

# Build args list for aspects polarity, aspects energy, zodenergy, significant points energy
paramsPolarityAspZodSiglonEnergy <- function(func, args) {
  # build the parameters based on GA indexes
  splitX <- function(args) {
    # gamixedidx+1 is due indexes start at 1 not 0 so we need to select after that index
    co.e=args$gamixedidx+1+length(deforbs)

    # Calculate Aspects Polarity Index only if aspects polarities are enabled
    if (args$aspolarity) {
      api.e=co.e+length(aspects)
      args$aspectspolarity <- args$x[co.e:(api.e-1)]
    }
    else {
      api.e=co.e
      args$aspectspolarity <- defpolarities
    }

    ae.e=api.e+length(aspects)
    pze.e=ae.e+lenZodEnergyMi
    # 14 natal points
    spe.e=pze.e+14

    if (args$model == 'natalAspectsModel') {
      args$mapredsm <- args$x[1]
    }
    else if (args$model == 'topNSigAspectsModel') {
      args$mapredsm <- args$x[1]
      args$degsplit <- args$x[2]
    }
    else if (args$model == 'daySignificantAspectsModel') {
      args$mapredsm <- x[1]
      args$degsplit <- args$x[2]
      args$threshold <- x[3]/100
      args$energymode <- x[4]
    }
    else {
      stop("Not valid model was provided.")
    }

    args$cusorbs <- args$x[(args$gamixedidx+1):(co.e-1)]
    args$aspectsenergy <- adjustEnergy(args$x[api.e:(ae.e-1)])
    args$planetszodenergy <- adjustEnergy(args$x[ae.e:(pze.e-1)])
    args$sigpenergy <- adjustEnergy(args$x[pze.e:(spe.e-1)])

    # Set majors planets zodenergy all to 1 due we don't have historical data for a complete cycle
    # so in this case we only allow the planets aspects energy and polarity to act on.
    majorsen <- sum(planetsBaseCols %in% planetsMajors) * length(zodSignsCols)

    if (majorsen > 0) {
      args$planetszodenergy <- c(args$planetszodenergy, rep(1, majorsen))
    }

    args <- setMatrix(args)

    return(args)
  }

  # Build the matrix parameters
  setMatrix <- function(args) {
    # Generate the string solution for the given model parameters
    args$strsol <- with(args, paste(paste(modelfunc), "('testSolution'",
                                    ", benchno=", shQuote(benchno),
                                    ", symbol=", shQuote(symbol),
                                    ", sectype=", shQuote(sectype),
                                    ", securityfile=", shQuote(securityfile),
                                    ", planetsfile=", shQuote(planetsfile),
                                    ", tsdate=", shQuote(tsdate), ", tedate=", shQuote(tedate),
                                    ", vsdate=", shQuote(vsdate), ", vedate=", shQuote(vedate),
                                    ", mapredsm=", mapredsm, ", mapricefs=", mapricefs, ", mapricesl=", mapricesl,
                                    ", cusorbs=c(", paste(cusorbs, collapse=", "), ")",
                                    ", aspectsenergy=c(", paste(aspectsenergy, collapse=", "), ")",
                                    ", sigpenergy=c(", paste(sigpenergy, collapse=", "), ")",
                                    ", planetszodenergy=c(", paste(planetszodenergy, collapse=", "), ")",
                                    ", aspectspolarity=c(", paste(aspectspolarity, collapse=", "), ")",
                                    ", dateformat=", shQuote(dateformat),
                                    ", fittype=", shQuote(fittype), ")\n", sep=""))

    # Build the matrices after build solution, otherwise the paste maxtrix implode values by cols
    # instead by rows, resulting in totally different solution.
    args$cusorbs <- matrix(args$cusorbs, nrow=1, ncol=length(aspects), byrow=T,
                           dimnames=list('orbs', aspects))

    args$aspectspolarity <- matrix(args$aspectspolarity, nrow=1, ncol=length(aspects), byrow=T,
                                   dimnames=list('polarity', aspects))

    args$aspectsenergy <- matrix(args$aspectsenergy, nrow=1, ncol=length(args$aspectsenergy), byrow=T,
                                 dimnames=list(c('energy'), aspects))

    if (!is.null(args$sigpenergy)) {
      args$sigpenergy <- matrix(args$sigpenergy, nrow=1, ncol=length(args$sigpenergy), byrow=T,
                                dimnames=list(c('energy'), args$siglons$lon))
    }
    else {
      args$sigpenergy <- ''
    }

    args$planetszodenergy <- matrix(args$planetszodenergy, nrow=length(planetsBaseCols), ncol=12, byrow=T,
                                    dimnames=list(planetsBaseCols, zodSignsCols))
    return(args)
  }

  gaMinMax <- function(args) {
    orbsmin <- rep(0, length(deforbs))
    orbsmax <- deforbs
    aspectenergymin <- rep(0, length(aspects))
    aspectenergymax <- rep(30, length(aspects))
    planetzodenergymin <- rep(0, lenZodEnergyMi)
    planetzodenergymax <- rep(30, lenZodEnergyMi)
    polaritymin <- c()
    polaritymax <- c()

    if (args$aspolarity) {
      polaritymin <- c(2, rep(0, length(aspects)-1))
      polaritymax <- c(2, rep(1, length(aspects)-1))
    }

    if (args$model == 'natalAspectsModel') {
      # 14 natal points
      sigpenergymin <- rep(0, 14)
      sigpenergymax <- rep(30, 14)
      args$gamixedidx <- 1
      mixedmin <- c(2)
      mixedmax <- c(10)
    }
    else if (args$model == 'topNSigAspectsModel') {
      sigpenergymin <- rep(0, args$topn)
      sigpenergymax <- rep(30, args$topn)
      args$gamixedidx <- 2
      mixedmin <- c(2,  4)
      mixedmax <- c(10, 6)
    }
    else if (args$model == 'daySignificantAspectsModel') {
      sigpenergymin <- c()
      sigpenergymax <- c()
      args$gamixedidx <- 2
      mixedmin <- c(2,  4)
      mixedmax <- c(10, 6)
    }
    else {
      stop("Not valid model was provided.")
    }

    # min/max ranges
    args$gamin <- c(mixedmin, orbsmin, polaritymin, aspectenergymin, planetzodenergymin, sigpenergymin)
    args$gamax <- c(mixedmax, orbsmax, polaritymax, aspectenergymax, planetzodenergymax, sigpenergymax)

    return(args)
  }

  get(get('func'))(args)
}

testSolution <- function(args) {
  if (is.null(args$dateformat)) stop("A dateformat is needed.")
  # Build the params sets
  args <- execfunc('bootstrapModel', args)
  args <- bootstrapSecurity(args$symbol, args)
  args <- get('paramsPolarityAspZodSiglonEnergy')('setMatrix', args)
  args$verbose <- T
  args$doplot <- T
  args$plotsol <- F

  # Create directory if do not exists
  if (!file.exists(dirname(args$predfile))) {
    dir.create(dirname(args$predfile), recursive=T)
  }

  if (args$doplot) pdf(args$predfile, width=11, height=8, family='Helvetica', pointsize=12)
  res <- get(args$fitfunc)(args)
  if (args$doplot) dev.off()
  # Return a response
  return(res)
}
