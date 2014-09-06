setModelData <- function(args) {
  args$rdates <- as.Date(with(args, c(tsdate, tedate, vsdate, vedate)))
  # open planets file and leave only needed cols for better speed
  planets <- openPlanets(args$planetsfile, deforbs, calcasps=F)
  args$planets <- planets[, c('Date', 'Year', 'wday', planetsLonCols), with=F]
  # load the security data and leave only needed cols
  security <- with(args, openSecurity(securityfile, mapricefs, mapricesl, dateformat, rdates[1]))
  args$security <- security[, c('Date', 'Year', 'Open', 'High', 'Low', 'Close', 'Mid', 'MidMAF', 'MidMAS', 'Eff'), with=F]
  return(args)
}

setSigLons <- function(args) {
  # build significant points vector
  siglons <- with(args, buildSignificantLongitudes(planets, security, degsplit, rdates[1], rdates[2]))
  # leave only the top N significant points
  args$siglons <- head(siglons, args$topn)
  return(args)
}

setParamsPAPAEPZSPMatrix <- function(args) {
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

  # Generate the string solution for for the given model parameters
  args$strsol <- with(args, paste("testPlanetsSignificanceRelative('testSolution'",
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

  args <- setModelData(args)
  args <- setSigLons(args)
  args <- setParamsPAPAEPZSPMatrix(args)

  return(args)
}

# aggregate the daily energy and apply it with the daily significance energy
# to calculate the final prediction
calculateUpDownEnergy <- function(energy.days) {
  # to prevent division by zero
  prediction <- energy.days[, list(down = sum(down), up = sum(up)), by='Date']
  #prediction[, Date := as.Date(Date, format="%Y-%m-%d")]
  prediction[, predRaw := (up-down)]
  return(prediction)
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

  if (args$doplot) {
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

# Split the data in optimization and CV by sample split
dataOptCVSampleSplit <- function(args, planets.pred) {
  # split data in optimization and cross validation
  planets.pred.opt <- planets.pred[1:round(nrow(planets.pred)/2),]
  planets.pred.cv <- planets.pred[round(nrow(planets.pred)/2):nrow(planets.pred),]
  # Identify years with alone observations that can affect the years fitness mean
  years.opt <- table(planets.pred.opt$Year)
  years.cv <- table(planets.pred.cv$Year)

  # When doplot is enabled use for confirmation all the available years
  if (args$doplot) {
    sample.opt <- planets.pred.opt[Year %in% names(years.opt[years.opt > 20]),]
    sample.cv <- planets.pred.cv[Year %in% names(years.cv[years.cv > 20]),]

    # plot CV years
    sp <- xts(sample.cv[, c('Open', 'High', 'Low', 'Close', 'predval'), with=F], order.by=sample.cv$Date)
    for (year in names(years.cv[years.cv > 20])) {
      barChart(sp, log.scale=T, subset=year, TA='addSMA(20, col="red");addSMA(40, col="green");addAspEnergy();
               addRSI(14);addPVLines("p",31,"green",c(1,2,3));addPVLines("v",31,"red",c(1,2,3))')
    }
  }
  else {
    # use sample of 50% optimization data
    sample.opt <- planets.pred.opt[Year %in% names(years.opt[years.opt > 20]),]
    sample.opt <- sample.opt[, .SD[sample(.N, round(nrow(sample.opt) * .5))]]
    # and 40% of cross validation data
    sample.cv <- planets.pred.cv[Year %in% names(years.cv[years.cv > 20]),]
    sample.cv <- sample.cv[, .SD[sample(.N, round(nrow(sample.cv) * .4))]]
  }

  return(list(opt=sample.opt, cv=sample.cv))
}

# process the daily aspects energy
dayAspectsEnergy <- function(args) {
  # aspects, orbs and longitudes in long format
  planets.pred.aspen <- with(args, meltedAndMergedDayAspects(planets, security, degsplit, rdates[1], rdates[2], rdates[3], rdates[4], topn))

  # Use only the separating aspects & applying with at much 1 deg of orb
  #planets.pred.aspen <- planets.pred.aspen[orbdir == 1 | (orbdir == -1 & orb <= 1 ),]

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
  planets.pred.aspen[, energy :=  aenergy * tenergy * spenergy]

  # use only aspects that are in the allowed orb for specific aspect
  # TODO: verify that the filtered aspects correspond to the maximum orb
  planets.pred.aspen <- planets.pred.aspen[orb <= args$cusorbs['orbs', aspect]]

  # Adjust conjuntion polarity based on involved planets: MA, SA, PL are
  # considered as a negative, others as positive.
  #planets.pred.aspen[polarity == 2 & origin %in% c('MA', 'SA', 'PL'), polarity := 0]
  #planets.pred.aspen[polarity == 2 & origin %ni% c('MA', 'SA', 'PL'), polarity := 1]

  # compute the given energy based on the aspect orb distance
  #planets.pred.aspen[, disenergy := energyGrowth(energy, orb)]
  # set energy up / down based on polarities
  planets.pred.aspen[polarity == 0, c('up', 'down') := list(0, energy)]
  planets.pred.aspen[polarity == 1, c('up', 'down') := list(energy, 0)]
  planets.pred.aspen[polarity == 2, c('up', 'down') := list(energy, energy)]

  return(planets.pred.aspen)
}

# Build the orbs, polarity, aspect energy, sigpoints energy and zodenergy matrix
# that are parameters needed to build the daily aspects energy.
calculateAZSPEN <- function(args) {
  # Calculate daily aspects energy for predict dates
  energy.days <- dayAspectsEnergy(args)

  return(energy.days)
}

# Build a long data table with daily aspects, orbs and longitudes
meltedAndMergedDayAspects <- function(planets, security, degsplit, tsdate, tedate, psdate, pedate, topn) {
  planetskey <- dataTableUniqueVector(planets)
  securitykey <- dataTableUniqueVector(security)
  ckey <- list(as.character(c('meltedAndMergedDayAspects', planetskey, securitykey, degsplit, tsdate, tedate, psdate, pedate, topn)))
  aspects.day.long <- secureLoadCache(key=ckey)

  if (is.null(aspects.day.long)) {
    # calculate daily aspects
    aspects.day <- buildSignificantLongitudesAspects(planets, security, degsplit, tsdate, tedate, topn, F)
    # leave only the aspects for prediction range dates
    aspects.day <- aspects.day[Date > psdate & Date <= pedate & wday %in% c(1, 2, 3, 4, 5)]

    # melt aspects
    aspects <- melt(aspects.day, id.var=c('Date', 'lon'), variable.name='origin',
                    value.name='aspect', value.factor=T, measure.var=planetsAspCols, na.rm=T)
    # remove ASP from the origin column name
    aspects[, origin := substr(origin, 1, 2)]

    # melt orbs
    orbs <- melt(aspects.day, id.var=c('Date', 'lon'), variable.name='origin',
                 value.name='orb', measure.var=planetsOrbCols)
    # remove ORBS from the origin column name
    orbs[, origin := substr(origin, 1, 2)]

    # melt longitudes
    longs <- melt(aspects.day, id.var=c('Date', 'lon'), variable.name='origin',
                  value.name='tlon', measure.var=planetsLonCols)
    # avoid lon 0 that cause 0 sign when divide celing(0/30)
    longs[tlon == 0, tlon := 1]
    # remove LON from the origin column name
    longs[, origin := substr(origin, 1, 2)]
    # Calculate zod signs for each transit planet
    longs[, tzsign := ceiling(tlon/30)]

    # join aspects & orbs & transit longs
    aspects.day.long <- merge(aspects, orbs, by=c('Date', 'lon', 'origin'))
    aspects.day.long <- merge(aspects.day.long, longs, by=c('Date', 'lon', 'origin'))

    # Use only the applicative aspects
    aspects.day.long[, orbdir := sign(orb - Lag(orb)), by=c('lon', 'origin', 'aspect')]
    aspects.day.long[orbdir == 0, orbdir := 1]
    # For the initial row that is NA due Lag orb calculation use the next row value
    aspects.day.long[, norbdir := Next(orbdir), by=c('lon', 'origin', 'aspect')]
    aspects.day.long[is.na(orbdir), orbdir := norbdir]
    aspects.day.long[, norbdir := NULL]

    # add up / down energy cols inintially to 0
    aspects.day.long[, c('up', 'down') := list(0, 0)]

    saveCache(aspects.day.long, key=ckey)
    cat("Set meltedAndMergedDayAspects cache\n")
  }

  return(aspects.day.long)
}

# Top N significant points aspects model
cmpTopNSigAspectsModel <- function(execfunc, ...) {
  if (!hasArg('execfunc')) stop("Provide function to execute")
  ptm <- proc.time()
  branch.name <- branchName()

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
      cat(args$strsol)
      cat("# Fitness = ", gar@fitnessValue, "\n\n")
      sink()
    }
  }

  testSolution <- function(...) {
    args <- list(...)
    if (!hasArg('dateformat')) stop("A dateformat is needed.")
    predfile <- paste("~/", args$predfile, ".pdf", sep="")

    # Create directory if do not exists
    if (!file.exists(dirname(predfile))) {
      dir.create(dirname(predfile), recursive=T)
    }

    if (args$doplot) pdf(predfile, width = 11, height = 8, family='Helvetica', pointsize=12)

    # Build the params sets
    args <- setModelData(args)
    args <- setSigLons(args)
    args <- setParamsPAPAEPZSPMatrix(args)
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
