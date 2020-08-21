modelDaySignificantAspectsModel <- function(args) {
  # process the daily aspects energy
  dayAspectsEnergy <- function(planets.pred) {
    # melt aspects
    planets.pred.aspects <- melt(planets.pred, id.var=c('Date'), variable.name='origin',
                                 value.name='aspect', value.factor=T, measure.var=planetsCombAsp, na.rm=T)
    # remove ASP from the origin column name
    planets.pred.aspects[, origin := substr(origin, 1, 4)]
    # melt orbs
    planets.pred.orbs <- melt(planets.pred, id.var=c('Date'), variable.name='origin', value.name='orb',
                              measure.var=planetsCombOrb)
    # remove ORBS from the origin column name
    planets.pred.orbs[, origin := substr(origin, 1, 4)]
    # join aspects & orbs
    planets.pred.aspen <- merge(planets.pred.aspects, planets.pred.orbs, by=c('Date', 'origin'))
    # add up / down energy cols inintially to 0
    planets.pred.aspen[, c('up', 'down') := list(0, 0)]
    # planets cols involved in the aspect
    planets.pred.aspen[, plaone := substr(origin, 1, 2)]
    planets.pred.aspen[, platwo := substr(origin, 3, 4)]
    planets.pred.aspen[, polarity := args$aspectspolarity['polarity', aspect]]
    # Energy is given to the two involved planets in an aspect then we need two tables
    planets.pred.aspen.one <- copy(planets.pred.aspen)
    planets.pred.aspen.one <- planets.pred.aspen.one[, origin := plaone]
    planets.pred.aspen.one[, energy := args$aspectsenergy['energy', aspect]]
    planets.pred.aspen.two <- copy(planets.pred.aspen)
    planets.pred.aspen.two <- planets.pred.aspen.two[, origin := platwo]
    planets.pred.aspen.two[, energy := args$aspectsenergy['energy', aspect]]
    # join the two aspects cols
    planets.pred.aspen <- rbind(planets.pred.aspen.one, planets.pred.aspen.two)
    # use only aspects that are in the allowed orb for specific aspect
    # TODO: verify that the filtered aspects correspond to the maximum orb
    planets.pred.aspen <- planets.pred.aspen[orb <= args$cusorbs['orbs', aspect]]
    # compute the given energy based on the aspect orb distance
    planets.pred.aspen[, disenergy := energyDecay(energy, orb)]
    # set energy up / down based on polarities
    planets.pred.aspen[polarity == 0, c('up', 'down') := list(0, disenergy)]
    planets.pred.aspen[polarity == 1, c('up', 'down') := list(disenergy, 0)]
    planets.pred.aspen[polarity == 2, c('up', 'down') := list(disenergy, disenergy)]

    return(planets.pred.aspen)
  }

  # aggregate the daily energy and apply it with the daily significance energy
  # to calculate the final prediction
  calculatePrediction <- function(significance.days, energy.days, energymode) {
    energy.sum <- energy.days[, list(sum(up), sum(down)), by=list(Date, origin)]
    energy.sum[, Date := as.character(Date)]
    setnames(energy.sum, c('Date', 'origin', 'up', 'down'))
    setkeyv(energy.sum, c('Date', 'origin'))
    significance.days[, Date := as.character(Date)]
    significance.days <- merge(significance.days, energy.sum, by=c('Date', 'origin'))
    setkeyv(significance.days, c('Date', 'Eff'))
    significance.days[, c('up', 'down') := list(up * abs(energy), down * abs(energy))]

    if (energymode == 1) {
      # add more energy to the lower part based on bad aspects and to the upper part with good aspects
      # energy influence by count
      significance.days[Eff == 'up', c('energy1', 'energy2') := list(down, up)]
      significance.days[Eff == 'down', c('energy1', 'energy2') := list(up, down)]
    }
    else if (energymode == 2) {
      # add more energy to the lower part based on good aspects and to the upper part with bad aspects
      # energy influence by countu
      significance.days[Eff == 'down', c('energy1', 'energy2') := list(down, up)]
      significance.days[Eff == 'up', c('energy1', 'energy2') := list(up, down)]
    }
    else {
      stop("No valid energy mode was provided.")
    }

    # to prevent division by zero
    significance.days[, c('PE1', 'PE2') := list(V1 * energy1, V2 * energy2)]
    prediction <- significance.days[, list(down = sum(PE1)/sum(energy1), up = sum(PE2)/sum(energy2)), by='Date']
    prediction[is.nan(down), down := 0]
    prediction[is.nan(up), up := 0]
    prediction[, Date := as.Date(Date, format="%Y-%m-%d")]
    prediction[, predRaw := (up-down) * 100]
    return(prediction)
  }

  planetsVarsSignificanceFilter <- function(significance, threshold) {
    significance <- significance[pdiff >= threshold | pdiff <= -threshold]
    significance <- significance[!is.na(key)]
    significance[, Eff := cut(pdiff, c(-10000, 0, 10000), labels=c('down', 'up'), right=FALSE)]
    return(significance)
  }

  # build the daily signficance table
  buildDailySignificance <- function(significance, planets) {
    # build daily significance indexes
    buildDailySignificanceIdxs <- function(planets.day) {
      curdate <- planets.day[['Date']]
      sigidxs <- paste(planets.day[planetsLonGCols], planetsBaseCols, sep='_')
      datelist <- rep(curdate, length(sigidxs))
      day.idxs <- list(Date=datelist, keyidx=sigidxs)
      return(day.idxs)
    }

    ckey <- list('buildDailySignificance', args$securityfile, significance, planets)
    significance.days <- loadCache(key=ckey, dirs=c(args$securityfile), onError='print')
    if (is.null(significance.days)) {
      pathname <- findCache(key=ckey, dirs=c(args$securityfile))
      if (!is.null(pathname)) file.remove(pathname)

      significance.days.idxs <- rbindlist(apply(planets, 1, buildDailySignificanceIdxs))
      significance.days <- merge(significance.days.idxs, significance, by=c('keyidx'))
      significance.days[, zsign := ceiling(lon/30)]
      setkeyv(significance.days, 'Date', 'origin')

      saveCache(significance.days, key=ckey, dirs=c(args$securityfile))
      cat("Set buildDailySignificance cache\n")
    }

    return(significance.days)
  }

  # process the significance rows energy
  dailySignificanceEnergy <- function(significance.days) {
    processSignificanceRow <- function(significance.row, by.row) {
      # add the planet zodiacal energy
      args$planetszodenergy[by.row[[1]], significance.row$zsign]
    }

    # process each significance row
    significance.days[, energy := processSignificanceRow(.SD, .BY), by=c('origin')]
    # invert the significance columns for negative energy
    significance.days[energy < 0, c('V1', 'V2', 'V3', 'V4') := list(V2, V1, V4, V3)]
    return(significance.days)
  }

  # open planets file
  planets <- with(args, mainProcessPlanetsDegSplit(planets, degsplit))
  # split in training and prediction
  planets.train <- planets[Date > args$tsdate & Date <= args$tedate & wday %in% c(1, 2, 3, 4, 5)]
  planets.pred <- planets[Date > args$vsdate & Date <= args$vedate & wday %in% c(1, 2, 3, 4, 5)]
  # process the planets significance table
  significance <- mainPlanetsVarsSignificance(planets.train, args$security)
  # filter the significance by threshold
  significance <- planetsVarsSignificanceFilter(significance, args$threshold)
  # build significance by days
  significance.days <- buildDailySignificance(significance, planets.pred)
  # Calculate daily significance energy and patterns (no cache due is fast to calculate
  significance.daysen <- dailySignificanceEnergy(significance.days)
  # Daily aspects energy
  energy.days <- dayAspectsEnergy(planets.pred)
  # calculate prediction
  prediction <- calculatePrediction(significance.daysen, energy.days, args$energymode)
  # calculate fitness
  return(modelCalculateFitness(args, prediction))
}

#####################################################################################################
# Day Significant threshold aspects model
####################################################################################################
daySignificantAspectsModelCommon <- function(args) {
  args$tsdate <- as.Date(args$tsdate)
  args$tedate <- as.Date(args$tedate)
  args$vsdate <- as.Date(args$vsdate)
  args$vedate <- as.Date(args$vedate)
  # common settings
  args$model <- 'daySignificantAspectsModel'
  args$fitfunc <- 'modelDaySignificantAspectsModel'
  args$datasplitfunc <- 'dataOptCVSampleSplit'
  args$conpolarity <- F
  #args$verbose <- T
  # Init the GA min/max
  args <- paramsPolarityAspZodSiglonEnergy('gaMinMax', args)
  # open planets file and leave only needed cols for better speed
  args$planets <- openPlanets(args$planetsfile, deforbs, calcasps=T)

  return(args)
}

daySignificantAspectsModel <- function(func, ...) {
  if (!hasArg('func')) stop("Provide function to execute")
  ptm <- proc.time()

  bootstrapModel <- function(args) {
    # model settings
    setAll2AspectsSet()
    args$model <- 'natalAspectsModel'
    args$paramsfunc <- 'paramsPolarityAspZodSiglonEnergy'
    args <- daySignificantAspectsModelCommon(args)

    return(args)
  }

  args <- list(...)
  args$modenv <- environment()
  execfunc(get('func'), args)
}
