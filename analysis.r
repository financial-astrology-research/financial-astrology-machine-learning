library(GA)
library(R.cache)
library(compiler)
library(data.table)
library(ggplot2)
library(microbenchmark)
library(quantmod)
library(reshape2)
library(splus2R)
library(stringr)
# no scientific notation
options(scipen=100)
options(width=130)
options(error=recover)
enableJIT(0)
startDate = as.Date("1970-01-01")
maxretry <- 1

`%ni%` <- Negate(`%in%`)
planetsBaseCols <- c('SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'NN', 'SA')
# Aspects and orbs
aspects            <-  c(0,30,36,40,45,51,60,72,80,90,103,108,120,135,144,150,154,160,180)
deforbs            <- c(12, 2, 2, 2, 2, 2, 7, 2, 2, 7,  2,  2,  7,  2,  2,  2,  2,  2, 12)

# columns names
buildPlanetsColsNames <- function(planetsBaseCols) {
  aspOrbsCols <<- as.character(apply(expand.grid(aspects, planetsBaseCols[1:(length(planetsBaseCols)-1)]), 1, function(x) paste(x[2], x[1], sep='')))
  planetsLonCols <<- paste(planetsBaseCols, 'LON', sep='')
  planetsLonDisCols <<- paste(planetsBaseCols, 'DIS', sep='')
  planetsLonOrbCols <<- paste(planetsBaseCols, 'ORB', sep='')
  planetsLonAspCols <<- paste(planetsBaseCols, 'ASP', sep='')
  planetsAspCols <<- paste(planetsBaseCols, 'ASP', sep='')
  planetsDecCols <<- paste(planetsBaseCols, 'DEC', sep='')
  planetsLonGCols <<- paste(planetsLonCols, 'G', sep='')
  planetsSpCols <<- paste(planetsBaseCols, 'SP', sep='')
  planetsSpGCols <<- paste(planetsSpCols, "G", sep="")
  planetsComb <<- combn(planetsBaseCols, 2, simplify=F)
  planetsCombLon <<- as.character(lapply(planetsComb, function(x) paste(x[1], x[2], 'LON', sep='')))
  planetsCombAsp <<- as.character(lapply(planetsComb, function(x) paste(x[1], x[2], 'ASP', sep='')))
  planetsCombOrb <<- paste(planetsCombLon, 'ORB', sep='')
  zodSignsCols <<- c('AR', 'TA', 'GE', 'CA', 'LE', 'VI', 'LI', 'SC', 'SA', 'CP', 'AC', 'PI')
  lenZodEnergyMi <<- length(planetsBaseCols) * length(zodSignsCols)
  #lenZodEnergyMa <- (length(planetsLonCols) * length(zodSignsCols)) - lenZodEnergyMi
  # Remove eclipse cols due it do not have speed
  planetsSpCols <<- planetsSpCols[grep('^E', planetsSpCols, ignore.case=T, invert=T)]
  planetsDecCols <<- planetsDecCols[grep('^E', planetsDecCols, ignore.case=T, invert=T)]
}

buildPlanetsColsNames(planetsBaseCols)

# a function that returns the position of n-th largest
maxn <- function(x, n) {
  order_x <- order(x, decreasing = TRUE)
  if ( length(order_x) < n ) {
    n = length(order_x)
  }
  x[order_x[n]]
}

CJDT <- function(X, Y) {
  setkey(X[,c(k=1, .SD)], k)[Y[,c(k=1, .SD)], allow.cartesian=TRUE][,k:=NULL]
}

decToDeg <- function(num) {
  num <- abs(num)
  d <- as.integer(num)
  part <- (num-d)*60
  m <- as.integer(part)
  s <- as.integer((part-m)*60)
  c(d, m, s)
}

gaint_Population <- function (object, ...) {
  min <- object@min
  max <- object@max
  nvars <- length(min)
  population <- matrix(NA, nrow = object@popSize, ncol = nvars)
  for (j in 1:nvars) {
    population[, j] <- sample(min[j]:max[j], object@popSize, replace=TRUE)
  }

  return(population)
}

gaint_raMutation <- function(object, parent, k = 1) {
  mutate <- parent <- as.vector(object@population[parent, ])
  n <- length(parent)
  j <- sample(1:n, size = k)
  # mutate the parameters
  mutate[j] <- sapply(j, function(x) sample(object@min[x[1]]:object@max[x[1]], 1))
  return(mutate)
}

gaint_rwSelection <- function (object, ...) {
  prob <- abs(object@fitness)/sum(abs(object@fitness))
  sel <- sample(1:object@popSize, size = object@popSize, prob = pmin(pmax(0, prob), 1, na.rm = TRUE), replace = TRUE)
  out <- list(population = object@population[sel, , drop = FALSE], fitness = object@fitness[sel])
  return(out)
}

gaint_spCrossover <- function (object, parents, ...) {
  fitness <- object@fitness[parents]
  parents <- object@population[parents, , drop = FALSE]
  n <- ncol(parents)
  children <- matrix(NA, nrow = 2, ncol = n)
  fitnessChildren <- rep(NA, 2)
  crossOverPoint <- sample(0:n, size = 1)
  if (crossOverPoint == 0) {
    children[1:2, ] <- parents[2:1, ]
    fitnessChildren[1:2] <- fitness[2:1]
  }
  else if (crossOverPoint == n) {
    children <- parents
    fitnessChildren <- fitness
  }
  else {
    children[1, ] <- c(parents[1, 1:crossOverPoint], parents[2, (crossOverPoint + 1):n])
    children[2, ] <- c(parents[2, 1:crossOverPoint], parents[1, (crossOverPoint + 1):n])
    fitnessChildren <- NA
  }
  out <- list(children = children, fitness = fitnessChildren)
  return(out)
}

npath <- function(path) {
  normalizePath(path.expand(path))
}

normalizeDistance <- function(x) {
  x[x > 180] <- abs(x[x > 180] - 360)
  x[x < -180] <- abs(x[x < -180] + 360)
  return(abs(x))
}

calculateAspects <- function(x, cusorbs) {
  allidx <- rep(FALSE, length(x))
  for (aspect in aspects) {
    comborb <- cusorbs['orbs', as.character(aspect)]
    rstart <- aspect-comborb
    rend <- aspect+comborb
    idx <- x >= rstart & x <= rend
    x[idx] <- aspect
    allidx[idx] <- TRUE
  }
  # put NA no aspects
  x[!allidx] <- NA
  return(x)
}

calculateAspectOrbs <- function(x, cusorbs) {
  allidx <- rep(FALSE, length(x))
  for (aspect in aspects) {
    comborb <- cusorbs['orbs', as.character(aspect)]
    rstart <- aspect-comborb
    rend <- aspect+comborb
    idx <- x >= rstart & x <= rend
    x[idx] <- abs(x[idx] - aspect)
    allidx[idx] <- TRUE
  }
  # put NA no aspects
  x[!allidx] <- NA
  return(x)
}

# calculate the planets aspects
processPlanetsAspects <- function(planetsorig, cusorbs) {
  # clone original to ensure is no modified
  planets <- copy(planetsorig)
  planets[, c(planetsCombAsp) := lapply(.SD, calculateAspects, cusorbs=cusorbs), .SDcols=planetsCombLon]
  planets[, c(planetsCombOrb) := lapply(.SD, calculateAspectOrbs, cusorbs=cusorbs), .SDcols=planetsCombOrb]
  return(planets)
}

mainProcessPlanetsDegSplit <- function(planetsorig, degsplit) {
  # calculate the lon deg splits
  calculateLonGroups <- function(x, degsplit) {
    return(cut(x, seq(0, 360, by=degsplit)))
  }

  planets <- copy(planetsorig)
  planets[, c(planetsLonGCols) := lapply(.SD, calculateLonGroups, degsplit=degsplit), .SDcols=planetsLonCols]
}

mainOpenPlanets <- function(planetsfile, cusorbs) {
  Date=DateMT4=Year=NULL
  planetsfile <- npath(paste("~/trading/dplanets/", planetsfile, ".tsv", sep=""))
  planets <- fread(planetsfile, sep="\t", na.strings="", verbose = F)
  planets[, Date := as.Date(planets$Date, format="%Y-%m-%d")]
  planets <- planets[, c('Date', planetsLonCols, planetsDecCols, planetsSpCols), with=F]
  #planets[, DateMT4 := as.character(format(Date, "%Y.%m.%d"))]
  planets[, Year := as.character(format(Date, "%Y"))]
  planets[, wday := format(Date, "%w")]
  setkey(planets, 'Date')

  # calculate longitudinal differences
  for (curcol in planetsCombLon) {
    col1 <- paste(substr(curcol, 1, 2), 'LON', sep='')
    col2 <- paste(substr(curcol, 3, 4), 'LON', sep='')
    planets[, c(curcol) := get(col1) - get(col2)]
  }

  # Normalize to 180 degrees range
  planets[, c(planetsCombLon) := lapply(.SD, normalizeDistance), .SDcols=planetsCombLon]
  # Copy to orbs
  exprcopy <- paste("c(planetsCombOrb) := list(", paste(planetsCombLon, collapse=","), ")", sep="")
  planets[, eval(parse(text = exprcopy))]

  # calculate aspects for max orbs
  orbsmatrix <- matrix(cusorbs, nrow = 1, ncol = length(aspects), byrow = TRUE, dimnames = list('orbs', aspects))
  planets <- processPlanetsAspects(planets, orbsmatrix)
  return(planets)
}

# Open planets file and handle caching
openPlanets <- function(planetsfile, cusorbs=deforbs, clear=F) {
  ckey <- list(planetsfile, cusorbs)
  planets <- loadCache(key=ckey)
  if (is.null(planets) || clear) {
    planets <- mainOpenPlanets(planetsfile, cusorbs)
    saveCache(planets, key=ckey)
    cat("Set openPlanets cache\n")
  }
  else {
    cat("Get openPlanets cache\n")
  }
  return(planets)
}

mainOpenSecurity <- function(securityfile, mapricefs, mapricesl, dateformat="%Y.%m.%d", sdate) {
  filename <- npath(paste("~/trading/", securityfile, ".csv", sep=''))
  security <- fread(filename)
  security[, Date := as.Date(as.character(Date), format=dateformat)]
  security[, Year := as.character(format(Date, "%Y"))]
  # sort by Date and key it
  setkey(security, 'Date')
  # take data starging from sdate
  security <- security[Date >= sdate,]
  security[, Mid := (High + Low + Close + Open) / 4]
  security[, MidMAF := SMA(Mid, n=mapricefs)]
  security[, MidMAS := SMA(Mid, n=mapricesl)]
  security[, val := MidMAF-MidMAS]

  if (all(security$val == 0)) {
    stop("Undetermined security price direction")
  }

  security <- security[!is.na(val)]
  security[, Eff := cut(val, c(-10000, 0, 10000), labels=c('down', 'up'), right=FALSE)]
  return(security)
}

mainPlanetsCompositeSignificance <- function(planets, security) {
  planets  <- merge(planets, security, by='Date')
  if (nrow(planets) == 0) {
    stop('No planets / security merged rows by Date, error in calculate planetsVarsSignificance.')
  }

  planets.long <- melt(planets, id.var=c('Date', 'Eff'), measure.var=planetsLonGCols)
  # calculate the signficance for each long
  significance <- planets.long[, cbind(as.list(prop.table(as.numeric(table(Eff)))), as.list(as.numeric(table(Eff)))), by=c('value')]
  # calculate the middle lon for each group
  significance[, c('lonx', 'lony') := data.table(str_split_fixed(value, ",", 2))]
  significance[, lonx := as.numeric(str_replace(lonx, '\\]|\\[|\\(|\\)', ''))]
  significance[, lony := as.numeric(str_replace(lony, '\\]|\\[|\\(|\\)', ''))]
  significance[, lon := (lonx + lony) / 2]
  # set names to table
  setnames(significance, c('key', 'V1', 'V2', 'V3', 'V4', 'lonx', 'lony', 'lon'))
  # calculate the difference
  significance[, pdiff := V2 - V1]
  significance <- significance[!is.na(key)]
  return(significance)
}

mainPlanetsVarsSignificance <- function(planets, security) {
  # build significance table for each planet
  planetTable <- function(curcol) {
    planet.significance <- copy(significance)
    planet.significance[, origin := substr(curcol, 1 , 2)]
    return(planet.significance)
  }

  significance <- mainPlanetsCompositeSignificance(planets, security)
  significance.full <- rbindlist(lapply(planetsLonCols, planetTable))
  significance.full[, keyidx := paste(key, origin, sep='_')]
  return(significance.full)
}

cmpTestPlanetsSignificanceRelative <- function(execfunc, sinkfile, ...) {
  if (!hasArg('execfunc')) stop("Provide function to execute")
  ptm <- proc.time()

  # Get the git branch / tag system name
  getSystemName <- function(strparams) {
    tryCatch(system2("git", strparams, stdout=T),
             warning = function(w) {
               'undefined'
             })
  }

  # determine the current system version
  system("cd ~/trading")
  # First try the branch name
  branch.name <- getSystemName("symbolic-ref -q --short HEAD")
  # If master or HEAD try the tag name
  if (branch.name == 'undefined') {
    branch.name <- getSystemName("describe --tags --exact-match")
  }

  # open a security historic file
  openSecurity <- function(securityfile, mapricefs, mapricesl, dateformat="%Y.%m.%d", sdate) {
    ckey <- list('openSecurity', securityfile, mapricefs, mapricesl, sdate)
    security <- loadCache(key=ckey, dirs=c(securityfile), onError='print')
    if (is.null(security)) {
      security <- mainOpenSecurity(securityfile, mapricefs, mapricesl, dateformat, sdate)
      saveCache(security, key=ckey, dirs=c(securityfile))
      cat("Set openSecurity cache\n")
    }
    else {
      cat("Get openSecurity cache\n")
    }

    return(security)
  }

  # process the degsplit for a cloned original planets dt
  processPlanetsDegsplit <- function(planetsorig, degsplit) {
    args <- list(...)
    ckey <- list('processPlanetsDegsplit', args$securityfile, planetsorig, degsplit)
    planets <- loadCache(key=ckey, dirs=c(args$securityfile), onError='print')
    if (is.null(planets)) {
      # If the cache file exists but no data was returned probably is corrupted
      pathname <- findCache(key=ckey, dirs=c(args$securityfile))
      if (!is.null(pathname)) file.remove(pathname)
      planets <- mainProcessPlanetsDegSplit(planetsorig, degsplit)
      saveCache(planets, key=ckey, dirs=c(args$securityfile))
      cat("Set processPlanetsDegsplit cache\n")
    }
    else {
      cat("Get processPlanetsDegsplit cache\n")
    }

    return(planets)
  }

  # calculate the proportional energy of aspect based on the distance
  energyGrowth <- function(energy, distance, speed = 0.5) {
    return(energy * (1 - speed) ^ abs(distance))
  }

  planetsVarsSignificance <- function(planets, security) {
    args <- list(...)
    ckey <- list('planetsVarsSignificance', args$securityfile, planets, security)
    significance <- loadCache(key=ckey, dirs=c(args$securityfile), onError='print')
    if (is.null(significance)) {
      pathname <- findCache(key=ckey, dirs=c(args$securityfile))
      if (!is.null(pathname)) file.remove(pathname)
      significance <- mainPlanetsVarsSignificance(planets, security)
      saveCache(significance, key=ckey, dirs=c(args$securityfile))
      cat("Set planetsVarsSignificance cache\n")
    }
    else {
      cat("Get planetsVarsSignificance cache\n")
    }

    return(significance)
  }

  planetsVarsSignificanceFilter <- function(significance, threshold) {
    significance <- significance[pdiff >= threshold | pdiff <= -threshold]
    significance <- significance[!is.na(key)]
    significance[, Eff := cut(pdiff, c(-10000, 0, 10000), labels=c('down', 'up'), right=FALSE)]
    return(significance)
  }

  # build the daily signficance table
  buildDailySignificance <- function(significance, planets.pred) {
    args <- list(...)
    # build daily significance indexes
    buildDailySignificanceIdxs <- function(planets.day) {
      curdate <- planets.day[['Date']]
      sigidxs <- paste(planets.day[planetsLonGCols], planetsBaseCols, sep='_')
      datelist <- rep(curdate, length(sigidxs))
      day.idxs <- list(Date=datelist, keyidx=sigidxs)
      return(day.idxs)
    }

    ckey <- list('buildDailySignificance', args$securityfile, significance, planets.pred)
    significance.days <- loadCache(key=ckey, dirs=c(args$securityfile), onError='print')
    if (is.null(significance.days)) {
      pathname <- findCache(key=ckey, dirs=c(args$securityfile))
      if (!is.null(pathname)) file.remove(pathname)

      significance.days.idxs <- rbindlist(apply(planets.pred, 1, buildDailySignificanceIdxs))
      significance.days <- merge(significance.days.idxs, significance, by=c('keyidx'))
      significance.days[, zsign := ceiling(lon/30)]
      setkeyv(significance.days, 'Date', 'origin')

      saveCache(significance.days, key=ckey, dirs=c(args$securityfile))
      cat("Set buildDailySignificance cache\n")
    }
    else {
      cat("Get buildDailySignificance cache\n")
    }

    return(significance.days)
  }

  # process the daily aspects energy
  dayAspectsEnergy <- function(planets.pred, aspectspolarity, aspectsenergy, zodenergy, orbs) {
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
    planets.pred.aspen[, polarity := aspectspolarity['polarity', aspect]]
    # Energy is given to the two involved planets in an aspect then we need two tables
    planets.pred.aspen.one <- copy(planets.pred.aspen)
    planets.pred.aspen.one <- planets.pred.aspen.one[, origin := plaone]
    planets.pred.aspen.one[, energy := aspectsenergy['energy', aspect]]
    planets.pred.aspen.two <- copy(planets.pred.aspen)
    planets.pred.aspen.two <- planets.pred.aspen.two[, origin := platwo]
    planets.pred.aspen.two[, energy := aspectsenergy['energy', aspect]]
    # join the two aspects cols
    planets.pred.aspen <- rbind(planets.pred.aspen.one, planets.pred.aspen.two)
    # use only aspects that are in the allowed orb for specific aspect
    # TODO: verify that the filtered aspects correspond to the maximum orb
    planets.pred.aspen <- planets.pred.aspen[orb <= orbs['orbs', aspect]]
    # compute the given energy based on the aspect orb distance
    planets.pred.aspen[, disenergy := energyGrowth(energy, orb)]
    # set energy up / down based on polarities
    planets.pred.aspen[polarity == 0, c('up', 'down') := list(0, disenergy)]
    planets.pred.aspen[polarity == 1, c('up', 'down') := list(disenergy, 0)]
    planets.pred.aspen[polarity == 2, c('up', 'down') := list(disenergy, disenergy)]

    return(planets.pred.aspen)
  }

  # process the significance rows energy
  dailySignificanceEnergy <- function(significance.days, planetszodenergy) {
    processSignificanceRow <- function(significance.row, by.row) {
      # add the planet zodiacal energy
      planetszodenergy[by.row[[1]], significance.row$zsign]
    }

    # process each significance row
    significance.days[, energy := processSignificanceRow(.SD, .BY), by=c('origin')]
    # invert the significance columns for negative energy
    significance.days[energy < 0, c('V1', 'V2', 'V3', 'V4') := list(V2, V1, V4, V3)]
    return(significance.days)
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

  # print a year solution summary
  printPredYearSummary <- function(x, type) {
    cat("\t ", x['Year.1'], " - ", type ,": vol =", x['volatility'], " - cor =", x['correlation'])
    cat(" - matches.t =", x['matches.t'], " - matches.f =", x['matches.f'], " - matches.d =", x['matches.d'], "\n")
  }

  # Plot the solution snippet
  plotSolutionSnippet <- function(snippet) {
    plot(0:20, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
    par(ps = 8, cex = 1, cex.main = 1)
    text(10, 10, snippet, pos=3)
  }

  relativeTrend <- function(args) {
    looptm <- proc.time()
    rdates <- as.Date(with(args, c(tsdate, tedate, vsdate, vedate, csdate, cedate)))
    new.fitness.best <- ""

    # build matrix
    orbsmatrix <- matrix(args$cusorbs, nrow = 1, ncol = length(aspects), byrow = TRUE,
                         dimnames = list('orbs', aspects))

    # aspects polarities
    aspectspolarity <- c(2, args$aspectspolarity)
    aspectspolaritymatrix <- matrix(aspectspolarity, nrow = 1, ncol = length(aspects), byrow = TRUE,
                                    dimnames = list('polarity', aspects))

    aspectsenergymatrix <- matrix(args$aspectsenergy, nrow = 1, ncol = length(args$aspectsenergy), byrow = TRUE,
                                  dimnames = list(c('energy'), aspects))

    #planetszodenergy <- c(args$planetszodenergy, rep(1, lenZodEnergyMa))
    planetszodenergymatrix <- matrix(args$planetszodenergy, nrow = length(planetsBaseCols), ncol = 12, byrow = TRUE,
                                     dimnames = list(planetsBaseCols, zodSignsCols))

    sout <- with(args, paste("testPlanetsSignificanceRelative('testSolution', securityfile=", shQuote(securityfile), ", planetsfile=", shQuote(planetsfile),
                             ", tsdate=", shQuote(tsdate), ", tedate=", shQuote(tedate), ", vsdate=", shQuote(vsdate), ", vedate=", shQuote(vedate),
                             ", csdate=", shQuote(csdate), ", cedate=", shQuote(cedate),
                             ", mapredsm=", mapredsm, ", mapricefs=", mapricefs, ", mapricesl=", mapricesl,
                             ", degsplit=", degsplit, ", threshold=", threshold,
                             ", energymode=", energymode,
                             ", cusorbs=c(", paste(cusorbs, collapse=", "), ")",
                             ", aspectsenergy=c(", paste(aspectsenergy, collapse=", "), ")",
                             ", planetszodenergy=c(", paste(planetszodenergy, collapse=", "), ")",
                             ", aspectspolarity=c(", paste(aspectspolarity, collapse=", "), ")",
                             ", dateformat=", shQuote(dateformat), ", verbose=F", ", doplot=T, plotsol=F", ", fittype=", shQuote(fittype), ")\n", sep=""))

    sout <- paste("system version: ", branch.name, "\n\n", sout, sep="")
    # open planets file
    planetsorig <- openPlanets(args$planetsfile, deforbs)
    planets <- processPlanetsDegsplit(planetsorig, args$degsplit)

    # split in training and prediction
    planets.train <- planets[Date > rdates[1] & Date <= rdates[2] & wday %in% c(1, 2, 3, 4, 5)]
    planets.pred <- planets[Date > rdates[3] & Date <= rdates[6] & wday %in% c(1, 2, 3, 4, 5)]
    # load the security data
    security <- with(args, openSecurity(securityfile, mapricefs, mapricesl, dateformat, tsdate))

    # process the planets significance table
    significance <- with(args, planetsVarsSignificance(planets.train, security))
    # filter the significance by threshold
    significance <- planetsVarsSignificanceFilter(significance, args$threshold)

    # build significance by days
    significance.days <- buildDailySignificance(significance, planets.pred)
    # Calculate daily significance energy and patterns (no cache due is fast to calculate
    significance.daysen <- dailySignificanceEnergy(significance.days, planetszodenergymatrix)

    # Daily aspects energy
    energy.days <- dayAspectsEnergy(planets.pred, aspectspolaritymatrix, aspectsenergymatrix,
                                    planetszodenergymatrix, orbsmatrix)

    # calculate prediction
    prediction <- calculatePrediction(significance.daysen, energy.days, args$energymode)
    planets.pred <- planets.pred[prediction]
    planets.pred <- security[planets.pred]
    setkeyv(planets.pred, c('Date', 'Year.1'))
    # smoth the prediction serie
    planets.pred[, predval := SMA(predRaw, args$mapredsm)]
    planets.pred[, predEff := predval]
    # determine a factor prediction response
    planets.pred[, predFactor := cut(predEff, c(-10000, 0, 10000), labels=c('down', 'up'), right=FALSE)]
    # plot solution snippet if doplot is enabled
    if (args$doplot && args$plotsol) {
      snippet <- paste(strwrap(sout, width=170), collapse="\n")
      plotSolutionSnippet(snippet)
    }
    # helper function to process predictions by year
    pltitle <- paste('Yearly prediction VS price movement for ', args$securityfile)
    processYearPredictions <- function(x, doplot) processPredictions(x, pltitle, doplot)

    # compute test predictions by year
    years.test <- format(seq(rdates[3], rdates[4], by='year'), '%Y')
    years.conf <- format(seq(rdates[5], rdates[6], by='year'), '%Y')
    res.test <- planets.pred[Year.1 %in% years.test, processYearPredictions(.SD, F), by=Year.1]
    resMean <- function(x) round(mean(x), digits=2)
    res.test.mean <- res.test[, list(correlation=resMean(correlation), volatility=resMean(volatility), matches.d=resMean(matches.d))]
    # compute confirmation predictions by year
    res.conf <- planets.pred[Year.1 %in% years.conf, processYearPredictions(.SD, args$doplot), by=Year.1]
    res.conf.mean <- res.conf[, list(correlation=resMean(correlation), volatility=resMean(volatility), matches.d=resMean(matches.d))]

    # use appropriate fitness type
    if (args$fittype == 'correlation') {
      fitness <- round(res.test.mean$correlation * 100, digits=0)
      fitness.total <- round(((res.test.mean$correlation + res.conf.mean$correlation) * 100) / 2, digits=0)
    }
    else if (args$fittype == 'matches') {
      fitness <- round(res.test.mean$matches.d, digits=0)
      fitness.total <- round((res.test.mean$matches.d + res.conf.mean$matches.d) / 2, digits=0)
    }
    else {
      stop("No valid fittype provided")
    }

    ckey <- list('fitnessBest')
    fitness.best <- loadCache(key=ckey, dirs=c(args$securityfile), onError='print')
    if (is.null(fitness.best)) {
      pathname <- findCache(key=ckey, dirs=c(args$securityfile))
      if (!is.null(pathname)) file.remove(pathname)
      saveCache(-100, key=ckey, dirs=c(args$securityfile))
    }
    else if (fitness.total > fitness.best) {
      saveCache(fitness.total, key=ckey, dirs=c(args$securityfile))
      new.fitness.best <- "best solution --- "
    }

    cat("\n---------------------------------------------------------------------------------\n")
    cat(sout)
    cat("\n")
    print(orbsmatrix)
    cat("\n")
    print(aspectspolaritymatrix)
    cat("\n")
    print(aspectsenergymatrix)
    cat("\n")
    print(planetszodenergymatrix)
    cat("\n")

    # print yearly summary
    apply(res.test, 1, printPredYearSummary, type="Optimization")
    with(res.test.mean, cat("\tvolatility =", volatility, " - correlation =", correlation, " - matches.d =", matches.d, " - ### = ", fitness, "\n"))
    apply(res.conf, 1, printPredYearSummary, type="Confirmation")
    with(res.conf.mean, cat("\tvolatility =", volatility, " - correlation =", correlation, " - matches.d =", matches.d, "\n"))
    cat("\n\t", new.fitness.best, "Total fitness - %%% = ", fitness.total, "\n")
    cat("\t Predict execution/loop time: ", proc.time()-ptm, " - ", proc.time()-looptm, "\n")
    cat("\t Trained significance table with: ", nrow(planets.train), " days", "\n")
    cat("\t Optimized and confirmed with: ", nrow(planets.pred), " days", "\n")
    return(fitness)
  }

  processPredictions <- function(planets.test, pltitle, doplot) {
    planets.pred <- copy(planets.test)
    zerores <- list(correlation=0.0, volatility=0.0, matches.t=as.integer(0), matches.f=as.integer(0), matches.d=as.integer(0))

    if (nrow(planets.pred) == 0) {
      return(zerores)
    }

    # in case that all predictions are 0 we skip this solution
    if (all(planets.pred$predRaw == 0)) {
      return(zerores)
    }

    if (all(is.na(planets.pred$Mid))) {
      correlation <- 0
      volatility <- 0
    }
    else {
      correlation <- planets.pred[!is.na(Mid), cor(predval, Mid, use="pairwise", method='spearman')]
      volatility <- planets.pred[!is.na(Mid), mean(Mid) / sd(Mid)]
    }

    # if plot is enabled
    if (doplot) {
      interval <- abs(as.integer((min(planets.pred$Date)-max(planets.pred$Date))/80))
      x_dates <- seq(min(planets.pred$Date), max(planets.pred$Date), by=interval)

      if (all(is.na(planets.pred$Mid))) {
        p1 <- ggplot() + geom_path(data = planets.pred, aes(Date, predval), size=1)
      }
      else {
        # split security & prediction data with it's corresponding MAs
        planets.sec.plot <- planets.pred[, c('Date', 'Mid', 'MidMAF', 'MidMAS'), with=F]
        planets.sec.plot[, type := 'security']
        setnames(planets.sec.plot, c('Date', 'val', 'valMAF', 'valMAS', 'type'))
        planets.pred.plot <- planets.pred[, c('Date', 'predval', 'predval', 'predval'), with=F]
        planets.pred.plot[, type := 'prediction']
        setnames(planets.pred.plot, c('Date', 'val', 'valMAF', 'valMAS', 'type'))
        planets.plot <- rbindlist(list(planets.pred.plot, planets.sec.plot))
        # facet plot
        p1 <- ggplot() + facet_grid(type ~ ., scale = "free") +
        geom_path(data=planets.plot, aes(Date, val), size = 1, na.rm=T) +
        geom_path(data = planets.plot, aes(Date, valMAF), colour="blue", size=0.7, na.rm=T) +
        geom_path(data = planets.plot, aes(Date, valMAS), colour="red", size=0.7, na.rm=T)
      }

      p1 <- p1 + theme(axis.text.x = element_text(angle = 90, size = 7), text=element_text(size=10)) +
      ggtitle(pltitle) + scale_fill_grey() + scale_shape_identity() + scale_x_date(breaks=x_dates)
      print(p1)
    }

    # calculate accuracy
    t1 <- with(planets.pred, table(Eff==predFactor))
    if (all(c('TRUE', 'FALSE') %in% names(t1))) {
      matches.t <- t1[['TRUE']]
      matches.f <- t1[['FALSE']]
    }
    else if ('TRUE' %in% names(t1)) {
      matches.t <- t1[['TRUE']]
      matches.f <- as.integer(0)
    }
    else if ('FALSE' %in% names(t1)) {
      matches.t <- as.integer(0)
      matches.f <- t1[['FALSE']]
    }
    else {
      matches.t <- as.integer(0)
      matches.f <- as.integer(0)
    }

    # calculate the matches difference
    if (matches.t == 0 && matches.f == 0) {
      matches.d <- 0
    }
    else {
      matches.d <- (matches.t / (matches.t + matches.f)) * 100
    }

    return(list(correlation=correlation, volatility=volatility, matches.t=matches.t, matches.f=matches.f, matches.d=matches.d))
  }

  relativeTrendFitness <- function(x, securityfile, planetsfile, tsdate, tedate, vsdate, vedate, csdate, cedate,
                                   fittype, dateformat, mapricefs, mapricesl) {
    # build the parameters based on GA indexes
    co.e = 5+length(deforbs)
    api.e = co.e+length(aspects)-1
    ae.e = api.e+length(aspects)
    pze.e = ae.e+lenZodEnergyMi

    args <-list(securityfile=securityfile,
                planetsfile=planetsfile,
                tsdate=tsdate,
                tedate=tedate,
                vsdate=vsdate,
                vedate=vedate,
                csdate=csdate,
                cedate=cedate,
                mapricefs=mapricefs,
                mapricesl=mapricesl,
                fittype=fittype,
                dateformat=dateformat,
                verbose=F,
                doplot=F,
                mapredsm=x[1],
                degsplit=x[2],
                threshold=x[3]/100,
                energymode=x[4],
                cusorbs=x[5:(co.e-1)],
                aspectspolarity=x[co.e:(api.e-1)],
                aspectsenergy=adjustEnergy(x[api.e:(ae.e-1)]),
                planetszodenergy=adjustEnergy(x[ae.e:(pze.e-1)]))

    return(relativeTrend(args))
  }

  adjustEnergy <- function(x) {
    x / 10
  }

  optimizeRelativeTrend <- function(securityfile, planetsfile, tsdate, tedate, vsdate, vedate, csdate, cedate, fittype,
                                    mapricefs, mapricesl, dateformat) {
    cat("---------------------------- Initialize optimization ----------------------------------\n\n")
    orbsmin <- rep(0, length(deforbs))
    orbsmax <- deforbs
    polaritymin <- rep(0, length(aspects)-1)
    polaritymax <- rep(1, length(aspects)-1)
    aspectenergymin <- rep(0, length(aspects))
    aspectenergymax <- rep(30, length(aspects))
    planetzodenergymin <- rep(-30, lenZodEnergyMi)
    planetzodenergymax <- rep(30, lenZodEnergyMi)

    minvals <- c( 2, 1,  0, 1, orbsmin, polaritymin, aspectenergymin, planetzodenergymin)
    maxvals <- c(10, 5, 20, 2, orbsmax, polaritymax, aspectenergymax, planetzodenergymax)

    # Clear the cache directory before start
    clearCache()
    # Create the cache directories structure
    getCachePath(dirs=c(securityfile))
    # Redirect output to file
    if (exists('sinkfile', envir=parent.frame())) {
      sinkpathfile <- npath(paste("~/trading/predict/", sinkfile, ".txt", sep=''))
      sink(sinkpathfile, append=T)
    }

    ga("real-valued", fitness=relativeTrendFitness, parallel=TRUE, monitor=gaMonitor, maxiter=60, run=50, min=minvals, max=maxvals,
       popSize=1000, elitism = 100, pcrossover = 0.9, pmutation = 0.1,
       selection=gaint_rwSelection, mutation=gaint_raMutation, crossover=gaint_spCrossover, population=gaint_Population,
       securityfile=securityfile, planetsfile=planetsfile, tsdate=tsdate, tedate=tedate, vsdate=vsdate,
       vedate=vedate, csdate=csdate, cedate=cedate, fittype=fittype, mapricefs=mapricefs, mapricesl=mapricesl, dateformat=dateformat)

    if (exists('sinkfile', envir=parent.frame())) {
      sink()
    }
  }

  testSolution <- function(predfile, ...) {
    args <- list(...)
    if (!hasArg('dateformat')) stop("A dateformat is needed.")
    predfile <- paste("~/", predfile, ".pdf", sep="")
    # Create directory if do not exists
    if (!file.exists(dirname(predfile))) {
      dir.create(dirname(predfile), recursive=T)
    }
    if (args$doplot) pdf(predfile, width = 11, height = 8, family='Helvetica', pointsize=12)
    relativeTrend(args)
    if (args$doplot) dev.off()
  }

  testSolutionDebug <- function(planetsfile, securityfile, tsdate, tedate, vsdate, vedate, csdate, cedate, fittype, dateformat, predfile, x) {
    # build the parameters based on GA indexes
    co.e = 5+length(deforbs)
    api.e = co.e+length(aspects)-1
    ae.e = api.e+length(aspects)
    pze.e = ae.e+lenZodEnergyMi

    args <-list(securityfile=securityfile,
                planetsfile=planetsfile,
                tsdate=tsdate,
                tedate=tedate,
                vsdate=vsdate,
                vedate=vedate,
                csdate=csdate,
                cedate=cedate,
                fittype=fittype,
                dateformat=dateformat,
                verbose=F,
                doplot=F,
                plotsol=F,
                mapredsm=x[1],
                degsplit=x[2],
                threshold=x[3]/100,
                energymode=x[4],
                cusorbs=x[5:(co.e-1)],
                aspectspolarity=x[co.e:(api.e-1)],
                aspectsenergy=adjustEnergy(x[api.e:(ae.e-1)]),
                planetszodenergy=adjustEnergy(x[ae.e:(pze.e-1)]))

    if (!hasArg('dateformat')) stop("A dateformat is needed.")
    relativeTrend(args)
  }

  clearCache <- function(path=getCachePath()) {
    answer <- '.'
    allFiles <- system2('lsfs', paste(getCachePath(), "| grep '^file' | awk '{print $2};'"), stdout=T)
    while (!(answer %in% c('y', 'n', ''))) {
      cat(sprintf("Are you really sure you want to delete %d files in '%s'? [y/N]: ", length(allFiles), path))
      answer <- tolower(readline())
    }
    if (answer != 'y') {
      return(invisible(NULL))
    }
    # remove files and count op results
    removed <- file.remove(allFiles)
    sucess <- length(removed[removed==TRUE])
    failed <- length(removed[removed==FALSE])
    cat(sprintf("%d files had been removed %d failed.\n", sucess, failed))
  }

  execfunc <- get(get('execfunc'))
  execfunc(...)

  #planets.security <- merge(planets, security, by='Date')
  #planets.security <- subset(planets.security, !is.na(Eff) & Date >= as.Date(tsdate) & Date <= as.Date(tedate))
  #pdf(npath(paste("~/", plotfile, "_SMA", mapricefs, ".pdf", sep='')), width = 11, height = 8, family='Helvetica', pointsize=12)
  #for (curcol in planetsLonGCols) {
  #  p1 <- ggplot(aes_string(x=curcol, fill="Eff"), data=planets.security) + geom_bar(position="fill") + theme(axis.text.x = element_text(angle = 90, size = 5)) + xlab(curcol)  + ggtitle(paste("Significance Planets LONG groups SMA", mapricefs)) + geom_hline(yintercept=seq(0, 1, by=0.1)) + scale_fill_grey()
  #  #p1 <- qplot(x=get(curcol), y=val2, geom='boxplot', data=planets.security) + theme(axis.text.x = element_text(angle = 85, size = 7)) + xlab(curcol) + ggtitle(paste("Significance Planets LONG groups SMA", mapricefs))
  #  print(p1)
  #}
  #dev.off()
}

# compile the function to byte code
testPlanetsSignificanceRelative <- cmpfun(cmpTestPlanetsSignificanceRelative)

securityPeaksValleys <- function(security, span=50, plotfile="peaks_valleys") {
  planets <- openPlanets("~/trading/dplanets/planets_4.tsv", orbs, aspects, 5, 10)
  planetsBaseCols <- c("SU", "ME", "VE", "MA", "JU", "SA", "NN")
  planetsLonCols <- paste(planetsBaseCols, 'LON', sep='')
  planetsLonGCols <- paste(planetsBaseCols, 'LONG', sep='')
  planetsCombLon <- combn(paste(c("SU", "MO", "ME", "VE", "MA", "JU", "SA", "UR", "NE", "PL", "SN", "NN"), 'LON', sep=''), 2, simplify=F)
  planetsCombLon <- as.character(lapply(planetsCombLon, function(x) paste(x[1], x[2], sep='')))
  cols <- c('Date', planetsLonGCols, planetsSpGCols, planetsCombLon)
  pos.p <- peaks(security$Mid, span)
  pos.v <- peaks(-security$Mid, span)
  # take 2 days before exact peak - valley
  dates.p <- security$Date[pos.p]
  dates.v <- security$Date[pos.v]
  cat(length(dates.p), "Peaks planets positions.\n")
  planets.p <- planets[Date %in% dates.p][, cols, with=F]
  planets.p[, type := 'peak']
  planets.p[, ds := 'selected']
  cat(length(dates.v), "Valleys planets positions.\n")
  planets.v <- planets[Date %in% dates.v][, cols, with=F]
  planets.v[, type := 'valley']
  planets.r <- planets[sample(1:nrow(planets), length(dates.p)+length(dates.v))]
  cat(nrow(planets.r), "Random planets positions.\n")
  planets.pv <- rbind(planets.p, planets.v)

  pdf(npath(paste("~/", plotfile, ".pdf", sep='')), width = 11, height = 8, family='Helvetica', pointsize=15)
  plot(security$Date, security$Mid, type="l")
  abline(v=dates.p, col="green", lty="dashed")
  abline(v=dates.v, col="red", lty="dashed")

  # Aggregated longitude
  planets.pv.long <- melt(planets.pv, id.var=c('Date', 'type'), measure.var=planetsLonGCols)
  planets.pv.long$value <- factor(planets.pv.long$value, mixedsort(unique(planets.pv.long$value)))
  pl <- ggplot(aes(x=value, fill=type), data=planets.pv.long) + geom_bar(position='fill') + theme(axis.text.x = element_text(angle = 90, size = 9)) + xlab("Aggregated Planets LONG.")  + ggtitle(paste("Peaks VS Valleys Aggregated Planets (Percent)"))
  print(pl)
  pl <- ggplot(aes(x=value, fill=type), data=planets.pv.long) + geom_bar() + theme(axis.text.x = element_text(angle = 90, size = 9)) + xlab("Aggregated Planets LONG.")  + ggtitle(paste("Peaks VS Valleys Aggregated Planets (Count)"))
  print(pl)

  # Aggregated Aspects
  planets.pv.asp <- melt(planets.pv, id.var=c('Date', 'type'), measure.var=planetsCombLon)
  planets.pv.asp <- planets.pv.asp[value != 'anon']
  pl <- ggplot(aes(x=value, fill=type), data=planets.pv.asp) + geom_bar(position = 'fill') + theme(axis.text.x = element_text(angle = 90, size = 9)) + xlab("Aggregated Planets Aspects")  + ggtitle(paste("Peaks VS Valleys Aggregated Planets (Percent)"))
  print(pl)
  pl <- ggplot(aes(x=value, fill=type), data=planets.pv.asp) + geom_bar() + theme(axis.text.x = element_text(angle = 90, size = 9)) + xlab("Aggregated Planets Aspects")  + ggtitle(paste("Peaks VS Valleys Aggregated Planets (Count)"))
  print(pl)

  for (curcol in planetsLonGCols) {
    if (curcol == 'Date') next
    ds <- planets.pv.long[variable == curcol]
    if ( nrow(ds) > 0 ) {
      pl <- ggplot(aes(x=value, fill=type), data=ds) + geom_bar(position = 'fill') + theme(axis.text.x = element_text(angle = 90, size = 9)) + xlab(curcol)  + ggtitle(paste("Peaks VS Planets variable: ", curcol, "(Percent)"))
      print(pl)
      pl <- ggplot(aes(x=value, fill=type), data=ds) + geom_bar() + theme(axis.text.x = element_text(angle = 90, size = 9)) + xlab(curcol)  + ggtitle(paste("Peaks VS Planets variable: ", curcol, "(Count)"))
      print(pl)
    }
  }

  for (curcol in planetsCombLon) {
    if (curcol == 'Date') next
    ds <- planets.pv.asp[variable == curcol]
    if ( nrow(ds) > 0 ) {
      pl <- ggplot(aes(x=value, fill=type), data=ds) + geom_bar(position = 'fill') + theme(axis.text.x = element_text(angle = 90, size = 9)) + xlab(curcol)  + ggtitle(paste("Peaks VS Planets variable: ", curcol, "(Percent)"))
      print(pl)
      pl <- ggplot(aes(x=value, fill=type), data=ds) + geom_bar() + theme(axis.text.x = element_text(angle = 90, size = 9)) + xlab(curcol)  + ggtitle(paste("Peaks VS Planets variable: ", curcol, "(Count)"))
      print(pl)
    }
  }

  dev.off()
}

processGetSymbol <- function(symbol) {
  for(t in 1:maxretry) {
    tryCatch({
      cat("Downloading ", symbol, "\t\t Attempt: ", t , "/", maxretry, "\n")
      symbol.df <- getSymbols(symbol, src="yahoo", from=startDate, env=NULL, return.class='data.frame')
      filename <- paste('./stocks/', symbol, ".csv", sep='')
      symbol.df <- cbind(rownames(symbol.df), symbol.df)
      names(symbol.df) <- c('Date', 'Open', 'High', 'Low', 'Close', 'Volume', 'Adj Close')
      write.csv(symbol.df, file=filename, row.names=F)
      cat("Sucessfully saved the stock data to ", filename, "\n")
      return(1)
    }, error = function(e) {
      print(e)
      return(0)
    })
  }
}

getMySymbolsData  <- function(listfile) {
  #Load the list of ticker symbols from a csv, each row contains a ticker
  symbolsls <- read.csv(paste("./symbols/", listfile, '.csv', sep=''),  header=F, stringsAsFactors=F)
  res <- lapply(symbolsls$V1, processGetSymbol)
}

buildSecurityPlanetsIndicators <- function(securityfile, sdate, mfs=20, msl=50, clear=F) {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'CE', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL', 'ES', 'EM')
  buildPlanetsColsNames(planetsBaseCols)
  planets <- openPlanets('planets_10', clear=clear)
  security <- mainOpenSecurity(securityfile, mfs, msl, "%Y-%m-%d", sdate)
  sp <- merge(security, planets, by='Date')
  # build composite indicators
  sp <- buildCompositeCols(sp)
  return(sp)
}

planetsIndicatorsChart <- function(securityfile, sdate, indicators, clear=F) {
  if (!is.vector(indicators)) {
    indicatorsfunc <- get(indicators)
    indicators <- indicatorsfunc()
  }

  sp <- as.data.frame(buildSecurityPlanetsIndicators(securityfile, sdate, clear=clear))
  # convert to xts class
  sp <- xts(sp[, c('Open', 'High', 'Low', 'Close', planetsCombLon, planetsLonCols, planetsSpCols, planetsDecCols)], order.by=sp$Date)
  # chart
  barChart(OHLC(sp), log.scale=T)
  # draw indicators
  planetsIndicatorsAdd(sp, indicators)
  return(sp)
}

planetsIndicatorsAdd <- function(sp, indicators) {
  indicators.exprs <- list()
  lines.exprs <- list()
  # add indicators we need expression for correctly work of chart zooom
  for (i in seq(1, length(indicators))) {
    name <- indicators[i]
    indicators.exprs[i] <- paste("addTA(sp[, c('", name, "')], legend='", name, "', col='yellow', type='p', pch=20, lwd=0.1)", sep="")
    lines.exprs[i] <- paste("addLines(0, c(45, 90, 135), NULL, col='red', on=", i+1, ")", sep="")
  }

  # eval indicators expression
  for (expr in indicators.exprs) {
    print(eval(parse(text = expr)))
  }

  # eval lines expressiions
  for (expr in lines.exprs) {
    print(eval(parse(text = expr)))
  }
}

idxPeaksMiddleValleys <- function(sp, span) {
  wpeaks <- sort.int(which(peaks(Op(sp), span=span)))
  wvalleys <- sort.int(which(peaks(-Op(sp), span=span)))
  # Remove valleys that are lower that first beak to try to sync the series
  wvalleys <- wvalleys[wvalleys > wpeaks[1]]
  pvi <- data.table(cbind(peaks=ts(wpeaks), valleys=ts(wvalleys)))
  pvi[, middle := round((peaks+valleys)/2)]
  pvi <- pvi[!is.na(wpeaks) & !is.na(valleys) & !is.na(middle),]
}

idxUpDowns <- function(sp) {
  wups <- which(sp$Eff=='up')
  wdowns <- which(sp$Eff=='down')
  maxlength <- ifelse(length(wups) < length(wdowns), length(wups), length(wdowns))
  totake <- round(maxlength * 0.5)
  # Choose 50% of observations of each group
  pvi <- data.table(cbind(ups=sample(wups, totake), downs=sample(wdowns, totake)))
}

pricePeaksLinesAdd <- function(sp, span, type=c('p', 'v', 'm'), col='green') {
  lchob <- quantmod:::get.current.chob()
  windows <- seq(1, lchob@windows)
  pvi <- idxPeaksMiddleValleys(sp, span)

  if (type == 'p') {
    lapply(windows, function(w) addLines(0, NULL, pvi$peaks, col=col, on=w))
  }
  else if (type == 'v') {
    lapply(windows, function(w) addLines(0, NULL, pvi$valleys, col=col, on=w))
  }
  else if (type == 'm') {
    lapply(windows, function(w) addLines(0, NULL, pvi$middle, col=col, on=w))
  }
}

buildCompositeCols <- function(sp) {
  # Calculate composite declinations
  sp$DSUMEVE <- calculateComposite(sp, c('SUDEC', 'MEDEC', 'VEDEC'))
  sp$DSUMEVEMACE <- calculateComposite(sp, c('SUDEC', 'MEDEC', 'VEDEC', 'MADEC', 'CEDEC'))
  sp$DMAJUNNSA <- calculateComposite(sp, c('MADEC', 'JUDEC', 'NNDEC', 'SADEC'))
  sp$DALL <- calculateComposite(sp, c('SUDEC', 'MEDEC', 'VEDEC', 'MADEC', 'CEDEC', 'JUDEC', 'NNDEC', 'SADEC', 'URDEC', 'NEDEC', 'PLDEC'))
  # Calculate composite speeds
  sp$SSUMEVE <- calculateComposite(sp, c('SUSP', 'MESP', 'VESP'))
  sp$SSUMEVEMACE <- calculateComposite(sp, c('SUSP', 'MESP', 'VESP', 'MASP', 'CESP'))
  sp$SMAJUNNSA <- calculateComposite(sp, c('MASP', 'JUSP', 'NNSP', 'SASP'))
  sp$SALL <- calculateComposite(sp, c('SUSP', 'MESP', 'VESP', 'MASP', 'CESP', 'JUSP', 'NNSP', 'SASP', 'URSP', 'NESP', 'PLSP'))
  # Calculate composite longitudes
  sp$LSUMEVE <- calculateComposite(sp, c('SULON', 'MELON', 'VELON'))
  sp$LSUMEVEMACE <- calculateComposite(sp, c('SULON', 'MELON', 'VELON', 'MALON', 'CELON'))
  sp$LMAJUNNSA <- calculateComposite(sp, c('MALON', 'JULON', 'NNLON', 'SALON'))
  sp$LALL <- calculateComposite(sp, c('SULON', 'MELON', 'VELON', 'MALON', 'CELON', 'JULON', 'NNLON', 'SALON', 'URLON', 'NELON', 'PLLON'))
  # Calculate composite aspects
  sp$AALL <- calculateComposite(sp, selectCols(planetsCombLon, '*', 'MO|ME|ES|EM'))
  sp$ASLOW <- calculateComposite(sp, selectCols(planetsCombLon, 'JU|NN|SA|UR|NE|PL', 'MO|ME|ES|EM'))
  sp$AMIDSLOW <- calculateComposite(sp, selectCols(planetsCombLon, 'MA|JU|NN|SA', 'MO|ME|ES|EM'))
  sp$AFAST <- calculateComposite(sp, selectCols(planetsCombLon, 'SU|ME|VE|MA', 'MO|ME|ES|EM'))
  return(sp)
}

calculateComposite <- function(sp, cols) {
  return(rowMeans(sp[, cols, with=F]))
}

aspectsCompositeIndicators <- function() {
  return(c('AALL', 'ASLOW', 'AMIDSLOW', 'AFAST'))
}

declinationCompositeIndicators <- function() {
  return(c('DSUMEVE', 'DSUMEVEMACE', 'DMAJUNNSA', 'DALL'))
}

speedCompositeIndicators <- function() {
  return(c('SSUMEVE', 'SSUMEVEMACE', 'SMAJUNNSA', 'SALL'))
}

longitudeCompositeIndicators <- function() {
  return(c('LSUMEVE', 'LSUMEVEMACE', 'LMAJUNNSA', 'LALL'))
}

declinationIndicators <- function() {
  return(planetsDecCols)
}

speedIndicators <- function() {
  return(planetsSpCols)
}

ecsuIndicators <- function() {
  cols <- planetsCombLon[grep('ES', planetsCombLon, ignore.case=T)]
  cols <- cols[grep('EM|SU|MO', cols, ignore.case=T, invert=T)]
  return(c(cols, 'ES'))
}

ecmoIndicators <- function() {
  cols <- planetsCombLon[grep('EM', planetsCombLon, ignore.case=T)]
  cols <- cols[grep('EM|SU|MO', cols, ignore.case=T, invert=T)]
  return(c(cols, 'EM'))
}

suIndicators <- function() {
  cols <- planetsCombLon[grep('SU', planetsCombLon, ignore.case=T)]
  return(c(cols))
}

meIndicators <- function() {
  cols <- planetsCombLon[grep('ME', planetsCombLon, ignore.case=T)]
  cols <- removeMoon(cols)
  cols <- removeEclipses(cols)
  return(c(cols, 'MESP'))
}

veIndicators <- function() {
  cols <- planetsCombLon[grep('VE', planetsCombLon, ignore.case=T)]
  cols <- removeMoon(cols)
  cols <- removeEclipses(cols)
  return(c(cols, 'VESP'))
}

maIndicators <- function() {
  cols <- planetsCombLon[grep('MA', planetsCombLon, ignore.case=T)]
  cols <- removeMoon(cols)
  cols <- removeEclipses(cols)
  return(c(cols, 'MASP'))
}

ceIndicators <- function() {
  cols <- planetsCombLon[grep('CE', planetsCombLon, ignore.case=T)]
  cols <- removeMoon(cols)
  cols <- removeEclipses(cols)
  return(c(cols, 'CESP'))
}

juIndicators <- function() {
  cols <- planetsCombLon[grep('JU', planetsCombLon, ignore.case=T)]
  cols <- removeMoon(cols)
  cols <- removeEclipses(cols)
  return(c(cols, 'JUSP'))
}

nnIndicators <- function() {
  cols <- planetsCombLon[grep('NN', planetsCombLon, ignore.case=T)]
  cols <- removeMoon(cols)
  cols <- removeEclipses(cols)
  return(c(cols, 'NNSP'))
}

saIndicators <- function() {
  cols <- planetsCombLon[grep('SA', planetsCombLon, ignore.case=T)]
  cols <- removeMoon(cols)
  cols <- removeEclipses(cols)
  return(c(cols, 'SASP'))
}

urIndicators <- function() {
  cols <- planetsCombLon[grep('UR', planetsCombLon, ignore.case=T)]
  cols <- removeMoon(cols)
  cols <- removeEclipses(cols)
  return(c(cols, 'URSP'))
}

neIndicators <- function() {
  cols <- planetsCombLon[grep('NE', planetsCombLon, ignore.case=T)]
  cols <- removeMoon(cols)
  cols <- removeEclipses(cols)
  return(c(cols, 'NESP'))
}

plIndicators <- function() {
  cols <- planetsCombLon[grep('PL', planetsCombLon, ignore.case=T)]
  cols <- removeMoon(cols)
  cols <- removeEclipses(cols)
  return(c(cols, 'PLSP'))
}

removeEclipses <- function(cols) {
  return(cols[grep('^..ES|^ES|^..EM|^EM', cols, ignore.case=T, invert=T)])
}

removeMoon <- function(cols) {
  return(cols[grep('MO', cols, ignore.case=T, invert=T)])
}

selectCols <- function(cols, usepat, ignpat) {
  cols <- cols[grep(usepat, cols)]
  cols <- cols[grep(ignpat, cols, invert=T)]
  return(cols)
}

indicatorPeakValleyHist <- function(sp, indicator, span, width, ylim, ybreak) {
  pvi <- idxPeaksMiddleValleys(sp, span)
  ipeaks <- sp[pvi$peaks,][[indicator]]
  ivalleys <- sp[pvi$valleys,][[indicator]]
  imiddle <- sp[pvi$middle,][[indicator]]
  # cbind with ts so if different vector lengths we avoid recycle
  pv <- data.table(cbind(peaks=ts(ipeaks), valleys=ts(ivalleys)), middle=ts(imiddle))
  pv <- pv[!is.na(peaks) & !is.na(valleys) & !is.na(middle),]
  pv <- melt(pv, variable.name='type', value.name='value', measure.var=c('peaks', 'valleys', 'middle'))
  #grid.arrange(p1, p2, ncol=2, main = paste("Peaks & Valleys", indicator, "hist", sep=' '))
  p <- ggplot(pv, aes(x = value)) +
  geom_histogram(binwidth = width) +
  scale_y_continuous(breaks=seq(1, 100, by=2)) +
  scale_x_continuous(breaks=seq(ybreak[1], ybreak[2], by=width), limits=ylim) +
  ggtitle(paste("Peaks VS Valleys VS Middle - ", indicator, " - histogram")) +
  facet_grid(. ~ type)
  print(p)
}

reportPeakValleyFreq <- function(sp, indicators, span, width, breaks=c(-360, 360)) {
  # Get peaks and valleys index
  pvi <- idxPeaksMiddleValleys(sp, span)
  pv <- copy(sp)
  return(frequencyCalculation(pv, pvi$peaks, pvi$valleys, indicators, width, breaks))
}

reportUpDownsFreq <- function(sp, indicators, width, breaks=c(-360, 360)) {
  # Get peaks and valleys index
  pvi <- idxUpDowns(sp)
  pv <- copy(sp)
  return(frequencyCalculation(pv, pvi$ups, pvi$downs, indicators, width, breaks))
}

frequencyCalculation <- function(pv, iup, idown, indicators, width, breaks=c(-360, 360)) {
  # identify peaks & valleys
  pv[iup, type := 'peaks']
  pv[idown, type := 'valleys']
  pv <- pv[!is.na(type), c(indicators, 'type'), with=F]
  # Convert the continuos values to cut factors
  pv[, c(indicators) := lapply(.SD, function(x) cut(x, breaks=seq(breaks[1], breaks[2], by=width))), .SDcols=indicators]
  # Calculate the frequencies
  pv <- melt(pv, id.var=c('type'), measure.var=indicators)
  freq <- pv[, data.table(table(value, type)), by=c('variable')]
  freq[, relFreq := prop.table(N), by=c('variable', 'value')]
  # join peaks & valleys cols in same rows
  freq.pv <- merge(freq[value == 'peaks'], freq[value =='valleys'], by=c('variable', 'type'))
  freq.pv[, value.x := NULL]
  freq.pv[, value.y := NULL]
  setnames(freq.pv, c('variable', 'value', 'Freq.p', 'relFreq.p', 'Freq.v', 'relFreq.v'))
  # total and relative significance of the event
  freq.pv[, relsig := relFreq.p - relFreq.v]
  freq.pv[Freq.p > Freq.v, sig := Freq.p / (Freq.p + Freq.v)]
  freq.pv[Freq.p < Freq.v, sig := -(Freq.v / (Freq.p + Freq.v))]
  freq.pv[Freq.p == Freq.v, sig := 0]
  # round numeric cols
  roundcols <- c('relFreq.p', 'relFreq.v', 'relsig', 'sig')
  freq.pv[, c(roundcols) := lapply(.SD, function(x) round(x, digits=3)), .SDcols=roundcols]
  return(freq.pv)
}

reportIndicatorsPeakValleyHist <- function(symbol, sp, span) {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'CE', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL', 'ES', 'EM')
  buildPlanetsColsNames(planetsBaseCols)
  pdf(npath(paste("~/pvm_histograms_", symbol, ".pdf", sep='')), width = 11, height = 8, family='Helvetica', pointsize=15)
  lapply(planetsCombLon, function(indicator) indicatorPeakValleyHist(sp, indicator, span, 15, c(1, 180), c(0, 180)))
  lapply(planetsDecCols, function(indicator) indicatorPeakValleyHist(sp, indicator, span, 5, c(-30, 30), c(-30, 30)))
  dev.off()
}

reportSignificantLongitudes <- function(securityfile, sdate, mfs, msl, degsplit, clear=F) {
  planets <- openPlanets('planets_10', clear=clear)
  planets <- mainProcessPlanetsDegSplit(planets, degsplit)
  security <- mainOpenSecurity(securityfile, mfs, msl, "%Y-%m-%d", sdate)
  freq <- mainPlanetsCompositeSignificance(planets, security)
  return(freq)
}

significantLongitudesAspects <- function(..., threshold=0.2, clear=F) {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'CE', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL', 'ES', 'EM')
  buildPlanetsColsNames(planetsBaseCols)
  planets <- openPlanets('planets_10', clear=clear)
  # leave only the longitudes
  planets <- planets[, c('Date', planetsLonCols), with=F]
  siglons <- reportSignificantLongitudes(..., clear=clear)
  # leave only the longitude & pdiff that above threshold
  # TODO: sort by pdiff and take the top 15 points.
  siglons <- siglons[abs(pdiff) > threshold, c('lon', 'pdiff'), with=F]
  # cartesian join
  siglons.day <- CJDT(siglons, planets)

  # Calculate lon / planets distance
  for (curcol in planetsLonDisCols) {
    planetcol <- paste(substr(curcol, 1, 2), 'LON', sep='')
    siglons.day[, c(curcol) := lon - get(planetcol)]
  }

  # Normalize to 180 degrees range
  siglons.day[, c(planetsLonDisCols) := lapply(.SD, normalizeDistance), .SDcols=planetsLonDisCols]

  # Calculate the aspects & orbs
  orbsmatrix <- matrix(deforbs, nrow = 1, ncol = length(aspects), byrow = TRUE, dimnames = list('orbs', aspects))
  siglons.day[, c(planetsLonAspCols) := lapply(.SD, calculateAspects, cusorbs=orbsmatrix), .SDcols=planetsLonDisCols]
  siglons.day[, c(planetsLonOrbCols) := lapply(.SD, calculateAspectOrbs, cusorbs=orbsmatrix), .SDcols=planetsLonDisCols]
  #siglons[Date == as.Date('2014-05-21'), c('Date', 'lon', planetsLonAspCols), with=F]
  return(siglons.day)
}
