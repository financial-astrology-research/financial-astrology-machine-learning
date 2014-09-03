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
aspects            <- c( 0,30,45,52,60,72,90,103,120,135,144,150,180)
deforbs            <- c(10, 3, 3, 3, 6, 3,10,  3,  6,  3,  3,  6, 10)

# columns names
buildPlanetsColsNames <- function(planetsBaseCols) {
  aspOrbsCols <<- as.character(apply(expand.grid(aspects, planetsBaseCols[1:(length(planetsBaseCols)-1)]), 1, function(x) paste(x[2], x[1], sep='')))
  planetsLonCols <<- paste(planetsBaseCols, 'LON', sep='')
  planetsLonDisCols <<- paste(planetsBaseCols, 'DIS', sep='')
  planetsLonOrbCols <<- paste(planetsBaseCols, 'ORB', sep='')
  planetsLonAspCols <<- paste(planetsBaseCols, 'ASP', sep='')
  planetsAspCols <<- paste(planetsBaseCols, 'ASP', sep='')
  planetsOrbCols <<- paste(planetsBaseCols, 'ORB', sep='')
  planetsDecCols <<- paste(planetsBaseCols, 'DEC', sep='')
  planetsLonGCols <<- paste(planetsLonCols, 'G', sep='')
  planetsSpCols <<- paste(planetsBaseCols, 'SP', sep='')
  planetsSpGCols <<- paste(planetsSpCols, "G", sep="")
  planetsComb <<- combn(planetsBaseCols, 2, simplify=F)
  planetsCombLon <<- as.character(lapply(planetsComb, function(x) paste(x[1], x[2], 'LON', sep='')))
  planetsCombAsp <<- as.character(lapply(planetsComb, function(x) paste(x[1], x[2], 'ASP', sep='')))
  planetsCombOrb <<- as.character(lapply(planetsComb, function(x) paste(x[1], x[2], 'ORB', sep='')))
  zodSignsCols <<- c('AR', 'TA', 'GE', 'CA', 'LE', 'VI', 'LI', 'SC', 'SA', 'CP', 'AC', 'PI')
  lenZodEnergyMi <<- length(planetsBaseCols) * length(zodSignsCols)
  #lenZodEnergyMa <- (length(planetsLonCols) * length(zodSignsCols)) - lenZodEnergyMi
  # Remove eclipse cols due it do not have speed
  planetsSpCols <<- planetsSpCols[grep('^E', planetsSpCols, ignore.case=T, invert=T)]
  planetsDecCols <<- planetsDecCols[grep('^E', planetsDecCols, ignore.case=T, invert=T)]
}

buildPlanetsColsNames(planetsBaseCols)

# Build a data table unique vector by taking first and last rows plus nrows and ncols
# faster for performance to build the unique cache key
dataTableUniqueVector <- function(DT) {
  return(c(DT[1,], DT[nrow(DT),], nrow(DT), ncol(DT)))
}

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

# calculate the proportional energy of aspect based on the distance
energyGrowth <- function(energy, distance, speed = 0.5) {
  return(energy * (1 - speed) ^ abs(distance))
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
  planets[, c(planetsCombOrb) := lapply(.SD, calculateAspectOrbs, cusorbs=cusorbs), .SDcols=planetsCombLon]
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

mainOpenPlanets <- function(planetsfile, cusorbs, calcasps=T) {
  Date=DateMT4=Year=NULL
  planetsfile <- npath(paste("~/trading/dplanets/", planetsfile, ".tsv", sep=""))
  planets <- fread(planetsfile, sep="\t", na.strings="", verbose = F)
  planets[, Date := as.Date(planets$Date, format="%Y-%m-%d")]
  planets <- planets[, c('Date', planetsLonCols, planetsDecCols, planetsSpCols), with=F]
  #planets[, DateMT4 := as.character(format(Date, "%Y.%m.%d"))]
  planets[, Year := as.character(format(Date, "%Y"))]
  planets[, wday := format(Date, "%w")]
  setkey(planets, 'Date')

  # Only calculate transit aspects if needed
  if (calcasps) {
    # calculate longitudinal differences
    for (curcol in planetsCombLon) {
      col1 <- paste(substr(curcol, 1, 2), 'LON', sep='')
      col2 <- paste(substr(curcol, 3, 4), 'LON', sep='')
      planets[, c(curcol) := get(col1) - get(col2)]
    }

    # Normalize to 180 degrees range
    planets[, c(planetsCombLon) := lapply(.SD, normalizeDistance), .SDcols=planetsCombLon]

    # calculate aspects for max orbs
    orbsmatrix <- matrix(cusorbs, nrow = 1, ncol = length(aspects), byrow = TRUE, dimnames = list('orbs', aspects))
    planets <- processPlanetsAspects(planets, orbsmatrix)
  }

  return(planets)
}

# Open planets file and handle caching
openPlanets <- function(planetsfile, cusorbs=deforbs, calcasps=T, clear=F) {
  ckey <- list(as.character(c('openPlanets', planetsfile, cusorbs)))
  planets <- secureLoadCache(key=ckey)
  if (is.null(planets) || clear) {
    planets <- mainOpenPlanets(planetsfile, cusorbs, calcasps)
    saveCache(planets, key=ckey)
    cat("Set openPlanets cache\n")
  }

  return(planets)
}

mainOpenSecurity <- function(securityfile, mapricefs=20, mapricesl=50, dateformat="%Y-%m-%d", sdate='1970-01-01') {
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

# Usage: psl <- openPlanetsSecurity("stocks/AXP", 20, 50)
openPlanetsSecurity <- function(securityfile, mafs=20, masl=50, dateformat="%Y-%m-%d", sdate='1970-01-01', planetsfile='planets_10', clear=F) {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'CE', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL', 'ES', 'EM')
  buildPlanetsColsNames(planetsBaseCols)
  planets <- openPlanetsAll(planetsfile, clear=clear)
  security <- mainOpenSecurity(securityfile, mafs, masl, dateformat, sdate)
  return(list(planets=planets, security=security))
}

# Usage: psl <- openPlanetsAll('planets_10')
openPlanetsAll <- function(planetsfile='planets_10', clear=F) {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'CE', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL', 'ES', 'EM')
  buildPlanetsColsNames(planetsBaseCols)
  planets <- openPlanets(planetsfile, clear=clear)
  return(planets)
}

openPlanetsBasic <- function(planetsfile='planets_10', clear=F) {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'CE', 'JU', 'NN', 'SA')
  buildPlanetsColsNames(planetsBaseCols)
  planets <- openPlanets(planetsfile, clear=clear)
  return(planets)
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

secureLoadCache <- function(key) {
  cached.data <- loadCache(key=key, onError='print')
  if (is.null(cached.data)) {
    # Check if there is a corrupted cache
    pathname <- findCache(key=key)
    if (!is.null(pathname)) file.remove(pathname)
    return(NULL)
  }

  return(cached.data)
}

# open a security historic file
openSecurity <- function(securityfile, mapricefs, mapricesl, dateformat="%Y.%m.%d", sdate) {
  ckey <- list(as.character(c('openSecurity', securityfile, mapricefs, mapricesl, sdate)))
  security <- secureLoadCache(key=ckey)
  if (is.null(security)) {
    security <- mainOpenSecurity(securityfile, mapricefs, mapricesl, dateformat, sdate)
    saveCache(security, key=ckey)
    cat("Set openSecurity cache\n")
  }

  return(security)
}

# Get the current git repository branch / tag name
branchName <- function() {
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

  return(branch.name)
}

# Prediction Moddel with GA optimization
cmpTestPlanetsSignificanceRelative <- function(execfunc, ...) {
  if (!hasArg('execfunc')) stop("Provide function to execute")
  ptm <- proc.time()
  branch.name <- branchName()

  # Build a long data table with daily aspects, orbs and longitudes
  meltedAndMergedDayAspects <- function(planets, symbol, psdate, pedate) {
    planetskey <- dataTableUniqueVector(planets)
    ckey <- list(as.character(c('meltedAndMergedDayAspects', planetskey, symbol, psdate, pedate)))
    aspects.day.long <- secureLoadCache(key=ckey)

    if (is.null(aspects.day.long)) {
      # calculate daily aspects
      aspects.day <- buildNatalLongitudeAspects(symbol, planets, F)
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

  # process the daily aspects energy
  dayAspectsEnergy <- function(planets, symbol, psdate, pedate, aspectspolarity, aspectsenergy, zodenergy, sigpenergy, orbs) {
    # aspects, orbs and longitudes in long format
    planets.pred.aspen <- meltedAndMergedDayAspects(planets, symbol, psdate, pedate)

    # Use only the separating aspects & applying with at much 1 deg of orb
    #planets.pred.aspen <- planets.pred.aspen[orbdir == 1 | (orbdir == -1 & orb <= 1 ),]

    # Add the aspects polarity
    planets.pred.aspen[, polarity := aspectspolarity['polarity', aspect]]
    # Calculate the transit planet zoodiacal energy
    processAspEnergy <- function(asp.row, by.row) {
      zodenergy[by.row[[1]], asp.row[[1]]]
    }

    # Set columns with transit zodiacal energy / aspect energy / sigpoints energy
    planets.pred.aspen[, tenergy := processAspEnergy(.SD, .BY), by=c('origin'), .SDcols=c('tzsign')]
    planets.pred.aspen[, aenergy := aspectsenergy['energy', aspect], by=c('aspect')]
    planets.pred.aspen[, spenergy := sigpenergy['energy', as.character(lon)], by=c('lon')]

    # Calculate the energy considering significant point / transit / aspect energy
    planets.pred.aspen[, energy :=  aenergy * tenergy * spenergy]

    # use only aspects that are in the allowed orb for specific aspect
    # TODO: verify that the filtered aspects correspond to the maximum orb
    planets.pred.aspen <- planets.pred.aspen[orb <= orbs['orbs', aspect]]

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

  # aggregate the daily energy and apply it with the daily significance energy
  # to calculate the final prediction
  calculatePrediction <- function(energy.days) {
    # to prevent division by zero
    prediction <- energy.days[, list(down = sum(down), up = sum(up)), by='Date']
    #prediction[, Date := as.Date(Date, format="%Y-%m-%d")]
    prediction[, predRaw := (up-down)]
    return(prediction)
  }

  # print a year solution summary
  printPredYearSummary <- function(x, type) {
    cat("\t ", x['Year'], " - ", type ,": vol =", x['volatility'], " - cor =", x['correlation'])
    cat(" - matches.r =", x['matches.r'], " - matches.u =", x['matches.u'], " - matches.d =", x['matches.d'], " - matches.t =", x['matches.t'], "\n")
  }

  # Plot the solution snippet
  plotSolutionSnippet <- function(snippet) {
    plot(0:20, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
    par(ps = 8, cex = 1, cex.main = 1)
    text(10, 10, snippet, pos=3)
  }

  stringSolution <- function(args) {
    sol <- with(args, paste("testPlanetsSignificanceRelative('testSolution'",
                            ", symbol=", shQuote(symbol),
                            ", securityfile=", shQuote(securityfile),
                            ", planetsfile=", shQuote(planetsfile),
                            ", predfile=", shQuote(predfile),
                            ", vsdate=", shQuote(vsdate), ", vedate=", shQuote(vedate),
                            ", mapredsm=", mapredsm, ", mapricefs=", mapricefs, ", mapricesl=", mapricesl,
                            ", cusorbs=c(", paste(cusorbs, collapse=", "), ")",
                            ", aspectsenergy=c(", paste(aspectsenergy, collapse=", "), ")",
                            ", sigpenergy=c(", paste(sigpenergy, collapse=", "), ")",
                            ", planetszodenergy=c(", paste(planetszodenergy, collapse=", "), ")",
                            ", aspectspolarity=c(", paste(aspectspolarity, collapse=", "), ")",
                            ", dateformat=", shQuote(dateformat), ", verbose=T", ", doplot=T, plotsol=F",
                            ", fittype=", shQuote(fittype), ")\n", sep=""))
    return(sol)
  }

  relativeTrendExec <- function(x, ...) {
    # Build the params sets
    args <- processParams(x, ...)
    # Execute
    return(relativeTrend(args))
  }

  relativeTrend <- function(args) {
    looptm <- proc.time()
    rdates <- as.Date(with(args, c(vsdate, vedate)))

    # open planets file and leave only needed cols for better speed
    planets <- openPlanets(args$planetsfile, deforbs, calcasps=F)
    planets <- planets[, c('Date', 'Year', 'wday', planetsLonCols), with=F]
    # load the security data and leave only needed cols
    security <- with(args, openSecurity(securityfile, mapricefs, mapricesl, dateformat, vsdate))
    security <- security[, c('Date', 'Year', 'Open', 'High', 'Low', 'Close', 'Mid', 'MidMAF', 'MidMAS', 'Eff'), with=F]

    # build significant points vector
    siglons <- buildNatalLongitudes(args$symbol)

    # build matrix
    orbsmatrix <- matrix(args$cusorbs, nrow = 1, ncol = length(aspects), byrow = TRUE,
                         dimnames = list('orbs', aspects))

    # aspects polarities
    aspectspolarity <- c(2, args$aspectspolarity)
    aspectspolaritymatrix <- matrix(aspectspolarity, nrow = 1, ncol = length(aspects), byrow = TRUE,
                                    dimnames = list('polarity', aspects))

    aspectsenergymatrix <- matrix(args$aspectsenergy, nrow = 1, ncol = length(args$aspectsenergy), byrow = TRUE,
                                  dimnames = list(c('energy'), aspects))

    sigpenergymatrix <- matrix(args$sigpenergy, nrow = 1, ncol = length(args$sigpenergy), byrow = TRUE,
                                  dimnames = list(c('energy'), siglons$lon))

    planetszodenergymatrix <- matrix(args$planetszodenergy, nrow = length(planetsBaseCols), ncol = 12, byrow = TRUE,
                                     dimnames = list(planetsBaseCols, zodSignsCols))

    # Calculate daily aspects energy for predict dates
    energy.days <- dayAspectsEnergy(planets, args$symbol, rdates[1], rdates[2], aspectspolaritymatrix, aspectsenergymatrix,
                                    planetszodenergymatrix, sigpenergymatrix, orbsmatrix)

    # Calculate prediction
    prediction <- calculatePrediction(energy.days)
    # join the security table with prediction
    planets.pred <- security[prediction]
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
    processYearPredictions <- function(x, doplot) processPredictions(x, pltitle, doplot)
    # split data in optimization and cross validation
    planets.pred.opt <- planets.pred[1:round(nrow(planets.pred)/2),]
    planets.pred.cv <- planets.pred[round(nrow(planets.pred)/2):nrow(planets.pred),]
    # Identify years with alone observations that can affect the years fitness mean
    years.opt <- table(planets.pred.opt$Year)
    years.cv <- table(planets.pred.cv$Year)

    if (args$doplot) {
      # When doplot is enabled use for confirmation all the available years
      sample.opt <- planets.pred.opt[Year %in% names(years.opt[years.opt > 20]),]
      sample.cv <- planets.pred.cv[Year %in% names(years.cv[years.cv > 20]),]

      # plot CV years
      sp <- xts(sample.cv[, c('Open', 'High', 'Low', 'Close', 'predval'), with=F], order.by=sample.cv$Date)
      for (year in names(years.cv[years.cv > 20])) {
        barChart(sp, log.scale=T, subset=year, TA='addSMA(20, col="red");addSMA(40, col="green");addAspEnergy();
                 addRSI(14);addATR(14);addPVLines("p",31,"green",c(1,2,3));addPVLines("v",31,"red",c(1,2,3))')
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

    # Sort samples by Date
    setkey(sample.opt, 'Date')
    setkey(sample.cv, 'Date')

    # compute test predictions by year
    res.test <- sample.opt[, processYearPredictions(.SD, F), by=Year]
    resMean <- function(x) round(mean(x), digits=2)
    res.test.mean <- res.test[, list(correlation=resMean(correlation), volatility=resMean(volatility), matches.t=resMean(matches.t))]
    # compute confirmation predictions by year
    res.conf <- sample.cv[, processYearPredictions(.SD, args$doplot), by=Year]
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
      sout <- stringSolution(args)

      # plot solution snippet if doplot is enabled
      if (args$plotsol) {
        snippet <- paste(strwrap(sout, width=170), collapse="\n")
        plotSolutionSnippet(snippet)
      }

      mout <- capture.output(print(orbsmatrix),
                             print(aspectspolaritymatrix),
                             print(aspectsenergymatrix),
                             print(sigpenergymatrix),
                             print(planetszodenergymatrix))
      # print buffered output
      cat(sout, mout, "\n", sep="\n")

      # print yearly summary
      apply(res.test, 1, printPredYearSummary, type="Optimization")
      with(res.test.mean, cat("\tvol =", volatility, " - cor =", correlation, " - matches.t =", matches.t, "\n"))
      apply(res.conf, 1, printPredYearSummary, type="Confirmation")
      with(res.conf.mean, cat("\tvol =", volatility, " - cor =", correlation, " - matches.t =", matches.t, "\n"))
      # totals and execution time
      cat("\n\t Totals: fitness = ", fitness, "\n")
      cat("\t Optimized and confirmed with: ", nrow(planets.pred), " days", "\n")
      cat("\t Predict execution/loop time: ", proc.time()-ptm, " - ", proc.time()-looptm, "\n\n")
    }

    return(fitness)
  }

  processPredictions <- function(planets.test, pltitle, doplot) {
    planets.pred <- copy(planets.test)
    zerores <- list(correlation=0.0, volatility=0.0, matches.r=as.integer(0), matches.u=as.integer(0), matches.d=as.integer(0))

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
      correlation <- (planets.pred[!is.na(Mid), cor(predval, MidMAF, use="pairwise", method='spearman')] * 100)
      volatility <- planets.pred[!is.na(Mid), mean(Mid) / sd(Mid)]
    }

    # calculate accuracy
    t1 <- with(planets.pred, table(Eff, Eff == predFactor))

    # fix any missing row or column in the table
    if ('FALSE' %ni% colnames(t1)) {
      t1 <- cbind(t1, 'FALSE' = c(as.integer(0), as.integer(0)))
    }
    if ('TRUE' %ni% colnames(t1)) {
      t1 <- cbind(t1, 'TRUE' = c(as.integer(0), as.integer(0)))
    }
    if ('down' %ni% rownames(t1)) {
      t1 <- rbind(t1, 'down' = c(as.integer(0), as.integer(0)))
    }
    if ('up' %ni% rownames(t1)) {
      t1 <- rbind(t1, 'up' = c(as.integer(0), as.integer(0)))
    }

    # add table margins
    t1 <- addmargins(t1)

    # calculate the percentage matches for each direction
    matches.u <- as.integer(t1['up', 'TRUE'] / t1['up', 'Sum'] * 100)
    matches.d <- as.integer(t1['down', 'TRUE'] / t1['down', 'Sum'] * 100)
    matches.r <- t1['Sum', 'Sum']

    # Percentage of correctly day responses from the total year days
    matches.t <- as.integer((t1['Sum', 'TRUE'] / matches.r) * 100)

    return(list(correlation=correlation, volatility=volatility, matches.r=matches.r, matches.u=matches.u, matches.d=matches.d, matches.t=matches.t))
  }

  processParams <- function(x, symbol, securityfile, planetsfile, predfile, vsdate, vedate, fittype, dateformat, mapricefs, mapricesl) {
    # build the parameters based on GA indexes
    co.e = 2+length(deforbs)
    api.e = co.e+length(aspects)-1
    ae.e = api.e+length(aspects)
    pze.e = ae.e+lenZodEnergyMi
    # 14 natal points
    spe.e = pze.e+14

    args <-list(symbol=symbol,
                securityfile=securityfile,
                planetsfile=planetsfile,
                predfile=predfile,
                vsdate=vsdate,
                vedate=vedate,
                mapricefs=mapricefs,
                mapricesl=mapricesl,
                fittype=fittype,
                dateformat=dateformat,
                verbose=F,
                doplot=F,
                plotsol=F,
                mapredsm=x[1],
                cusorbs=x[2:(co.e-1)],
                aspectspolarity=x[co.e:(api.e-1)],
                aspectsenergy=adjustEnergy(x[api.e:(ae.e-1)]),
                planetszodenergy=adjustEnergy(x[ae.e:(pze.e-1)]),
                sigpenergy=adjustEnergy(x[pze.e:(spe.e-1)]))

    return(args)
  }

  adjustEnergy <- function(x) {
    x / 10
  }

  optimizeRelativeTrend <- function(benchno, sectype, secsymbols, planetsfile, vsdate, vedate, fittype, mapricefs, mapricesl, dateformat) {
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

    minvals <- c( 2, orbsmin, polaritymin, aspectenergymin, planetzodenergymin, sigpenergymin)
    maxvals <- c(10, orbsmax, polaritymax, aspectenergymax, planetzodenergymax, sigpenergymax)

    # Clear the cache directory before start
    clearCache()

    # redirect the output to symbol sink file
    sinkpathfile <- npath(paste("~/trading/benchmarks/b", benchno, "_", sectype, ".txt", sep=''))
    # Redirect output to file
    #if (exists('sinkfile', envir=parent.frame())) {
    sink(sinkpathfile, append=T)
    cat("# version: ", branch.name, "\n")
    cat("#", vsdate, '-', vedate, 'OPTwCV -', fittype, 'fit -', mapricefs, '-', mapricesl, 'MAS', '\n\n')
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
                symbol=symbol, securityfile=securityfile, planetsfile=planetsfile, predfile=predfile,
                vsdate=vsdate, vedate=vedate, fittype=fittype, mapricefs=mapricefs, mapricesl=mapricesl, dateformat=dateformat)

      # output the solution string
      sink(sinkpathfile, append=T)
      x <- gar@solution[1,]
      args <- processParams(x, symbol, securityfile, planetsfile, predfile, vsdate, vedate, fittype, dateformat, mapricefs, mapricesl)
      cat(stringSolution(args))
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
    relativeTrend(args)
    if (args$doplot) dev.off()
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
      # Adjust the prices for splits / dividends
      symbol.df[, 2:7] <- adjustOHLC(symbol.df[, 2:7], use.Adjusted=T)
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

buildPlanetsIndicators <- function(clear=F) {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'CE', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL', 'ES', 'EM')
  buildPlanetsColsNames(planetsBaseCols)
  planets <- openPlanets('planets_10', clear=clear)
  # build composite indicators
  planets <- buildCompositeCols(planets)
  return(planets)
}

planetsIndicatorsChart <- function(securityfile, sdate, indicators, clear=F) {
  if (!is.vector(indicators)) {
    indicatorsfunc <- get(indicators)
    indicators <- indicatorsfunc()
  }

  planets <- buildPlanetsIndicators(clear=clear)
  security <- mainOpenSecurity(securityfile, 20, 50, "%Y-%m-%d", sdate)
  sp <- merge(security, planets, by='Date')
  # convert to xts class
  sp <- xts(sp[, c('Open', 'High', 'Low', 'Close', planetsCombLon, planetsLonCols, planetsSpCols, planetsDecCols), with=F], order.by=sp$Date)
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
  # TODO: test with more data
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

addPVLines <- function (type=c('p', 'v'), span=21, col = "blue", on = 1, overlay = TRUE) {
  lchob <- quantmod:::get.current.chob()
  x <- as.matrix(lchob@xdata)
  xsub <- as.matrix(lchob@xdata[lchob@xsubset,])

  if (type == 'p') {
    # Calculate peaks
    pv <- which(peaks(Op(x), span=span))
  }
  else if (type == 'v') {
    # Calculate valleys
    pv <- which(peaks(-Op(x), span=span))
  }

  # Determine the peak/valleys index positions in the subset
  xsub[rownames(xsub) %in% names(pv),]
  v <- which(rownames(xsub) %in% names(pv))

  chobTA <- new("chobTA")
  chobTA@new <- !overlay
  chobTA@TA.values <- NULL
  chobTA@name <- "chartLines"
  chobTA@call <- match.call()
  chobTA@on <- on
  chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col,
                        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, x.labels = lchob@x.labels, time.scale = lchob@time.scale,
                        col = col, h = 0, x = NULL, v = v)

  if (is.null(sys.call(-1))) {
    TA <- lchob@passed.args$TA
    lchob@passed.args$TA <- c(TA, chobTA)
    lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 0)
    do.call("chartSeries.chob", list(lchob))
    invisible(chobTA)
  }
  else {
    return(chobTA)
  }
}

# Aspect Energy Indicator
aspEnergy <- function(x) { x[,'predval'] }
addAspEnergy <- newTA(aspEnergy, type='l', col='white')

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

indicatorPeakValleyHist <- function(sp, indicator, span, wcut, ylim, ybreak) {
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
  geom_histogram(binwidth = wcut) +
  scale_y_continuous(breaks=seq(1, 100, by=2)) +
  scale_x_continuous(breaks=seq(ybreak[1], ybreak[2], by=wcut), limits=ylim) +
  ggtitle(paste("Peaks VS Valleys VS Middle - ", indicator, " - histogram")) +
  facet_grid(. ~ type)
  print(p)
}

reportPeakValleyFreq <- function(sp, indicators, span, wcut, breaks=c(-360, 360)) {
  # Get peaks and valleys index
  pvi <- idxPeaksMiddleValleys(sp, span)
  pv <- copy(sp)
  return(frequencyCalculation(pv, pvi$peaks, pvi$valleys, indicators, wcut, breaks))
}

# Usage: freq <- reportUpDownsFreq(sp, indicators, 10, "1970-01-01", "2001-01-01")
reportUpDownsFreq <- function(sp, indicators, wcut, sdate, edate, breaks=c(-360, 360)) {
  # Split the training data
  sp <- sp[Date >= as.Date(sdate) & Date < as.Date(edate),]
  # Get peaks and valleys index
  pvi <- idxUpDowns(sp)
  return(frequencyCalculation(sp, pvi$ups, pvi$downs, indicators, wcut, breaks))
}

frequencyCalculation <- function(sp, iup, idown, indicators, wcut, breaks=c(-360, 360)) {
  pv <- copy(sp)
  # identify peaks & valleys
  pv[iup, type := 'peaks']
  pv[idown, type := 'valleys']
  pv <- pv[!is.na(type), c(indicators, 'type'), with=F]

  if (wcut != 0) {
    # if wcut is zero then we expect a factors indicators
    # Convert the continuos values to cut factors
    pv[, c(indicators) := lapply(.SD, function(x) cut(x, breaks=seq(breaks[1], breaks[2], by=wcut))), .SDcols=indicators]
  }

  # Calculate the frequencies
  pv <- melt(pv, id.var=c('type'), measure.var=indicators)
  # Remove NAS
  pv <- pv[!is.na(value),]
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

# Usage: buildSignificantLongitudes(planets, security, 6, '1970-01-01', '1995-01-01')
buildSignificantLongitudes <- function(planets, security, degsplit, tsdate, tedate, clear=F) {
  planetskey <- dataTableUniqueVector(planets)
  securitykey <- dataTableUniqueVector(security)
  ckey <- list(as.character(c('buildSignificantLongitudes', planetskey, securitykey, degsplit, tsdate, tedate)))
  freq <- secureLoadCache(key=ckey)

  if (is.null(freq) || clear) {
    # split a training set to build the composite significance points
    planets.train <- planets[Date > tsdate & Date <= tedate & wday %in% c(1, 2, 3, 4, 5)]
    # leave only the longitudes
    loncols <- colnames(planets.train)
    loncols <- loncols[grep('^..LON$', loncols)]
    planets.train <- planets.train[, c('Date', loncols), with=F]
    # build the deg splits
    planets.train <- mainProcessPlanetsDegSplit(planets.train, degsplit)
    freq <- mainPlanetsCompositeSignificance(planets.train, security)
    # sort by most significant
    freq <- freq[, lon, pdiff][order(-abs(pdiff))]

    saveCache(freq, key=ckey)
    cat("Set buildSignificantLongitudes cache\n")
  }

  return(freq)
}

calculatePointsPlanetsAspects <- function(points.planets, fwide=F) {
  loncols <- colnames(points.planets)
  loncols <- loncols[grep('^..LON$', loncols)]

  # build colnames based on the existing planets
  planetsLonDisCols <- str_replace(loncols, 'LON', 'DIS')
  planetsLonAspCols <- str_replace(loncols, 'LON', 'ASP')
  planetsLonOrbCols <- str_replace(loncols, 'LON', 'ORB')

  # Calculate lon / planets distance
  for (curcol in planetsLonDisCols) {
    planetcol <- paste(substr(curcol, 1, 2), 'LON', sep='')
    points.planets[, c(curcol) := lon - get(planetcol)]
  }

  # Normalize to 180 degrees range
  points.planets[, c(planetsLonDisCols) := lapply(.SD, normalizeDistance), .SDcols=planetsLonDisCols]

  # Calculate the points.planets & orbs
  orbsmatrix <- matrix(deforbs, nrow = 1, ncol = length(aspects), byrow = TRUE, dimnames = list('orbs', aspects))
  points.planets[, c(planetsLonAspCols) := lapply(.SD, calculateAspects, cusorbs=orbsmatrix), .SDcols=planetsLonDisCols]
  points.planets[, c(planetsLonOrbCols) := lapply(.SD, calculateAspectOrbs, cusorbs=orbsmatrix), .SDcols=planetsLonDisCols]
  #siglons[Date == as.Date('2014-05-21'), c('Date', 'lon', planetsLonAspCols), with=F]

  # Format wide all the significant point points.planets
  if (fwide) {
    points.planets.wide <- data.table(Date=unique(points.planets$Date))
    for (curcol in unique(points.planets$lon)) {
      points.planets.cur <- points.planets[lon == curcol, c('Date', planetsLonDisCols, planetsLonAspCols, planetsLonOrbCols), with=F]
      curcolnames <- paste(c(planetsLonDisCols, planetsLonAspCols, planetsLonOrbCols), '.', curcol, sep='')
      setnames(points.planets.cur, c('Date', curcolnames))
      points.planets.wide <- merge(points.planets.wide, points.planets.cur, by=c('Date'))
    }
    points.planets <- points.planets.wide
  }

  return(points.planets)
}

# Build the natal positions table for a given symbol
buildNatalLongitudes <- function(symbol, clear=F) {
  ckey <- list(as.character(c('buildNatalLongitudes', symbol)))
  natal.symbol <- secureLoadCache(key=ckey)

  if (is.null(natal.symbol) || clear) {
    # open the stocks incorporation date planets positions
    natalfile <- npath(paste("~/trading/charts/stocksinc.tsv", sep=""))
    natal <- fread(natalfile, sep="\t", na.strings="", verbose = F)
    loncols <- colnames(natal)
    loncols <- loncols[grep('^..LON$', loncols)]
    natal.long <- melt(natal, id.var=c('Symbol'), measure.var=loncols)
    natal.symbol <- natal.long[Symbol == symbol,]
    setnames(natal.symbol, c('Symbol', 'variable', 'lon'))

    saveCache(natal.symbol, key=ckey)
    cat("Set buildNatalLongitudes cache\n")
  }

  return(natal.symbol)
}

# Calculate transits to natal position (symbol incorporation chart) aspects
# Usage: buildNatalLongitudeAspects('AXP', planets)
buildNatalLongitudeAspects <- function(symbol, planets, fwide=F, clear=F) {
  planetskey <- dataTableUniqueVector(planets)
  ckey <- list(as.character(c('buildNatalLongitudeAspects', symbol, planetskey, fwide)))
  planets.natal.aspsday <- secureLoadCache(key=ckey)

  if (is.null(planets.natal.aspsday) || clear) {
    # build natal positions
    natal.symbol <- buildNatalLongitudes(symbol)
    # extract only the planets longitudes
    planets <- planets[, c('Date', 'wday', planetsLonCols), with=F]
    # cartesian join
    natal.symbol.planets <- CJDT(natal.symbol, planets)
    # calculate aspects
    planets.natal.aspsday <- calculatePointsPlanetsAspects(natal.symbol.planets, fwide)

    saveCache(planets.natal.aspsday, key=ckey)
    cat("Set buildNatalLongitudeAspects cache\n")
  }

  return(planets.natal.aspsday)
}

# Calculate aspects for the significant longitude points and cache
# Usage: aspects.day <- buildSignificantLongitudesAspects(planets, security, 4, 10, F)
buildSignificantLongitudesAspects <- function(planets, security, degsplit, tsdate, tedate, topn, fwide=F, clear=F) {
  # To improve cache lookup use first and last rows and the number of rows and cols
  planetskey <- dataTableUniqueVector(planets)
  securitykey <- dataTableUniqueVector(security)
  ckey <- list(as.character(c('buildSignificantLongitudesAspects', planetskey, securitykey, degsplit, tsdate, tedate, topn, fwide)))
  planets.aspsday <- secureLoadCache(key=ckey)

  if (is.null(planets.aspsday) || clear) {
    # calculate the significance points
    siglons <- buildSignificantLongitudes(planets, security, degsplit, tsdate, tedate)
    # leave only the top N significant points
    siglons <- head(siglons, topn)
    # cartesian join
    planets.aspsday <- CJDT(siglons, planets)
    # calculate aspects
    planets.aspsday <- calculatePointsPlanetsAspects(planets.aspsday, fwide)

    saveCache(planets.aspsday, key=ckey)
    cat("Set buildSignificantLongitudesAspects cache\n")
  }

  return(planets.aspsday)
}

# Usage: daily.freq <- dailySignificantCutIndicators(planets, security, "1970-01-01", "2001-01-01", 6, 10, 10, 5)
dailySignificantCutIndicators <- function(planets, security, sdate, edate, degsplit, topn, aspsplit, decsplit, clear=F, breaks=c(-360, 360)) {
  # Significant points aspects
  planets.aspsday <- buildSignificantLongitudesAspects(planets, security, degsplit, topn, T, clear)
  cols <- colnames(planets.aspsday)
  indicators <- cols[grep('DIS.', cols)]
  # Melt the distances
  planets.long <- melt(planets.aspsday, id.var=c('Date'), measure.var=indicators)
  planets.long[, variable := paste(substr(variable, 1, 2), substr(variable, 6, 9), sep='')]
  setkeyv(planets.long, c('Date', 'variable'))
  # Melt the aspects
  planets.long.asps <- melt(planets.aspsday, id.var=c('Date'), measure.var=cols[grep('ASP.', cols)])
  planets.long.asps[, variable := paste(substr(variable, 1, 2), substr(variable, 6, 9), sep='')]
  setnames(planets.long.asps, c('Date', 'variable', 'asp'))
  setkeyv(planets.long.asps, c('Date', 'variable'))
  # Melt the orbs
  planets.long.orbs <- melt(planets.aspsday, id.var=c('Date'), measure.var=cols[grep('ORB.', cols)])
  planets.long.orbs[, variable := paste(substr(variable, 1, 2), substr(variable, 6, 9), sep='')]
  setnames(planets.long.orbs, c('Date', 'variable', 'orb'))
  setkeyv(planets.long.orbs, c('Date', 'variable'))
  # Set the real distance value
  planets.long[, rvalue := value]
  # Set indicator type
  planets.long[, type := 'spaspect']
  planets.long[, value := cut(value, breaks=seq(breaks[1], breaks[2], by=aspsplit))]
  sp <- merge(planets.aspsday, security, by=c('Date'))
  freq <- reportUpDownsFreq(sp, indicators, aspsplit, sdate, edate)
  freq[, variable := paste(substr(variable, 1, 2), substr(variable, 6, 9), sep='')]
  siglon.aspects.daily.freq <- merge(planets.long, freq, by=c('variable', 'value'))
  # Add the orbs & aspects
  siglon.aspects.daily.freq <- merge(siglon.aspects.daily.freq, planets.long.asps, by=c('Date', 'variable'))
  siglon.aspects.daily.freq <- merge(siglon.aspects.daily.freq, planets.long.orbs, by=c('Date', 'variable'))

  # security / planets table
  planets <- buildCompositeCols(planets)
  sp <- merge(security, planets, by='Date')
  cols <- colnames(planets)

  # Planets aspects
  indicators <- c(planetsCombLon, aspectsCompositeIndicators())
  planets.long <- melt(planets, id.var=c('Date'), measure.var=indicators)
  planets.long[, variable := substr(variable, 1, 4)]
  setkeyv(planets.long, c('Date', 'variable'))
  # Melt the aspects
  planets.long.asps <- melt(planets, id.var=c('Date'), measure.var=cols[grep('ASP$', cols)])
  planets.long.asps[, variable := substr(variable, 1, 4)]
  setnames(planets.long.asps, c('Date', 'variable', 'asp'))
  setkeyv(planets.long.asps, c('Date', 'variable'))
  # Melt the orbs
  planets.long.orbs <- melt(planets, id.var=c('Date'), measure.var=cols[grep('ORB$', cols)])
  planets.long.orbs[, variable := substr(variable, 1, 4)]
  setnames(planets.long.orbs, c('Date', 'variable', 'orb'))
  setkeyv(planets.long.orbs, c('Date', 'variable'))
  # Set the real distance value
  planets.long[, rvalue := value]
  planets.long[, type := 'aspect']
  planets.long[, value := cut(value, breaks=seq(breaks[1], breaks[2], by=aspsplit))]
  freq <- reportUpDownsFreq(sp, indicators, aspsplit, sdate, edate)
  freq[, variable := substr(variable, 1, 4)]
  aspects.daily.freq <- merge(planets.long, freq, by=c('variable', 'value'))
  # Add the orbs & aspects
  aspects.daily.freq <- merge(aspects.daily.freq, planets.long.asps, by=c('Date', 'variable'))
  aspects.daily.freq <- merge(aspects.daily.freq, planets.long.orbs, by=c('Date', 'variable'))

  # Declinations
  indicators <- c(planetsDecCols, declinationCompositeIndicators())
  planets.long <- melt(planets, id.var=c('Date'), measure.var=indicators)
  planets.long[, rvalue := value]
  planets.long[, type := 'declination']
  planets.long[, value := cut(value, breaks=seq(breaks[1], breaks[2], by=decsplit))]
  freq <- reportUpDownsFreq(sp, indicators, decsplit, sdate, edate)
  declinations.freq <- merge(planets.long, freq, by=c('variable', 'value'))
  declinations.freq[, asp := NA]
  declinations.freq[, orb := NA]
  # Use same cols order as pevious two tables
  declinations.freq <- declinations.freq[, colnames(aspects.daily.freq), with=F]

  # TODO: longitude indicators

  # combine rows
  daily.freq <- rbind(siglon.aspects.daily.freq, aspects.daily.freq, declinations.freq)
  setkey(daily.freq, Date)
  return(daily.freq)
}

# build the daily planets longitude aspects with frequencies
buildLongitudeAspectsFrequencies <- function(points.planets, security, sdate, edate) {
  cols <- colnames(points.planets)
  indicators <- cols[grep('ASP.', cols)]
  # Melt the distances
  planets.long <- melt(points.planets, id.var=c('Date'), measure.var=indicators)
  planets.long[, type := 'spaspect']
  planets.long[, variable := paste(substr(variable, 1, 2), substr(variable, 6, 12), sep='')]
  planets.long <- planets.long[!is.na(value),]
  planets.long <- planets.long[, value := as.factor(value)]
  setkeyv(planets.long, c('Date', 'variable'))
  # Melt the orbs
  planets.long.orbs <- melt(points.planets, id.var=c('Date'), measure.var=cols[grep('ORB.', cols)])
  planets.long.orbs[, variable := paste(substr(variable, 1, 2), substr(variable, 6, 12), sep='')]
  setnames(planets.long.orbs, c('Date', 'variable', 'orb'))
  setkeyv(planets.long.orbs, c('Date', 'variable'))
  # Calculate frequencies
  sp <- merge(points.planets, security, by=c('Date'))
  freq <- reportUpDownsFreq(sp, indicators, 0, sdate, edate)
  freq[, variable := paste(substr(variable, 1, 2), substr(variable, 6, 12), sep='')]
  aspects.daily.freq <- merge(planets.long, freq, by=c('variable', 'value'))
  # Add the orbs & aspects
  aspects.daily.freq <- merge(aspects.daily.freq, planets.long.orbs, by=c('Date', 'variable'))
  aspects.daily.freq[, orbdif := round(orb - Lag(orb), digits=2), by=c('variable', 'value')]
  setkey(aspects.daily.freq, Date)

  return(aspects.daily.freq)
}

# Usage: daily.freq <- dailySignificantAspectsIndicators(planets, security, "1970-01-01", "2001-01-01", 6, 10)
dailySignificantAspectsIndicators <- function(planets, security, sdate, edate, degsplit, topn, clear=F) {
  # Significant points aspects
  points.planets <- buildSignificantLongitudesAspects(planets, security, degsplit, topn, T, clear)
  aspects.daily.freq <- buildLongitudeAspectsFrequencies(points.planets, security, sdate, edate)
  return(aspects.daily.freq)
}

# Usage: daily.freq <- dailyNatalAspectsIndicators(symbol, planets, security, "1970-01-01", "2001-01-01")
dailyNatalAspectsIndicators <- function(symbol, planets, security, sdate, edate, clear=F) {
  # Significant points aspects
  points.planets <- buildNatalLongitudeAspects(symbol, planets, T)
  aspects.daily.freq <- buildLongitudeAspectsFrequencies(points.planets, security, sdate, edate)
  return(aspects.daily.freq)
}

# Usage: printDailySignificantIndicators('dailyf_AXP', daily.freq, '2001-01-01', '2015-01-01', 0.60, '>=', 'spaspect|aspect|declination', 'sig', 100, T)
printDailySignificantIndicators <- function(outfile, daily.freq, sdate, edate, th, op, ft, field='sig', masig, doplot=F) {
  printDay <- function(daily.freq.day, row.by) {
    cat("------------------------------", as.character(row.by[[1]]), "------------------------------\n")
    print(as.data.frame(daily.freq.day))
    print(colMeans(daily.freq.day[, 5:10, with=F]))
    cat("\t###", daily.freq.aggr[Date == as.Date(row.by[[1]]), V1], "\n")
    return(list())
  }

  # Redirect output
  sink(npath(paste("~/trading/predict/", outfile, ".txt", sep='')), append=F)
  # Filter the daily frequencies patterns
  daily.freq.filt <- daily.freq[eval(parse(text=paste('abs(get(field))', op, 'th'))),]
  if (ft != '') daily.freq.filt <- daily.freq.filt[grep(ft, type)]
  daily.freq.filt <- daily.freq.filt[Date >= as.Date(sdate) & Date < as.Date(edate),]
  # Calculate aggregated significance with SMA
  daily.freq.aggr <- daily.freq.filt[, mean(get(field)), by=Date]
  daily.freq.aggr[, V1 := SMA(V1, masig)]
  # Print the daily report
  daily.freq.filt[, printDay(.SD, .BY), by=as.character(Date)]
  sink()

  if (doplot) {
    ggplot(data=daily.freq.aggr) + geom_line(aes(x=Date, y=V1))
  }
}

# Usage: analizeIndicatorCorrelation(daily.freq, psl$security, "2001-01-01", "2015-01-01", 100, 'spaspect|aspect|declination', 'sig', 50, T)
# analizeIndicatorCorrelation(daily.freq[value %in% c(0, 90, 180) & abs(sig) >= 0.6,], psl$security, "2001-01-01", "2015-01-01", 50, 'spaspect', 'sig', 50, T)
analizeIndicatorCorrelation <- function(daily.freq, securityorig, sdate, edate, masl, ft, field='sig', masig=50, doplot=F, align=0, browse=F) {
  daily.freq.filt <- daily.freq
  if (ft != '') daily.freq.filt <- daily.freq.filt[grep(ft, type)]
  daily.freq.filt <- daily.freq.filt[Date >= as.Date(sdate) & Date < as.Date(edate),]
  daily.freq.aggr <- daily.freq.filt[, mean(get(field)), by=Date]
  daily.freq.aggr[, V1 := SMA(V1, masig)]
  security <- copy(securityorig)
  security[, MidMAS := SMA(Mid, masl)]
  daily.freq.aggr.sec <- merge(security, daily.freq.aggr, by='Date')

  # apply alignment to left & right
  if (align > 0) {
    daily.freq.aggr.sec[, V1 := c(V1[(align+1):length(V1)], rep(NA, align))]
  }
  else if (align < 0) {
    daily.freq.aggr.sec[, V1 := c(rep(NA, abs(align)), V1[1:(length(V1)-abs(align))])]
  }

  daily.freq.a.sec.long <- melt(daily.freq.aggr.sec, variable.name='type', value.name='value', measure.var=c('V1', 'MidMAS'))
  sigcor <- daily.freq.aggr.sec[, cor(MidMAS, V1, use="pairwise", method='spearman')]
  print(sigcor)

  if (browse) {
    browser()
  }

  if (doplot) {
    ggplot(data=daily.freq.a.sec.long) + geom_line(aes(x=Date, y=value)) + facet_grid(type ~ ., scale='free')
  }
}
