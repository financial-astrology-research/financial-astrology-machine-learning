library(GA)
library(R.cache)
library(RSQLite)
library(compiler)
library(data.table)
library(ggplot2)
library(microbenchmark)
library(reshape2)
library(splus2R)
# no scientific notation
options(scipen=100)
options(width=130)
options(error=recover)
enableJIT(0)
enableWD <- FALSE

`%ni%` <- Negate(`%in%`)
planetsBaseCols <- c('SU', 'MO', 'ME', 'VE', 'MA', 'CE', 'JU', 'SA', 'UR', 'NE', 'PL', 'NN')
# Aspects and orbs
aspects            <- c(0 , 30, 36, 40, 45, 51, 60, 72, 80, 90, 103, 108, 120, 135, 144, 154, 180)
deforbs            <- c(12, 1 , 1 , 1 , 1 , 1 , 8 , 1 , 1 , 8 , 1  , 1  , 8  , 1  , 1  , 1  , 12)
defaspectsenergy   <- c(10, 2 , 2 , 2 , 2 , 2 , 6 , 2 , 2 , 6 , 2  , 2  , 6  , 2  , 2  , 2  , 10)
defaspectspolarity <- c(NA, 1 , 0 , 0 , 1 , 0 , 1 , 0 , 1 , 0 , 0  , 0  , 1  , 0  , 0  , 1  , 0)
defaspectspolarity <- defaspectspolarity[!is.na(defaspectspolarity)]

# columns names
aspOrbsCols <- as.character(apply(expand.grid(aspects, planetsBaseCols[1:(length(planetsBaseCols)-1)]), 1, function(x) paste(x[2], x[1], sep='')))
planetsLonCols <- paste(planetsBaseCols, 'LON', sep='')
planetsLonGCols <- paste(planetsLonCols, 'G', sep='')
planetsLatCols <- paste(planetsBaseCols, 'LAT', sep='')
planetsSpCols <- paste(planetsBaseCols, 'SP', sep='')
planetsSpGCols <- paste(planetsSpCols, "G", sep="")
planetsCombLon <- combn(planetsLonCols, 2, simplify=F)
planetsCombLonCols <- as.character(lapply(planetsCombLon, function(x) paste(x[1], x[2], sep='')))
planetsCombLonOrbCols <- paste(planetsCombLonCols, 'ORB', sep='')
aspectsEnergyCols <- paste(aspects, 'E', sep='')
planetsEnergyCols <- paste(planetsBaseCols, 'E', sep='')

deforbsmatrix = matrix(deforbs, nrow = 1, ncol = length(deforbs), dimnames = list('orbs', aspects))
defconjpolarity <- list(SULONMOLON=1, SULONMELON=1, SULONVELON=1, SULONMALON=0, SULONJULON=1, SULONSALON=0, SULONURLON=1, SULONNELON=0,
                        SULONPLLON=0, SULONNNLON=1, MOLONMELON=1, MOLONVELON=1, MOLONMALON=0, MOLONJULON=1, MOLONSALON=0, MOLONURLON=0,
                        MOLONNELON=1, MOLONPLLON=0, MOLONNNLON=1, MELONVELON=1, MELONMALON=0, MELONJULON=1, MELONSALON=0, MELONURLON=1,
                        MELONNELON=1, MELONPLLON=0, MELONNNLON=1, VELONMALON=0, VELONJULON=1, VELONSALON=0, VELONURLON=1, VELONNELON=1,
                        VELONPLLON=1, VELONNNLON=1, MALONJULON=0, MALONSALON=0, MALONURLON=0, MALONNELON=0, MALONPLLON=0, MALONNNLON=0,
                        JULONSALON=0, JULONURLON=1, JULONNELON=1, JULONPLLON=0, JULONNNLON=1, SALONURLON=0, SALONNELON=0, SALONPLLON=0,
                        SALONNNLON=0, URLONNELON=0, URLONPLLON=0, URLONNNLON=0, NELONPLLON=0, NELONNNLON=0, PLLONNNLON=0)

defconjpolaritymatrix <- matrix(0, nrow = length(planetsCombLonCols), ncol = 1, dimnames = list(planetsCombLonCols, c('0')))
# set default polarity for aspects
defconjpolaritymatrix[names(defconjpolarity), '0'] <- as.integer(defconjpolarity)
defconjpolarity <- as.integer(defconjpolaritymatrix)
defpolarity <- c(defconjpolarity, defaspectspolarity)
aspectspolaritycols <- aspects[2:length(aspects)]

defplanetsenergy <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
defpanalogy <- list(c(2, 3, 4, 5, 6, 7, 8, 1, 1, 1, 1, 1),
                    c(2, 3, 4, 5, 6, 7, 8, 2, 2, 2, 2, 2),
                    c(2, 3, 4, 5, 6, 7, 8, 3, 3, 3, 3, 3),
                    c(2, 3, 4, 5, 6, 7, 8, 4, 4, 4, 4, 4),
                    c(2, 3, 4, 5, 6, 7, 8, 5, 5, 5, 5, 5),
                    c(2, 3, 4, 5, 6, 7, 8, 6, 6, 6, 6, 6),
                    c(2, 3, 4, 5, 6, 7, 8, 7, 7, 7, 7, 7),
                    c(2, 3, 4, 5, 6, 7, 8, 8, 8, 8, 8, 8))

zodSignsCols <- c('AR', 'TA', 'GE', 'CA', 'LE', 'VI', 'LI', 'SC', 'SA', 'CA', 'AC', 'PI')
planetsZodEnergyCols <- as.character(apply(expand.grid(planetsLonCols, zodSignsCols), 1, function(x) paste(x[1], '_', x[2], sep='')))
zodenergy.su <- c( 10,  01,  01,  01,  10,  01, -10,  01,  01,  01, -10,  01)
zodenergy.mo <- c( 01,  10,  01,  10,  01,  01,  01, -10,  01, -10,  01,  01)
zodenergy.me <- c( 01,  01,  10,  01,  01,  10,  01,  01, -10,  01,  01, -10)
zodenergy.ve <- c(-10,  10,  01,  01,  01, -10,  10, -10,  01,  01,  01,  10)
zodenergy.ma <- c( 10,  01,  01, -10,  01,  01, -10,  01,  01,  10,  01,  01)
zodenergy.ce <- c( 01,  01,  01,  01,  10,  01,  01,  01,  01,  01,  01,  01)
zodenergy.ju <- c( 01,  01, -10,  10,  01, -10,  01,  01,  10, -10,  01,  10)
zodenergy.sa <- c(-10,  01,  01, -10, -10,  01,  10,  01,  01,  10,  10,  01)
zodenergy.ur <- c( 01,  01,  01,  01, -10,  01,  01,  01,  01,  01,  10,  01)
zodenergy.ne <- c( 01,  01,  01,  01,  01, -10,  01,  01,  01,  01,  01,  10)
zodenergy.pl <- c( 01, -10,  01,  01,  01,  01,  01,  10,  01,  01,  01,  01)
zodenergy.nn <- c( 01,  01,  01,  01,  10,  01,  01,  01,  01,  01,  01,  01)

defplanetszodenergy <- c(zodenergy.su, zodenergy.mo, zodenergy.me, zodenergy.ve, zodenergy.ma, zodenergy.ce,
                         zodenergy.ju, zodenergy.sa, zodenergy.ur, zodenergy.ne, zodenergy.pl, zodenergy.nn)

# a function that returns the position of n-th largest
maxn <- function(x, n) {
  order_x <- order(x, decreasing = TRUE)
  if ( length(order_x) < n ) {
    n = length(order_x)
  }
  x[order_x[n]]
}

decToDeg <- function(num) {
  num <- abs(num)
  d <- as.integer(num)
  part <- (num-d)*60
  m <- as.integer(part)
  s <- as.integer((part-m)*60)
  c(d, m, s)
}

generateSamples <- function(ds, n) {
  total <- nrow(ds)
  # build test samples
  samples <- list()
  for (i in 1:n) {
    samples[[i]] <- ds[sample(total, total/2),]
  }
  # finally add the entire data set as a sample
  samples[[length(samples)+1]] <- ds
  samples
}

gaint_Population <- function (object, ...) {
  pdefpanalogy <- 0.6
  pdefpolarity <- 0.3
  pdeforb <- 0.3
  pdefaspectenergy <- 0.3
  pdefplanetenergy <- 0.3
  pdefplanetzodenergy <- 0.5
  min <- object@min
  max <- object@max
  nvars <- length(min)
  nvars1.e <- nvars-length(defpanalogy[[1]])-length(defpolarity)-length(deforbs)-length(defaspectsenergy)-length(defplanetsenergy)-length(defplanetszodenergy)
  nvars2.s <- nvars1.e+1
  nvars2.e <- nvars1.e+length(defpanalogy[[1]])
  nvars3.s <- nvars2.e+1
  nvars3.e <- nvars2.e+length(deforbs)
  nvars4.s <- nvars3.e+1
  nvars4.e <- nvars3.e+length(defpolarity)
  nvars5.s <- nvars4.e+1
  nvars5.e <- nvars4.e+length(defaspectsenergy)
  nvars6.s <- nvars5.e+1
  nvars6.e <- nvars5.e+length(defplanetsenergy)
  nvars7.s <- nvars6.e+1
  nvars7.e <- nvars6.e+length(defplanetszodenergy)

  population <- matrix(NA, nrow = object@popSize, ncol = nvars)
  for (j in 1:nvars) {
    population[, j] <- sample(min[j]:max[j], object@popSize, replace=TRUE)
  }

  for (i in 1:nrow(population)) {
    # override by default panalogy
    if (pdefpanalogy > runif(1)) {
      population[i, nvars2.s:nvars2.e] <- defpanalogy[[sample(1:length(defpanalogy), 1)]]
    }
    # override by default polarities
    if (pdeforb > runif(1)) {
      population[i, nvars3.s:nvars3.e] <- deforbs
    }
    # override by default orbs
    if (pdefpolarity > runif(1)) {
      population[i, nvars4.s:nvars4.e] <- defpolarity
    }
    # override by default aspects energy
    if (pdefaspectenergy > runif(1)) {
      population[i, nvars5.s:nvars5.e] <- defaspectsenergy
    }
    # override by default planets energy
    if (pdefplanetenergy > runif(1)) {
      population[i, nvars6.s:nvars6.e] <- defplanetsenergy
    }
    # override by default planets zod energy
    if (pdefplanetzodenergy > runif(1)) {
      population[i, nvars7.s:nvars7.e] <- defplanetszodenergy
    }
  }

  return(population)
}

gaint_raMutation <- function(object, parent) {
  mutate <- parent <- as.vector(object@population[parent, ])
  n <- length(parent)
  randn <- runif(1)

  # with 30% probability alter 1%, 30% alter 2% and 40% alter 0.5%
  if (0.3 > randn) {
    j <- sample(1:n, size = round(n*0.01))
  }
  else if (0.7 < randn) {
    j <- sample(1:n, size = round(n*0.02))
  }
  else {
    j <- sample(1:n, size = round(n*0.005))
  }

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

gaint_blxCrossover <- function (object, parents, ...) {
  parents <- object@population[parents, , drop = FALSE]
  n <- ncol(parents)
  a <- 0.5
  children <- matrix(NA, nrow = 2, ncol = n)
  for (i in 1:n) {
    x <- sort(parents[, i])
    xl <- max(x[1] - a * (x[2] - x[1]), object@min[i])
    xu <- min(x[2] + a * (x[2] - x[1]), object@max[i])
    children[, i] <- round(runif(2, xl, xu))
  }
  out <- list(children = children, fitness = NA)
  return(out)
}

npath <- function(path) {
  normalizePath(path.expand(path))
}

cmpTestPlanetsSignificanceRelative <- function(execfunc, sinkfile, ...) {
  if (!hasArg('execfunc')) stop("Provide function to execute")
  ptm <- proc.time()

  # determine the current system version
  system("cd ~/trading")
  branch.name <- system2("git", "rev-parse --abbrev-ref HEAD", stdout=T)

  getFitnessBest <- function(db) {
    return(as.numeric(dbGetQuery(db, "select * from keys where key='fitness.best'")[2]))
  }

  setFitnessBest <- function(db, value) {
    dbBeginTransaction(db)
    sql <- "update keys set count = ? where key = 'fitness.best'"
    dbGetPreparedQuery(db, sql, bind.data = data.frame(count=value))
    return(dbCommit(db))
  }

  initSQLiteDB <- function() {
    db <- dbConnect(SQLite(), dbname = "citlacom.db")
    # create table if not exists
    if (!dbExistsTable(db, 'keys')) {
      sql <- "create table keys (key text PRIMARY KEY, count integer)"
      dbGetQuery(db, sql)
      sql <- "insert into keys values (?, ?)"
      fitness.best <- data.frame(key='fitness.best', count=-100)
      dbGetPreparedQuery(db, sql, bind.data = fitness.best)
    }
    else {
      setFitnessBest(db, -100)
    }
    return(db)
  }

  dbcon <- initSQLiteDB()
  closeSQLiteDB <- function(db) {
    dbDisconnect(db)
  }

  # open a security historic file
  openSecurity <- function(security_file, mapricetype, mapricefs, mapricesl, dateformat="%Y.%m.%d", pricemadir=2) {
    mapricefunc <- get(get('mapricetype'))
    security_file <- npath(security_file)
    security <- fread(security_file)
    security[, Date := as.Date(as.character(Date), format=dateformat)]
    security[, Year := as.character(format(Date, "%Y"))]
    setkey(security, 'Date')
    security[, Mid := (High + Low + Close + Open) / 4]

    if (pricemadir == 1) {
      security[, MidMAF := mapricefunc(Mid, n=mapricefs)]
      security[, MidMAS := mapricefunc(Mid, n=round(mapricefs * mapricesl))]
      security[, val := MidMAF-MidMAS]
    }
    else if (pricemadir == 2) {
      security[, MidMAF := rev(mapricefunc(rev(Mid), n=mapricefs))]
      security[, MidMAS := rev(mapricefunc(rev(Mid), n=round(mapricefs * mapricesl)))]
      security[, val := MidMAS-MidMAF]
    }
    else {
      stop("No valid pricemadir was provided.")
    }

    if (all(security$val == 0)) {
      stop("Undetermined security price direction")
    }

    security <- security[!is.na(val)]
    security[, Eff := cut(val, c(-10000, 0, 10000), labels=c('down', 'up'), right=FALSE)]

    return(security)
  }

  openPlanets <- function(planetsfile, cusorbs) {
    args <- list(...)
    ckey <- list(planetsfile, cusorbs)
    planets <- loadCache(key=ckey, dirs=c(args$securityfile))
    if (is.null(planets)) {
      Date=DateMT4=Year=NULL
      planetsfile <- npath(paste("~/trading/dplanets/", planetsfile, ".tsv", sep=""))
      planets <- fread(planetsfile, sep="\t", na.strings="", verbose = F)
      planets[, Date := as.Date(planets$Date, format="%Y-%m-%d")]
      planets <- planets[, c('Date', planetsLonCols, planetsSpCols), with=F]
      #planets[, DateMT4 := as.character(format(Date, "%Y.%m.%d"))]
      planets[, Year := as.character(format(Date, "%Y"))]
      planets[, wday := format(Date, "%w")]
      setkey(planets, 'Date')

      # calculate longitudinal differences
      for (curcol in planetsCombLonCols) {
        col1 <- substr(curcol, 1, 5)
        col2 <- substr(curcol, 6, 10)
        combnameorb <- paste(curcol, 'ORB', sep='')
        planets[, c(curcol) := abs(get(col1) - get(col2))]
      }

      exprcopy <- paste("c(planetsCombLonOrbCols) := list(", paste(planetsCombLonCols, collapse=","), ")", sep="")
      planets[, eval(parse(text = exprcopy))]

      # calculate aspects for max orbs
      orbsmatrix <- matrix(cusorbs, nrow = 1, ncol = length(aspects), byrow = TRUE, dimnames = list('orbs', aspects))
      planets <- processPlanetsAspects(planets, orbsmatrix)
      saveCache(planets, key=ckey, dirs=c(args$securityfile))
      cat("Set openPlanets cache\n")
    }
    else {
      cat("Get openPlanets cache\n")
    }
    return(planets)
  }

  # process the degsplit for a cloned original planets dt
  processPlanetsDegsplit <- function(planetsorig, degsplit) {
    # calculate the lon deg splits
    calculateLonGroups <- function(x, degsplit) {
      return(cut(x, seq(0, 360, by=degsplit)))
    }

    planets <- copy(planetsorig)
    planets[, c(planetsLonGCols) := lapply(.SD, calculateLonGroups, degsplit=degsplit), .SDcols=planetsLonCols]
    return(planets)
  }

  # calculate the planets aspects for a given solution
  processPlanetsAspects <- function(planetsorig, cusorbs) {
    # clone original to ensure is no modified
    planets <- copy(planetsorig)

    calculateAspects <- function(x) {
      allidx <- rep(FALSE, length(x))
      for (aspect in aspects) {
        comborb <- cusorbs['orbs', as.character(aspect)]
        # for a0 the start is 360+(orb)
        if (aspect == 0) {
          rstart <- 360-comborb
          rend <- aspect+comborb
          idx <- x >= rstart | x <= rend
        }
        else {
          rstart <- aspect-comborb
          rend <- aspect+comborb
          idx <- x >= rstart & x <= rend
        }
        allidx[idx] <- TRUE
        x[idx] <- aspect
      }
      # put NA no aspects
      x[!allidx] <- NA
      return(x)
    }

    calculateAspectOrbs <- function(x) {
      for (aspect in aspects) {
        comborb <- cusorbs['orbs', as.character(aspect)]
        if (aspect == 0) {
          rstart <- 360-comborb
          rend <- aspect+comborb
          # first with aspect in mode 360
          idx <- x >= rstart
          x[idx] <- x[idx] - 360
          # second in mode 0
          idx <- x <= rend
          x[idx] <- x[idx]
        }
        else {
          rstart <- aspect-comborb
          rend <- aspect+comborb
          idx <- x >= rstart & x <= rend
          x[idx] <- aspect - x[idx]
        }
      }
      return(x)
    }

    planets[, c(planetsCombLonCols) := lapply(.SD, calculateAspects), .SDcols=planetsCombLonCols]
    planets[, c(planetsCombLonOrbCols) := lapply(.SD, calculateAspectOrbs), .SDcols=planetsCombLonOrbCols]
    return(planets)
  }

  # calculate the proportional energy of aspect based on the distance
  energyGrowth <- function(energy, distance, speed) {
    return(energy * (1 - speed) ^ abs(distance))
  }

  planetsVarsSignificance <- function(planets, currency) {
    # build significance table analogy for each planet
    planetTableAanalogy <- function(curcol) {
      planet.significance <- copy(significance)
      planet.significance[, origin := curcol]
      return(planet.significance)
    }

    planets  <- merge(planets, currency, by='Date')
    planets.long <- data.table(melt(planets, id.var=c('Date', 'Eff'), measure.var=planetsLonGCols))
    # calculate the signficance for each long
    significance <- planets.long[, cbind(as.list(prop.table(as.numeric(table(Eff)))), as.list(as.numeric(table(Eff)))), by=c('variable', 'value')]
    setnames(significance, c('variable', 'key', 'V1', 'V2', 'V3', 'V4'))
    significance.full <- rbindlist(lapply(planetsLonCols, planetTableAanalogy))
    significance.full[, c('pdiff', 'keyidx') := list(V2-V1, paste(key, variable, origin, sep='_'))]
    setkey(significance.full, 'keyidx', 'V1', 'V2')
    return(significance.full)
  }

  planetsVarsSignificanceFilter <- function(significance, threshold) {
    significance <- significance[pdiff >= threshold | pdiff <= -threshold]
    significance <- significance[!is.na(key)]
    significance[, Eff := cut(pdiff, c(-1000, 0, 1000), labels=c('down', 'up'), right=FALSE)]
    return(significance)
  }

  # build the daily signficance table
  buildDailySignificance <- function(significance, planets.pred, panalogymatrix) {
    # build daily significance indexes
    buildDailySignificanceIdxs <- function(planets.day) {
      curdate <- planets.day[['Date']]
      sigidxs <- paste(planets.day[planetsLonGCols], panalogymatrix['analogy', planetsLonGCols], planetsLonCols, sep='_')
      datelist <- rep(curdate, length(sigidxs))
      day.idxs <- list(Date=datelist, keyidx=sigidxs)
      return(day.idxs)
    }

    significance.days.idxs <- rbindlist(apply(planets.pred, 1, buildDailySignificanceIdxs))
    setkeyv(significance.days.idxs, c('Date', 'keyidx'))
    significance.days <- merge(significance.days.idxs, significance, by='keyidx')
    # build daily planets lon & sp tables
    planets.lon <- data.table(melt(planets.pred, id.var=c('Date'), measure.var=planetsLonCols))
    planets.lon[, origin := variable]
    planets.lon[, Date := as.character(Date, format="%Y-%m-%d")]
    setnames(planets.lon, c('Date', 'variable', 'lon', 'origin'))
    planets.sp <- data.table(melt(planets.pred, id.var=c('Date'), measure.var=planetsSpCols))
    planets.sp[, origin := gsub('SP', 'LON', variable)]
    planets.sp[, Date := as.character(Date, format="%Y-%m-%d")]
    setnames(planets.sp, c('Date', 'variable', 'sp', 'origin'))
    # merge significance with lon & sp
    significance.days <- merge(significance.days, planets.lon, by=c('Date', 'origin'))
    significance.days <- merge(significance.days, planets.sp, by=c('Date', 'origin'))
    setkeyv(significance.days, 'Date', 'origin')
    significance.days[, zsign := ceiling(lon/30)]
    # remove no used cols
    significance.days[, c('variable.x', 'variable.y', 'variable') := NULL]
    return(significance.days)
  }

  # process the daily aspects energy
  dayAspectsEnergy <- function(planets.day, panalogy, aspectspolarity, conjpolarity, aspectsenergy, planetsenergy, energygrowthsp, asporbs) {
    curdate <- planets.day[['Date']]
    activecols <- planetsCombLonCols[grep(planets.day[['sigpatterns']], planetsCombLonCols, perl=T)]
    # ignore anon aspects or non active
    planets.day.asp <- planets.day[!is.na(planets.day) & names(planets.day) %in% activecols]
    energy <- list()

    # no significant positions for this day
    if (length(planets.day.asp) == 0) {
      return(data.table())
    }

    # build energy aspects table
    for (idx in 1:length(planets.day.asp)) {
      aspect <- as.character(as.numeric(planets.day.asp[idx]))
      curcol <- names(planets.day.asp[idx])
      curcolorb <- paste(curcol, 'ORB', sep='')
      distance <- as.numeric(planets.day[[curcolorb]])
      # ignore aspects that are out of allowed orb
      if (distance > asporbs['orbs', aspect]) next;
      # code of planets involved in the aspect
      col1 <- substr(curcol, 1, 5)
      col2 <- substr(curcol, 6, 10)
      # energy for planets
      planetenergy1 <- planetsenergy['energy', col1]
      planetenergy2 <- planetsenergy['energy', col2]
      # determine aspect energy based on aspect and involved planets
      aspectenergy <- aspectsenergy['energy', aspect] * (planetenergy1 + planetenergy2)
      # determine aspect polarity
      if (aspect == '0') {
        aspectpolarity <- conjpolarity['polarity', curcol]
      }
      else {
        aspectpolarity <- aspectspolarity['polarity', aspect]
      }

      # compute the given energy based on the aspect orb distance
      aspectenergydis <- energyGrowth(abs(aspectenergy), distance, energygrowthsp)

      if (aspectpolarity == 1) {
        up <- aspectenergydis
        down <- 0
      }
      else if (aspectpolarity == 0) {
        up <- 0
        down <- aspectenergydis
      }
      else {
        stop(paste("No valid polarity was provided - ", curcol, aspect))
      }

      energy[[length(energy)+1]] <- list(Date=curdate, origin=col1, up=up, down=down)
      energy[[length(energy)+1]] <- list(Date=curdate, origin=col2, up=up, down=down)
    }

    # convert energy list to data table
    energy <- rbindlist(energy)
    return(energy)
  }

  # process the significance rows energy
  dailySignificanceEnergy <- function(significance.days, energyret, planetszodenergy) {
    processSignificanceRow <- function(significance.row, by.row) {
      watchdog <- list()
      energy <- 0
      # add retrograde energy
      if (significance.row$sp < 0) {
        energy <- energy + energyret
        if (enableWD) {
          watchdog[length(watchdog)+1] <- paste(energy, by.row$origin, ' Re')
        }
      }
      # add the planet zodiacal energy
      zodenergy <- planetszodenergy[by.row$origin, significance.row$zsign]
      energy <- energy + zodenergy
      if (enableWD) {
        watchdog[length(watchdog)+1] <- paste(zodenergy, by.row$origin, ' zod ', significance.row$zsign)
        return(list(energy=energy, watchdog=paste(watchdog, collapse=' / ')))
      }
      else {
        return(list(energy=energy, watchdog=''))
      }
    }

    # process each significance row
    significance.days[, c('energy', 'watchdog') := processSignificanceRow(.SD, .BY), by=c('Date', 'origin')]
    # invert the significance columns for negative retrograde energy
    significance.days[energy < 0, c('V1', 'V2', 'V3', 'V4') := list(V2, V1, V4, V3)]
    return(significance.days)
  }

  # aggregate the daily energy and apply it with the daily significance energy
  # to calculate the final prediction
  calculatePrediction <- function(significance.days, energy.days, energymode) {
    energy.sum <- energy.days[, list(sum(up), sum(down)), by=list(Date, origin)]
    setnames(energy.sum, c('Date', 'origin', 'up', 'down'))
    setkeyv(energy.sum, c('Date', 'origin'))
    significance.days <- merge(significance.days, energy.sum, by=c('Date', 'origin'))
    setkeyv(significance.days, c('Date', 'Eff'))
    significance.days[, c('up', 'down') := list(up + abs(energy), down + abs(energy))]

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

  # build daily significance patterns
  buildDaySignificancePatterns <- function(origin) {
    patterns <- paste(strtrim(origin, 5), collapse='|', sep='')
    return(list(sigpatterns=patterns))
  }

  # print a year solution summary
  printPredYearSummary <- function(x, type) {
    cat("\t ", type, "test: vol =", x['volatility'], " - cor =", x['correlation'])
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
    mapricefunc <- get(args$mapricetype)
    mapredfunc <- get('SMA')
    rdates <- as.Date(with(args, c(tsdate, tedate, vsdate, vedate, csdate, cedate)))
    new.fitness.best <- ""

    # build matrix
    orbsmatrix <- matrix(args$cusorbs, nrow = 1, ncol = length(aspects), byrow = TRUE, dimnames = list('orbs', aspects))

    # aspects polarities
    pol1 <- args$aspectspolarity[1:length(defconjpolarity)]
    pol2 <- args$aspectspolarity[(length(defconjpolarity)+1):length(args$aspectspolarity)]
    conjpolaritymatrix <- matrix(pol1, nrow = 1, ncol = length(planetsCombLonCols), byrow = TRUE, dimnames = list('polarity', planetsCombLonCols))
    aspectspolaritymatrix <- matrix(pol2, nrow = 1, ncol = length(aspectspolaritycols), byrow = TRUE, dimnames = list('polarity', aspectspolaritycols))

    aspectsenergymatrix <- matrix(args$aspectsenergy, nrow = 1, ncol = length(args$aspectsenergy), byrow = TRUE,
                                  dimnames = list(c('energy'), aspects))

    planetsenergymatrix <- matrix(args$planetsenergy, nrow = 1, ncol = length(args$planetsenergy), byrow = TRUE,
                                  dimnames = list(c('energy'), planetsLonCols))

    planetszodenergymatrix <- matrix(args$planetszodenergy, nrow = length(planetsLonCols), ncol = 12, byrow = TRUE,
                                     dimnames = list(planetsLonCols, zodSignsCols))

    panalogymatrix <- matrix(args$panalogy, nrow = 1, ncol = length(args$panalogy), byrow = TRUE,
                             dimnames = list(c('analogy'), planetsLonGCols))

    sout <- with(args, paste("testPlanetsSignificanceRelative('testSolution', securityfile=", shQuote(securityfile), ", planetsfile=", shQuote(planetsfile),
                             ", tsdate=", shQuote(tsdate), ", tedate=", shQuote(tedate), ", vsdate=", shQuote(vsdate), ", vedate=", shQuote(vedate),
                             ", csdate=", shQuote(csdate), ", cedate=", shQuote(cedate),
                             ", mapredsm=", mapredsm, ", mapricefs=", mapricefs, ", mapricesl=", mapricesl,
                             ", mapricetype=", shQuote(mapricetype),
                             ", degsplit=", degsplit, ", threshold=", threshold,
                             ", energymode=", energymode, ", energygrowthsp=", energygrowthsp, ", energyret=", energyret, ", alignmove=", alignmove,
                             ", panalogy=c(", paste(shQuote(panalogy), collapse=", "), ")",
                             ", cusorbs=c(", paste(cusorbs, collapse=", "), ")",
                             ", aspectsenergy=c(", paste(aspectsenergy, collapse=", "), ")",
                             ", planetsenergy=c(", paste(planetsenergy, collapse=", "), ")",
                             ", planetszodenergy=c(", paste(planetszodenergy, collapse=", "), ")",
                             ", aspectspolarity=c(", paste(aspectspolarity, collapse=", "), ")",
                             ", dateformat=", shQuote(dateformat), ", verbose=F", ", doplot=T", ", fittype=", shQuote(fittype), ")\n", sep=""))

    sout <- paste("system version: ", branch.name, "\n\n", sout, sep="")
    # use a cloned planets to ensure original is no modified
    ckey <- list('processPlanetsDegsplit', args$degsplit)
    planets <- loadCache(key=ckey, dirs=c(args$securityfile))
    if (is.null(planets)) {
      planets <- processPlanetsDegsplit(args$planetsorig, args$degsplit)
      saveCache(planets, key=ckey, dirs=c(args$securityfile))
      cat("Set processPlanetsDegsplit cache\n")
    }
    else {
      cat("Get processPlanetsDegsplit cache\n")
    }

    # split in training and prediction
    planets.train <- planets[Date > rdates[1] & Date <= rdates[2] & wday %in% c(1, 2, 3, 4, 5)]
    planets.pred <- planets[Date > rdates[3] & Date <= rdates[6] & wday %in% c(1, 2, 3, 4, 5)]

    # load the security data
    ckey <- with(args, list('openSecurity', mapricetype, mapricefs, mapricesl))
    security <- loadCache(key=ckey, dirs=c(args$securityfile))
    if (is.null(security)) {
      security <- with(args, openSecurity(paste("~/trading/", securityfile, ".csv", sep=''), mapricetype, mapricefs, mapricesl, dateformat))
      saveCache(security, key=ckey, dirs=c(args$securityfile))
      cat("Set openSecurity cache\n")
    }
    else {
      cat("Get openSecurity cache\n")
    }

    # process the planets significance table
    ckey <- with(args, list('planetsVarsSignificance', degsplit, mapricetype, mapricefs, mapricesl))
    significance <- loadCache(key=ckey, dirs=c(args$securityfile))
    if (is.null(significance)) {
      significance <- with(args, planetsVarsSignificance(planets.train, security))
      saveCache(significance, key=ckey, dirs=c(args$securityfile))
      cat("Set planetsVarsSignificance cache\n")
    }
    else {
      cat("Get planetsVarsSignificance cache\n")
    }

    # filter the significance by threshold
    significance <- planetsVarsSignificanceFilter(significance, args$threshold)
    # build significance by days
    ckey <- with(args, list('buildDailySignificance', degsplit, mapricetype, mapricefs, mapricesl, panalogymatrix))
    significance.days <- loadCache(key=ckey, dirs=c(args$securityfile))
    if (is.null(significance.days)) {
      significance.days <- buildDailySignificance(significance, planets.pred, panalogymatrix)
      saveCache(significance.days, key=ckey, dirs=c(args$securityfile))
      cat("Set buildDailySignificance cache\n")
    }
    else {
      cat("Get buildDailySignificance cache\n")
    }

    ckey <- with(args, list('buildDailySignificance', degsplit, mapricetype, mapricefs, mapricesl, panalogymatrix, energyret))
    ckey[[length(ckey)+1]] <- planetszodenergymatrix
    significance.daysen <- loadCache(key=ckey, dirs=c(args$securityfile))
    if (is.null(significance.daysen)) {
      significance.daysen <- dailySignificanceEnergy(significance.days, args$energyret, planetszodenergymatrix)
      saveCache(significance.daysen, key=ckey, dirs=c(args$securityfile))
      cat("Set dailySignificanceEnergy cache\n")
    }
    else {
      cat("Get dailySignificanceEnergy cache\n")
    }

    significance.patterns <- significance.daysen[, buildDaySignificancePatterns(origin), by=Date]
    significance.patterns[, Date := as.Date(Date, format="%Y-%m-%d")]
    planets.pred <- merge(planets.pred, significance.patterns, by='Date')
    # helper function to process day aspects energy
    processPlanesDaySignificance <- function(x) {
      dayAspectsEnergy(x, panalogymatrix, aspectspolaritymatrix, conjpolaritymatrix, aspectsenergymatrix,
                       planetsenergymatrix, args$energygrowthsp, orbsmatrix)
    }

    energy.days <- rbindlist(apply(planets.pred, 1, processPlanesDaySignificance))
    # calculate prediction
    prediction <- calculatePrediction(significance.daysen, energy.days, args$energymode)
    planets.pred <- planets.pred[prediction]
    planets.pred <- security[planets.pred]
    setkeyv(planets.pred, c('Date', 'Year.1'))
    # smoth the prediction serie
    planets.pred[, predval := mapredfunc(predRaw, args$mapredsm)]
    # apply alignment to left & right
    if (args$alignmove > 0) {
      planets.pred[, predval := c(predval[(args$alignmove+1):length(predval)], rep(NA, args$alignmove))]
    }
    else if (args$alignmove < 0) {
      planets.pred[, predval := c(rep(NA, abs(args$alignmove)), predval[1:(length(predval)-abs(args$alignmove))])]
    }

    planets.pred[, predEff := predval]
    # determine a factor prediction response
    planets.pred[, predFactor := cut(predEff, c(-10000, 0, 10000), labels=c('down', 'up'), right=FALSE)]
    # plot solution snippet if doplot is enabled
    if (args$doplot) {
      snippet <- paste(strwrap(sout, width=170), collapse="\n")
      plotSolutionSnippet(snippet)
    }
    # helper function to process predictions by year
    pltitle <- paste('Yearly prediction VS price movement for ', args$securityfile)
    processYearPredictions <- function(x) processPredictions(x, pltitle, args$doplot)

    # compute test predictions by year
    years.test <- format(seq(rdates[3], rdates[4], by='year'), '%Y')
    years.conf <- format(seq(rdates[5], rdates[6], by='year'), '%Y')
    res.test <- planets.pred[Year.1 %in% years.test, processYearPredictions(.SD), by=Year.1]
    resMean <- function(x) round(mean(x), digits=2)
    res.test.mean <- res.test[, list(correlation=resMean(correlation), volatility=resMean(volatility), matches.d=resMean(matches.d))]
    # compute confirmation predictions by year
    res.conf <- planets.pred[Year.1 %in% years.conf, processYearPredictions(.SD), by=Year.1]
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

    fitness.best <- getFitnessBest(dbcon)
    if (fitness.total > fitness.best) {
      setFitnessBest(dbcon, fitness.total)
      new.fitness.best <- "best solution --- "
    }

    cat("\n---------------------------------------------------------------------------------\n")
    cat(sout)
    cat("\n")
    print(orbsmatrix)
    cat("\n")
    print(conjpolaritymatrix)
    cat("\n")
    print(aspectspolaritymatrix)
    cat("\n")
    print(aspectsenergymatrix)
    cat("\n")
    print(planetszodenergymatrix)
    cat("\n")
    print(planetsenergymatrix)
    cat("\n")
    print(panalogymatrix)
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

  relativeTrendFitness <- function(x, planetsorig, securityfile, planetsfile, tsdate, tedate, vsdate, vedate, csdate, cedate, fittype, dateformat) {
    # build the parameters based on GA indexes
    mapricetypes <- c('SMA', 'EMA', 'WMA', 'ZLEMA')
    predtypes <- c('absolute',  'relative')
    pricetypes <- c('averages',  'daily', 'priceaverage')
    analogytypes <- c(NA, 'SULONG', 'MOLONG', 'MELONG', 'VELONG', 'MALONG', 'CELONG', 'JULONG')
    pa.e = 11+length(planetsBaseCols)
    co.e = pa.e+length(deforbs)
    api.e = co.e+length(defpolarity)
    ae.e = api.e+length(defaspectsenergy)
    pe.e = ae.e+length(defplanetsenergy)
    pze.e = pe.e+length(defplanetszodenergy)

    args <-list(planetsorig=planetsorig,
                securityfile=securityfile,
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
                mapredsm=x[1],
                mapricefs=x[2],
                mapricesl=x[3]/2,
                mapricetype=mapricetypes[[x[4]]],
                degsplit=x[5],
                threshold=x[6]/100,
                energymode=x[7],
                energygrowthsp=x[8]/10,
                energyret=adjustEnergy(x[9]),
                alignmove=x[10],
                panalogy=analogytypes[x[11:(pa.e-1)]],
                cusorbs=x[pa.e:(co.e-1)],
                aspectspolarity=x[co.e:(api.e-1)],
                aspectsenergy=adjustEnergy(x[api.e:(ae.e-1)]),
                planetsenergy=adjustEnergy(x[ae.e:(pe.e-1)]),
                planetszodenergy=adjustEnergy(x[pe.e:(pze.e-1)]))

    return(relativeTrend(args))
  }

  adjustEnergy <- function(x) {
    ifelse(x >= 0, (10 + x) / 10, (-10 + x) / 10)
  }

  optimizeRelativeTrend <- function(securityfile, planetsfile, tsdate, tedate, vsdate, vedate, csdate, cedate, fittype, dateformat) {
    cat("---------------------------- Initialize optimization ----------------------------------\n\n")
    dsmin <- 1
    dsmax <- 5
    panalogymin <- rep(1, length(planetsBaseCols))
    panalogymax <- rep(8, length(planetsBaseCols))
    orbsmin <- rep(0, length(deforbs))
    orbsmax <- deforbs
    polaritymin <- rep(0, length(defpolarity))
    polaritymax <- rep(1, length(defpolarity))
    aspectenergymin <- rep(0, length(defaspectsenergy))
    aspectenergymax <- rep(20, length(defaspectsenergy))
    planetenergymin <- rep(0, length(defplanetsenergy))
    planetenergymax <- rep(20, length(defplanetsenergy))
    planetzodenergymin <- rep(-20, length(defplanetszodenergy))
    planetzodenergymax <- rep(20, length(defplanetszodenergy))

    minvals <- c( 2,  2, 3, 1, dsmin,  0, 1, 0, -20, -20, panalogymin, orbsmin, polaritymin, aspectenergymin,
                 planetenergymin, planetzodenergymin)
    maxvals <- c(10, 20, 6, 4, dsmax, 30, 2, 9,  20,  20, panalogymax, orbsmax, polaritymax, aspectenergymax,
                 planetenergymax, planetzodenergymax)

    panalogyCols <- planetsLonGCols[5:length(planetsLonGCols)]
    varnames <- c('mapredsm', 'mapricefs', 'mapricesl', 'mapricetype', 'degsplit', 'threshold', 'energymode',
                  'energygrowthsp', 'energyret', 'alignmove', panalogyCols, aspOrbsCols, planetsCombLonCols, aspectspolaritycols,
                  aspectsEnergyCols, planetsEnergyCols, planetsZodEnergyCols)

    # Clear the cache directory before start
    clearCache(recursive=T, prompt=T)
    # Create the cache directories structure
    getCachePath(dirs=c(securityfile))
    # Load the planets file
    planetsorig <- openPlanets(planetsfile, deforbs)
    # Redirect output to file
    if (exists('sinkfile', envir=parent.frame())) {
      sinkpathfile <- npath(paste("~/trading/predict/", sinkfile, ".txt", sep=''))
      sink(sinkpathfile, append=T)
    }

    ga("real-valued", fitness=relativeTrendFitness, names=varnames, parallel=TRUE,
       monitor=gaMonitor, maxiter=200, run=50, popSize=500, min=minvals, max=maxvals, pcrossover = 0.4, pmutation = 0.3,
       selection=gaint_rwSelection, mutation=gaint_raMutation, crossover=gaint_spCrossover, population=gaint_Population,
       planetsorig=planetsorig, securityfile=securityfile, planetsfile=planetsfile, tsdate=tsdate, tedate=tedate, vsdate=vsdate,
       vedate=vedate, csdate=csdate, cedate=cedate, fittype=fittype, dateformat=dateformat)

    if (exists('sinkfile', envir=parent.frame())) {
      sink()
    }
  }

  testSolution <- function(predfile, ...) {
    args <- list(...)
    if (!hasArg('dateformat')) stop("A dateformat is needed.")
    if (args$doplot) pdf(paste("~/chart_", predfile, ".pdf", sep=""), width = 11, height = 8, family='Helvetica', pointsize=12)
    planetsorig <- openPlanets(args$planetsfile, deforbs)
    args[['planetsorig']] <- planetsorig
    relativeTrend(args)
    if (args$doplot) dev.off()
  }

  testSolutionDebug <- function(planetsfile, securityfile, tsdate, tedate, vsdate, vedate, csdate, cedate, fittype, dateformat, predfile, x) {
    # build the parameters based on GA indexes
    mapricetypes <- c('SMA', 'EMA', 'WMA', 'ZLEMA')
    predtypes <- c('absolute',  'relative')
    pricetypes <- c('averages',  'daily', 'priceaverage')
    analogytypes <- c(NA, 'SULONG', 'MOLONG', 'MELONG', 'VELONG', 'MALONG')

    pa.e = 11+length(planetsBaseCols)
    co.e = pa.e+length(deforbs)
    api.e = co.e+length(defpolarity)
    ae.e = api.e+length(defaspectsenergy)
    pe.e = ae.e+length(defplanetsenergy)
    pze.e = pe.e+length(defplanetszodenergy)

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
                mapredsm=x[1],
                mapricefs=x[2],
                mapricesl=x[3]/2,
                mapricetype=mapricetypes[[x[4]]],
                degsplit=x[5],
                threshold=x[6]/100,
                energymode=x[7],
                energygrowthsp=x[8]/10,
                energyret=adjustEnergy(x[9]),
                alignmove=x[10],
                panalogy=analogytypes[x[11:(pa.e-1)]],
                cusorbs=x[pa.e:(co.e-1)],
                aspectspolarity=x[co.e:(api.e-1)],
                aspectsenergy=adjustEnergy(x[api.e:(ae.e-1)]),
                planetsenergy=adjustEnergy(x[ae.e:(pe.e-1)]),
                planetszodenergy=adjustEnergy(x[pe.e:(pze.e-1)]))

    if (!hasArg('dateformat')) stop("A dateformat is needed.")
    relativeTrend(args)
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
  closeSQLiteDB(dbcon)
}

# compile the function to byte code
testPlanetsSignificanceRelative <- cmpfun(cmpTestPlanetsSignificanceRelative)

securityPeaksValleys <- function(security, span=50, plotfile="peaks_valleys") {
  planets <- openPlanets("~/trading/dplanets/planets_4.tsv", orbs, aspects, 5, 10)
  planetsBaseCols <- c("SU", "ME", "VE", "MA", "JU", "SA", "NN")
  planetsLonCols <- paste(planetsBaseCols, 'LON', sep='')
  planetsLonGCols <- paste(planetsBaseCols, 'LONG', sep='')
  planetsCombLon <- combn(paste(c("SU", "MO", "ME", "VE", "MA", "JU", "SA", "UR", "NE", "PL", "SN", "NN"), 'LON', sep=''), 2, simplify=F)
  planetsCombLonCols <- as.character(lapply(planetsCombLon, function(x) paste(x[1], x[2], sep='')))
  cols <- c('Date', planetsLonGCols, planetsSpGCols, planetsCombLonCols)
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
  planets.pv.long <- data.table(melt(planets.pv, id.var=c('Date', 'type'), measure.var=planetsLonGCols))
  planets.pv.long$value <- factor(planets.pv.long$value, mixedsort(unique(planets.pv.long$value)))
  pl <- ggplot(aes(x=value, fill=type), data=planets.pv.long) + geom_bar(position='fill') + theme(axis.text.x = element_text(angle = 90, size = 9)) + xlab("Aggregated Planets LONG.")  + ggtitle(paste("Peaks VS Valleys Aggregated Planets (Percent)"))
  print(pl)
  pl <- ggplot(aes(x=value, fill=type), data=planets.pv.long) + geom_bar() + theme(axis.text.x = element_text(angle = 90, size = 9)) + xlab("Aggregated Planets LONG.")  + ggtitle(paste("Peaks VS Valleys Aggregated Planets (Count)"))
  print(pl)

  # Aggregated Aspects
  planets.pv.asp <- data.table(melt(planets.pv, id.var=c('Date', 'type'), measure.var=planetsCombLonCols))
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

  for (curcol in planetsCombLonCols) {
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
