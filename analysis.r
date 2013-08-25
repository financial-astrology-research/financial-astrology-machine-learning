library(xts)
library(timeDate)
library(quantmod)
library(ggplot2)
#library(msProcess)
library(plyr)
library(data.table)
library(fields)
library(reshape2)
library(randomForest)
library(rpart)
library(evtree)
library(GA)
library(gtools)
library(clusterSim)
library(splus2R)
`%ni%` <- Negate(`%in%`)
# no scientific notation
options(scipen=100)
options(width = 160)
options(error=recover)

planetsList <- list(c("SU", "SUR", "MO", "MOR", "ME", "MER", "VE", "VER", "MA", "MAR", "JU", "JUR", "SA", "SAR", "UR", "URR", "NE", "NER", "PL", "PLR"),
                    # combined fast planets
                    c("SU", "SUR", "MO", "MOR", "ME", "MER", "VE", "VER", "MA", "MAR"),
                    # combined slow planets
                    c("JU", "JUR", "SA", "SAR", "UR", "URR", "NE", "NER", "PL", "PLR"),
                    # trans jupiter
                    c("SA", "SAR", "UR", "URR", "NE", "NER", "PL", "PLR"),
                    # trans saturn
                    c("UR", "URR", "NE", "NER", "PL", "PLR"),
                    # all to radical
                    c("SUR", "MOR", "MER", "VER", "MAR", "JUR", "SAR", "URR", "NER", "PLR"),
                    # fast planets to radical
                    c("SUR", "MOR", "MER", "VER", "MAR"),
                    # slow planets to radical
                    c("JUR", "SAR", "URR", "NER", "PLR"),
                    # trans jupiter
                    c("SAR", "URR", "NER", "PLR"),
                    # trans saturn
                    c("URR", "NER", "PLR"),
                    # all planets to transit
                    c("SU", "MO", "ME", "VE", "MA", "JU", "SA", "UR", "NE", "PL"),
                    # fast planets
                    c("SU", "MO", "ME", "VE", "MA"),
                    # slow planets
                    c("JU", "SA", "UR", "NE", "PL"),
                    # trans jupiter
                    c("SA", "UR", "NE", "PL"),
                    # trans saturn
                    c("UR", "NE", "PL"),
                    # special combinations
                    c("SU", "SUR", "MO", "MOR", "ME", "MER", "VE", "VER"),
                    c("SU", "MO", "ME", "VE"),
                    c("SU", "SUR", "ME", "MER", "VE", "VER", "JU", "JUR"),
                    c("SU", "ME", "VE", "JU"),
                    c("SU", "SUR", "VE", "VER", "JU", "JUR"),
                    c("SU", "VE", "JU"),
                    c("MA", "MAR", "SA", "SAR", "UR", "URR", "NE", "NER", "PL", "PLR"),
                    c("MA", "SA", "UR", "NE", "PL"))

planetsCombList <- planetsList
#combn(planetsList[[1]], 5, simplify=FALSE))

aspectsList <- list(c('a0', 'a30', 'a45', 'a60', 'a72', 'a90', 'a120', 'a135', 'a144', 'a150', 'a180', 'a18', 'a40', 'a51', 'a80', 'a103', 'a108', 'a154', 'a160'),
                    c('a0', 'a45', 'a60', 'a90', 'a120', 'a150', 'a180'),
                    c('a0', 'a60', 'a90', 'a120', 'a180'),
                    c('a0', 'a30', 'a45', 'a60', 'a90', 'a120', 'a135', 'a150', 'a180'),
                    c('a30', 'a45', 'a72', 'a135', 'a144', 'a51', 'a103'),
                    c('a30', 'a45', 'a72', 'a135', 'a144', 'a18', 'a40', 'a51', 'a80', 'a103', 'a108', 'a154', 'a160'))

aspectsCombList <- aspectsList

# aspect types cols names
aspectTypesCols <- c('SUT', 'MOT', 'MET', 'VET', 'MAT', 'JUT', 'SAT', 'URT', 'NET', 'PLT')

# planets cols
planetsBaseCols <- c("SU", "MO", "ME", "VE", "MA", "JU", "SA", "UR", "NE", "PL", "SN", "NN")
planetsLonGCols <- c('SULONG', 'MOLONG', 'MELONG', 'VELONG', 'MALONG', 'JULONG', 'SALONG', 'URLONG', 'NELONG', 'PLLONG', 'NNLONG')

# Aspects and orbs
aspects = c(0, 30, 45, 60, 90, 120, 135, 150, 180)
orbs = list(SULON = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
            MOLON = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
            MELON = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
            VELON = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
            MALON = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
            JULON = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
            SALON = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
            URLON = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
            NELON = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
            PLLON = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
            NNLON = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0),
            SNLON = c(2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0))

zodDegrees <- seq(0, 360, by=2)

# planets columns names
planetsLonCols <- paste(planetsBaseCols, 'LON', sep='')
planetsLatCols <- paste(planetsBaseCols, 'LAT', sep='')
planetsSpCols <- paste(planetsBaseCols, 'SP', sep='')
planetsSpGCols <- paste(planetsSpCols, "G", sep="")
planetsCombLon <- combn(planetsLonCols, 2, simplify=F)
planetsCombLonCols <- as.character(lapply(planetsCombLon, function(x) paste(x[1], x[2], sep='')))
planetsGridLon <- expand.grid(planetsLonCols, planetsLonCols)
planetsGridLonCols <- as.character(apply(planetsGridLon, 1, function(x) paste(x[1], 'r', x[2], sep='')))
planetsGridAspCols <- as.character(apply(expand.grid(planetsGridLonCols, aspects), 1, function(x) paste(x[1], '_', as.numeric(x[2]), sep='')))
planetsGridZodCols <- as.character(apply(expand.grid(planetsLonCols, zodDegrees), 1, function(x) paste(x[1], '_', as.numeric(x[2]), sep='')))

# aspects types
aspectTypesList <- list(c('A', 'AE', 'SE', 'S'),
                        c('A', 'AE', 'SE'),
                        c('AE', 'SE'),
                        c('A', 'AE'),
                        c('SE', 'S'))

npath <- function(path) {
  normalizePath(path.expand(path))
}

planetsConjPolarity <- list(SULONMOLON = 1, SULONMELON = 1, SULONVELON = 1, SULONMALON = 0, SULONJULON = 1, SULONSALON = 0, SULONURLON = 1,
                            SULONNELON = 0, SULONPLLON = 0, SULONSNLON = 1, SULONNNLON = 1, MOLONMELON = 1, MOLONVELON = 1, MOLONMALON = 0,
                            MOLONJULON = 1, MOLONSALON = 0, MOLONURLON = 0, MOLONNELON = 1, MOLONPLLON = 0, MOLONSNLON = 1, MOLONNNLON = 1,
                            MELONVELON = 1, MELONMALON = 0, MELONJULON = 1, MELONSALON = 0, MELONURLON = 1, MELONNELON = 1, MELONPLLON = 0,
                            MELONSNLON = 1, MELONNNLON = 1, VELONMALON = 0, VELONJULON = 1, VELONSALON = 0, VELONURLON = 1, VELONNELON = 1,
                            VELONPLLON = 1, VELONSNLON = 1, VELONNNLON = 1, MALONJULON = 0, MALONSALON = 0, MALONURLON = 0, MALONNELON = 0,
                            MALONPLLON = 0, MALONSNLON = 0, MALONNNLON = 0, JULONSALON = 0, JULONURLON = 1, JULONNELON = 1, JULONPLLON = 0,
                            JULONSNLON = 1, JULONNNLON = 1, SALONURLON = 0, SALONNELON = 0, SALONPLLON = 0, SALONSNLON = 0, SALONNNLON = 0,
                            URLONNELON = 0, URLONPLLON = 0, URLONSNLON = 0, URLONNNLON = 0, NELONPLLON = 0, NELONSNLON = 0, NELONNNLON = 0,
                            PLLONSNLON = 0, PLLONNNLON = 0, SNLONNNLON = 0)

# a function that returns the position of n-th largest
maxn <- function(x, n) {
  order_x <- order(x, decreasing = TRUE)
  if ( length(order_x) < n ) {
    n = length(order_x)
  }
  x[order_x[n]]
}

shift <- function(x, shift_by) {
  stopifnot(is.numeric(shift_by))
  stopifnot(is.numeric(x))
  if (length(shift_by) > 1)
    return(sapply(shift_by, shift, x=x))
  out <- NULL
  abs_shift_by = abs(shift_by)
  if (shift_by > 0)
    out <- c(tail(x, -abs_shift_by), rep(NA, abs_shift_by))
  else if (shift_by < 0 )
    out <- c(rep(NA, abs_shift_by), head(x, -abs_shift_by))
  else
    out <- x
  out
}

dsPlanetsData <- function(ds) {
  # remove observations that don't have enough market reaction (mondays with less than 12 hours)
  fds <- subset(ds, !(dayOfWeek(ds$endEffect) == 'Mon' & as.numeric(format(ds$endEffect, '%H')) < 12))
  # convert date as same format of planets days
  #fds$Date <- as.Date(as.character(fds$endEffect), format="%Y-%m-%d")
  # build new data frame with the important columns
  fds <- subset(fds, select=c("PT", "Date", "val", "PRLON"))
  # calculate effect in categorical type
  fds$effect = cut(fds$val, c(-1, 0, 1), labels=c('down', 'up'), right=FALSE)
  # merge planets and filtered data set
  fds <- merge(fds, planets.eur, by = "Date")
  # set an id
  fds$id <- as.numeric(rownames(fds))
  fds
}

dsPlanetsLon <- function(fds) {
  # planet transit name
  pt <- paste(fds[1,c('PT')], 'LON', sep='')
  start_degree <- fds[1,c(pt)]
  points.x <- unique(unlist(list(seq(start_degree, 0, by=-45), seq(start_degree, 360, by=45), seq(start_degree, 0, by=-30), seq(start_degree, 360, by=30))))
  angles <- data.frame(cbind(points.x, rep(-5, length(points.x))))
  names(angles) <- c('x', 'y')
  # reshape longitudes
  fds.lon <- reshape(fds, varying = c("SULON", "MOLON", "MELON", "VELON", "MALON", "JULON", "SALON", "URLON", "NELON", "PLLON", "NNLON", "SNLON", "PRLON"), v.names = "lon", times = c("SULON", "MOLON", "MELON", "VELON", "MALON", "JULON", "SALON", "URLON", "NELON", "PLLON", "NNLON", "SNLON", "PRLON"),  direction = "long")
  p1 <- ggplot(fds.lon, aes(x=lon, y=id, colour=time, shape=time, facet= . ~ effect)) + facet_grid(. ~ effect) + geom_point() + scale_x_continuous(breaks=seq(from=0, to=359, by=5), limits=c(0,359)) + scale_shape_manual("",values=c(1,2,3,4,5,6,7,8,9,10,11,12,13)) + coord_polar(start = 1.57, direction=-1) + scale_y_continuous(breaks = seq(-5, 15, 1), limits = c(-5, 15)) + theme_update(axis.text.x = theme_text(angle = -30, size = 6))
  p1 <- p1 + geom_segment(data = angles, aes(x = x, y = -5, xend = x, yend = 15), size = 0.2, show_guide = FALSE)
  p1 <- p1 + opts(panel.grid.major = theme_line(colour="grey", size=0.2), title='Longitudes down VS up')
  print(p1)
}

dsPlanetsLat <- function(fds) {

  minor_planets <- c("MOLAT", "MELAT", "VELAT", "MALAT", "JULAT")
  major_planets <- c("SALAT", "URLAT", "NELAT", "PLLAT")
  #p1 + geom_line(aes(y = c(8)), color = "yellow")
  # latitude
  fds.lat.min <- reshape(fds, varying = minor_planets, v.names = "lat", times = minor_planets, direction = "long")
  fds.lat.maj <- reshape(fds, varying = major_planets, v.names = "lat", times = major_planets, direction = "long")
  #p2 <- qplot(x=fds.lat$lat, y=fds.lat$id, colour=fds.lat$time, shape=fds.lat$time, data=fds.lat, geom='jitter') + scale_x_continuous(breaks=seq(from=-10, to=10, by=1)) + scale_shape_manual("",values=c(1,2,3,4,5,6,7,8,9,10)) + facet_grid(. ~ effect)
  # density
  p1 <- ggplot(fds.lat.min, aes(x = lat, fill=time)) + scale_x_continuous(breaks=seq(from=-10, to=10, by=1)) + geom_histogram(binwidth = 0.5) + facet_grid(effect ~ time) + opts(axis.text.x = theme_text(angle = -90, size = 6), axis.text.y = theme_text(angle = 0, size = 6))
  p1 <- p1 + labs(title = "Latitude Lower Planets")
  p2 <- ggplot(fds.lat.maj, aes(x = lat, fill=time)) + scale_x_continuous(breaks=seq(from=-10, to=10, by=1)) + geom_histogram(binwidth = 0.5) + facet_grid(effect ~ time) + opts(axis.text.x = theme_text(angle = -90, size = 6), axis.text.y = theme_text(angle = 0, size = 6))
  p2 <- p2 + labs(title = "Latitude Major Planets")
  print(p1)
  print(p2)
}

dsPlanetsSpeed <- function(fds) {
  # speed
  slow_planets <- c("SASP", "URSP", "NESP", "PLSP")
  fast_planets <- c("MESP", "VESP", "MASP", "JUSP", "SASP")
  extra_planets <- c("MOSP")
  fds.sp.fast <- reshape(fds, varying = fast_planets, v.names = "speed", times = fast_planets, direction = "long")
  fds.sp.slow <- reshape(fds, varying = slow_planets, v.names = "speed", times = slow_planets, direction = "long")
  fds.sp.extra <- reshape(fds, varying = extra_planets, v.names = "speed", times = extra_planets, direction = "long")

  p1 <- ggplot(fds.sp.slow, aes(x = speed, fill=time)) + scale_x_continuous(breaks=seq(from=-0.3, to=0.3, by=0.01)) + geom_histogram(binwidth = 0.005) + facet_grid(effect ~ time)
  p1 <- p1 + opts(axis.text.x = theme_text(angle = -90, size = 6), axis.text.y = theme_text(angle = 0, size = 6), title = 'Speed Major Planets down VS up')
  p2 <- ggplot(fds.sp.fast, aes(x = speed, fill=time)) + scale_x_continuous(breaks=seq(from=-1, to=2, by=0.2)) + geom_histogram(binwidth = 0.1) + facet_grid(effect ~ time)
  p2 <- p2 + opts(axis.text.x = theme_text(angle = -90, size = 6), axis.text.y = theme_text(angle = 0, size = 6), title = 'Speed Minor Planets down VS up')
  p3 <- ggplot(fds.sp.extra, aes(x = speed, fill=time)) + scale_x_continuous(breaks=seq(from=9, to=16, by=0.5)) + geom_histogram(binwidth = 0.5) + facet_grid(effect ~ time)
  p3 <- p3 + opts(axis.text.x = theme_text(angle = -90, size = 6), axis.text.y = theme_text(angle = 0, size = 6), title = 'Speed Moon down VS up')
  print(p1)
  print(p2)
  print(p3)
}

dsPlanetsReport <- function(ds, data_name, kplanets, kaspects) {
  dsasp <- reshape(ds, varying = kplanets, v.names = "aspect", times = kplanets,  direction = "long")
  # select only the aspects to consider
  dsasp <- subset(dsasp, aspect %in% kaspects)
  dsasp$aspect <- factor(dsasp$aspect)
  ds_up <- data.table(subset(dsasp, Eff=='up'))
  ds_down <- data.table(subset(dsasp, Eff=='down'))
  tup <- as.data.frame(ds_up[, as.list(table(aspect)), by = c('idx')])
  tdown <- as.data.frame(ds_down[, as.list(table(aspect)), by = c('idx')])
  tablediff <- abs(tup[,-1]-tdown[,-1])
  qtile <- as.numeric(quantile(tablediff[,tablediff[,] > 0])[3])
  diffcols <- colnames(tablediff[,tablediff >= qtile])
  tup[!colnames(tup) %in% diffcols] <- 0
  tdown[!colnames(tup) %in% diffcols] <- 0
  tupdown <- t(rbind(tup, tdown))[-1,]
  colnames(tupdown) <- c('tup', 'tdown')
  tup <- apply(tupdown, 1, function(x) ifelse(x[1] > x[2], 1, 0))
  tdown <- apply(tupdown, 1, function(x) ifelse(x[2] > x[1], 1, 0))
  tupdown <- t(tupdown)
  tupdown2 <- rbind(tup, tdown)
  print(tupdown)
  print(tupdown2)
  #dis1 <- dist(rbind(tupdown2[1,], asptable), method="canberra")
  #dis2 <- dist(rbind(tupdown2[2,], asptable), method="canberra")
  #effect <- ifelse(dis1 < dis2, ifelse(dis1 != dis2, 'up', 'none'), 'down')
  #writeLines("UP aspects table")
  #writeLines("DOWN aspects table")

  fds <- dsPlanetsData(ds)
  pdf("~/plots.pdf", width = 11, height = 8, family='Helvetica', pointsize=12)
  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  report_title <- paste("Chart ", data_name, " : ", ds[1,c('PT')], " ", ds[1,c('AS')], " ", ds[1,c('PR')])
  text(5, 10, report_title)
  obs_dates <- paste(fds[,c('Date')], collapse='\n')
  text(5, 4, obs_dates)
  # chart the hourly currency price for each date
  chart_dates = paste(fds$Date, fds$Date, sep="/")
  for (j in 1:length(chart_dates)) {
    if (nrow(eurusd.hour.xts[chart_dates[j]]) == 24) {
      chartSeries(eurusd.hour.xts, subset=chart_dates[j], major.ticks='hours', show.grid=TRUE, theme='white.mono')
    }
  }
  p1 <- qplot(data=fds, x=val, binwidth=0.0025) + scale_x_continuous(breaks=seq(from=-0.05, to=0.05, by=0.0025))
  dsPlanetsLon(fds)
  dsPlanetsLat(fds)
  dsPlanetsSpeed(fds)
  dev.off()
}

dsAspectTable <- function(ds, keys) {
  dsasp <- reshape(ds, varying = keys, v.names = "aspect", times = keys,  direction = "long")
  sapply(table(dsasp$aspect), function(x) ifelse(x > 0, 1, x))
}

decToDeg <- function(num) {
  num <- abs(num)
  d <- as.integer(num)
  part <- (num-d)*60
  m <- as.integer(part)
  s <- as.integer((part-m)*60)
  c(d, m, s)
}

fivePastDays <- function(x) {
  unique(paste(as.Date(index(x)) - 5, as.Date(index(x), format="%Y%m%d"), sep = "/"))
}

hourlyValue <- function(dataset, bhours, ahours) {
  results = array()
  for (j in 1:nrow(dataset)) {
    tbl = sapply(eurusd.hour.xts[paste(dataset[j,]$endEffect-3600*bhours, dataset[j,]$endEffect+3600*ahours, sep='/')], sum)
    results[j] <- tbl['val']
  }
  results
}

planetAspectsEffect <- function(ptcode, pacode, pacodet) {
  #ds = subset(eurusd.merged, eval(conditions))
  conditions = expression(PT == ptcode & get(pacode) %in% c('a12', 'a9', 'a6', 'a180', 'a0') & get(pacodet) %in% c('A', 'AE'))
  ds = subset(eurusd.merged, eval(conditions))
  ds$val = round(hourlyValue(ds, 5, 1), digits=4)
  p = qplot(get(pacode), val, data=ds, geom='boxplot')
  p + geom_hline(yintercept=c(0.007, 0.005, 0, -0.005, -0.007))
}

# trans.xts[fivePastDays(trans.xts['20121022'])]
# index(trans.xts)

openCurrency  <- function(currency_file) {
  currency_file <- npath(currency_file)
  currency <- read.table(currency_file, header = T, sep=",")
  names(currency) <- c('Ticker', 'Date', 'Time', 'Open', 'Low', 'High', 'Close')
  currency <- currency[,-1]
  #currency$Mid <- (currency$High + currency$Low + currency$Close) / 3
  currency$Mid <- (currency$High + currency$Low + currency$Close + currency$Open) / 4
  #currency$val <- currency$Mid - currency$Open
  currency$val <- c(0, round(diff(currency$Mid, lag=1), digits=4))
  currency$Eff = cut(currency$val, c(-1, 0, 1), labels=c('down', 'up'), right=FALSE)
  currency$Date <- as.Date(as.character(currency$Date), format="%Y%m%d")
  currency
}

openSecurity <- function(security_file, mapricetype, maprice, dateformat="%Y.%m.%d", pricetype="daily", pricemadir=1) {
  mapricefunc <- get(get('mapricetype'))
  security_file <- npath(security_file)
  security <- fread(security_file)
  security[, Date := as.Date(as.character(Date), format=dateformat)]
  security[, Year := as.character(format(Date, "%Y"))]
  setkey(security, 'Date')
  security[, Mid := (High + Low + Close + Open) / 4]

  if (pricemadir == 1 | pricemadir == 2) {
    security[, MidMAF := mapricefunc(Mid, n=maprice)]
    security[, MidMAS := mapricefunc(Mid, n=maprice*2)]
  }
  else if (pricemadir == 3 | pricemadir == 4) {
    security[, MidMAF := rev(mapricefunc(rev(Mid), n=maprice))]
    security[, MidMAS := rev(mapricefunc(rev(Mid), n=maprice*2))]
  }
  else {
    stop("No valid pricemadir was provided.")
  }

  if (pricetype == 'averages') {
    security[, val := MidMAF-MidMAS]
  }
  else if (pricetype == 'daily') {
    security[, val := c(NA, diff(Mid, lag=1, differences=1))]
  }
  else if (pricetype == 'priceaverage') {
    security[, val := Mid-MidMAF]
  }
  else {
    stop("No valid price type was provided.")
  }

  if (all(security$val == 0)) {
    stop("Undetermined security price direction")
  }

  security <- security[!is.na(val)]
  #security$val <- security$Close - security$Mid
  if (pricemadir == 1 | pricemadir == 3) {
    security[, Eff := cut(val, c(-1000, 0, 1000), labels=c('down', 'up'), right=FALSE)]
  }
  else if (pricemadir == 2 | pricemadir == 4) {
    security[, Eff := cut(val, c(-1000, 0, 1000), labels=c('up', 'down'), right=FALSE)]
  }

  return(security)
}

openCurrencyXts <- function(currency_file) {
  currency_file <- npath(currency_file)
  currency <- openTrans(currency_file)
  xts(currency[,-1:-2], order.by=currency[,1])
}

removeAspectsOutType <- function(X, asptype) {
  Y <- X[1:20]
  v <- X[21:30] %in% asptype
  # double the validation vector intercalating elements
  v <- c(matrix(c(v,v), 2, byrow=T))
  Y <- ifelse(v, Y, NA);
  Y
}

# Open a Transits table and merge with a currency table
openTrans <- function(trans_file, effcorrection=0, hourcorrection=7) {
  trans_file <- npath(trans_file)
  # transits
  trans <- fread(trans_file, header = T, sep="\t", na.strings = "", verbose = FALSE)
  trans[, Date := as.Date(Date, format="%Y-%m-%d")]
  trans[, S2 := S+SUS+MOS+MES+VES+JUS+SAS+URS+NES+PLS]
  #timeEffect <- strptime(trans$endEffect, format="%Y-%m-%d %H:%M:%S", tz="UTC")
  # timeDate automatically correct summer time based on the rules of the financial center
  timeEffect <- timeDate(paste(trans$Date, trans$Hour), format = "%Y-%m-%d %H:%M:%S", zone = "UTC", FinCenter = "Europe/Bucharest")
  trans[, endEffect := as.character(timeEffect)]
  trans[, WD := format(timeEffect, "%w")]
  trans[, H := as.numeric(format(timeEffect, "%H"))]
  # remove the slow planets transits and other points that complicate prediction
  trans <- trans[PT %ni% c('MO', 'JU', 'SA', 'UR', 'NE', 'PL') & PR %ni% c('Asc', 'MC')]

  if (effcorrection) {
    # substract one day to the aspects tha are early in the day
    # the effect should be produced the previous day
    trans[, Date := as.double(strptime(Date, "%Y-%m-%d"))]
    trans[, Date := as.Date(as.POSIXlt(ifelse(H <= hourcorrection, Date-86400, Date), origin="1970-01-01"), format="%Y-%m-%d")]
  }

  # replace NA aspects by none due randomForest need a value
  trans[, planetsList[[1]] := lapply(.SD, function(x) ifelse(is.na(x), 'none', x)), .SDcols=planetsList[[1]]]
  # set as factors
  trans[, planetsList[[1]] := lapply(.SD, as.factor), .SDcols=planetsList[[1]]]

  # Convert the riseset times to correct timezone for Cyprus
  #tzcorrect <- format(as.POSIXlt(paste(trans$Date, trans$ASC1), format = "%Y-%m-%d %H:%M") + 3600 * 2.5, "%H:%M")
  #trans$ASC1 <- tzcorrect
  trans[, PC := trans$PT]
  trans[, PC := factor(trans$PC, levels = c('SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'SA', 'UR', 'NE', 'PL'), labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))]
  # give each transit type an index
  trans[, idx := paste(PT, AS, PR, SI, sep='')]
  trans[, idx2 := paste(Date, PT, AS, PR, SI, sep='')]
  return(as.data.frame(trans))
}

openTransXts <- function(trans_file) {
  trans <- openTrans(trans_file)
  # remove the time column
  trans_xts <- xts(trans[,-2], order.by=trans$Date)
  trans_xts
}

# build merged table for currency and transits
mergeTrans <- function(ds_trans, currency_table) {
  trans.price = merge(currency_table, ds_trans, by = "Date")
  trans.price[, !(names(trans.price) %in% c("Time", "Hour"))]
}

predictTrend <- function(search_date) {
  date_date <- as.Date(as.character(search_date), format="%Y-%m-%d")
  chart_dates = paste(date_date-1, date_date, sep="/")
  ds <- trans.eur[search_date]
  print(ds)
  hisds <- subset(trans.eurusd.eur, idx %in% ds$idx)
  print(prop.table(table(hisds$Eff)))
  chartSeries(eurusd.hour.xts, subset=chart_dates, major.ticks='hours', show.grid=TRUE, theme='white.mono')
}

predColNames = c('Date', 'idx', 'idx2', 'endEffect', 'S2', 'PC', 'SUR', 'SURD', 'MOR', 'MORD', 'MER', 'MERD', 'VER', 'VERD', 'MAR', 'MARD', 'JUR', 'JURD', 'SAR', 'SARD', 'URR', 'URRD', 'NER', 'NERD', 'PLR', 'PLRD', 'udis', 'ddis', 'corEff', 'ucor', 'dcor')

predictTransTable <- function(trans, trans.hist, cor_method, kplanets, kaspects, binarize=1, rmzeroaspects=1, qinmode='q3', maxasp=2) {
  # convert to data table
  trans.hist <- data.table(trans.hist)
  trans <- data.table(trans)
  setkey(trans, 'Date', 'idx')
  # generate the count effect table
  #predict_table <- predictAspectsTable(trans.hist)
  selected <- trans[, idx[which(S2 >= maxn(S2, maxasp))], by=c('Date')]
  setnames(selected, 'V1', 'idx')
  # get the aspects correlation table
  aspect_dates_cor <- historyAspectsCorrelation(trans, trans.hist, cor_method, kplanets, kaspects, binarize, rmzeroaspects, qinmode)
  # merge
  #aspect_dates_predict <- merge(selected, predict_table, by='idx')
  predict.table <- merge(selected, trans, by=c('Date', 'idx'))
  predict.table <- merge(predict.table, aspect_dates_cor, by=c('Date', 'idx'))
  # sort and return
  predict.table <- arrange(predict.table, desc(Date))
  predict.table <- as.data.frame(predict.table)
  predict.table[predColNames]
}

predictTransTableWrite <- function(trans, trans.hist, cor_method, kplanets, kaspects, binarize, rmzeroaspects, qinmode, filename) {
  aspect_dates_predict <- predictTransTable(trans, trans.hist, cor_method, kplanets, kaspects, binarize, rmzeroaspects, qinmode, 2)
  write.csv(aspect_dates_predict[predColNames], file=npath(paste("~/trading/predict/", file_name, sep='')), eol="\r\n", quote=FALSE, row.names=FALSE)
}

predictSingleTransTable <- function(ds, ds_hist, file_name) {
  ds <- as.data.frame(ds)
  ds_count <- data.frame(tapply(1:NROW(ds), ds$Date, function(x) length(unique(x))))
  names(ds_count) <- c('Count')
  filter_dates <- rownames(subset(ds_count, Count==1))
  # access a date with ndist['2014-12-29',]
  single_aspect_dates <- subset(ds, select=c('Date', 'idx', 'endEffect', 'WD', 'S2', 'SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'SA', 'UR', 'NE', 'PL'), Date %in% filter_dates)
  single_aspect_dates <- as.data.frame(single_aspect_dates, row.names=1:nrow(single_aspect_dates))
  aspects_effect <- predictAspectsTable(ds_hist)
  #aspect_val_mean <- tapply(ds$val, ds$idx, mean)
  aspect_dates_predict <- merge(single_aspect_dates, aspects_effect, by='idx')
  # sort by date
  aspect_dates_predict <- arrange(aspect_dates_predict, desc(Date))
  # round the val mean
  write.csv(aspect_dates_predict[c(2, 12:14, 3:11, 1)], file=npath(paste("~/trading/predict/", file_name, sep='')), eol="\r\n", quote=FALSE, row.names=FALSE)
}

predictAspectsTable <- function(ds_hist) {
  ds_hist <- ds_hist[,c('idx', 'Eff'), with=FALSE]
  setkey(ds_hist, 'idx')
  #predict_table <- ddply(ds_hist, .(idx), summarize, down=table(Eff)[['down']], up=table(Eff)[['up']], count=length(Eff))
  predict_table <- ds_hist[,as.list(table(Eff)), by=c('idx')]
  predict_table <- predict_table[,count:=down+up]
  predict_table
}

filterZeroAspects <- function(X) {
  init1 <- 1
  end1 <- length(X)/3
  init2 <- end1+1
  end2 <- end1*2
  init3 <- end2+1
  end3 <- end1*3
  X1 <- X[init1:end1]
  X2 <- X[init2:end2]
  X3 <- X[init3:end3]
  X1 <- ifelse(X2 == 0 & X3 == 0, 0, X1)
}

completeAspectList <- function(X, kaspects) {
  missidx <- kaspects[kaspects %ni% names(X)]
  X[missidx] <- 0
  X
}

historyAspectsCorrelation <- function(trans, trans.hist, cor_method, kplanets, kaspects, binarize=1, rmzeroaspects=1, qinmode='q3') {
  if (qinmode %ni% c('q1', 'q2', 'q3', 'q4', 'q5')) stop("Provide a valid value for qinmode")
  #remove no needed cols to save memory
  trans <- trans[,c('Date', 'idx', kplanets), with=FALSE]
  trans.hist <- trans.hist[,c('Date', 'idx', 'Eff', kplanets), with=FALSE]
  # melt return a data.frame so we need to convert to data.table
  dsasp <- data.table(melt(trans.hist, id.var=c('Date', 'idx', 'Eff'), measure.var=kplanets))
  setnames(dsasp, c('Date', 'idx', 'Eff', 'planet', 'aspect'))
  setkey(dsasp, 'Date', 'idx', 'Eff', 'aspect')
  # select only the aspects to consider
  dsasp <- dsasp[aspect %in% kaspects]
  # adjust factor levels
  dsasp$aspect <- factor(dsasp$aspect)
  # separate the up/down tables
  dsup <- dsasp[Eff=='up']
  dsdown <- dsasp[Eff=='down']
  # build up/down aspect tables
  dsup <- dsup[, as.list(completeAspectList((table(aspect)), kaspects=kaspects)), by = c('idx')]
  ucolnames <- paste(names(dsup)[2:length(names(dsup))], 'u', sep='.')
  setnames(dsup, c('idx', ucolnames))
  dsdown <- dsdown[, as.list(completeAspectList((table(aspect)), kaspects=kaspects)), by = c('idx')]
  dcolnames <- paste(names(dsdown)[2:length(names(dsdown))], 'd', sep='.')
  # before set dcolnames build the substract colnames
  scolnames <- paste(names(dsdown)[2:length(names(dsdown))], 's', sep='.')
  setnames(dsdown, c('idx', dcolnames))
  # merge_recurse(apply(trans[,kplanets], 2, function(x) count(x)))
  # by(trans.eur[,kplanets], trans.eur$idx, function(x) { table(apply(x, 1, function(x) x)) })
  # reshape the transits data and generate aspect table
  dscurrent <- data.table(melt(trans, id.var=c('Date','idx'), measure.var=kplanets))
  setnames(dscurrent, c('Date', 'idx', 'planet', 'aspect'))
  # select only the aspects to consider
  dscurrent <- dscurrent[aspect %in% kaspects]
  # adjust factor levels
  dscurrent$aspect <- factor(dscurrent$aspect)
  setkey(dscurrent, 'Date', 'idx', 'aspect')
  dscurrent <- dscurrent[, as.list(completeAspectList((table(aspect)), kaspects=kaspects)), by = c('Date','idx')]
  ccolnames <- paste(names(dscurrent)[3:length(names(dscurrent))], 'c', sep='.')
  setnames(dscurrent, c('Date', 'idx', ccolnames))

  # merge
  predtable <- merge(dscurrent, dsup, by='idx')
  predtable <- merge(predtable, dsdown, by='idx')
  #predtable <- Reduce('merge', list(dscurrent, dsup, dsdown))
  # number of aspects
  naspects <- length(kaspects)
  # the first two cols are date and indx
  initcol <- 3
  # selected cols
  cols1 <- seq(initcol, naspects+initcol-1)
  cols2 <- cols1 + naspects
  cols3 <- cols2 + naspects

  predtable[,scolnames] = abs(predtable[,cols2,with=FALSE]-predtable[,cols3,with=FALSE])
  # set zero cols to NA for each column via data.table
  for (colname in scolnames) {
    expr1 <- parse(text = paste(colname, "== 0"))
    expr2 <- parse(text = paste(colname, ":= NA"))
    predtable[eval(expr1), eval(expr2)]
  }

  # build expression to generate quantile based on the diff cols
  expr1 <- parse(text = paste('as.list(quantile(c(',paste(scolnames, collapse=","),'), na.rm=TRUE))',sep=''))
  qtiles <- predtable[,eval(expr1),by=c('Date','idx')]
  setnames(qtiles, c('Date', 'idx', 'q1', 'q2', 'q3', 'q4', 'q5'))
  predtable  <- merge(predtable, qtiles, by=c('Date','idx'))

  # set all the columns that are less that qintile to 0
  for (i in 1:naspects) {
    expr1 <- parse(text = paste(scolnames[[i]], "<", qinmode, '| is.na(', scolnames[[i]], ')'))
    expr2 <- parse(text = paste(ucolnames[[i]], ":= 0"))
    expr3 <- parse(text = paste(dcolnames[[i]], ":= 0"))
    predtable[eval(expr1), eval(expr2)]
    predtable[eval(expr1), eval(expr3)]
  }

  # leave the major value between up/downs
  for (i in 1:naspects) {
    # up greater that down, reset down
    expr1 <- parse(text = paste(ucolnames[[i]], ">", dcolnames[[i]]))
    expr2 <- parse(text = paste(dcolnames[[i]], ":= 0"))
    predtable[eval(expr1), eval(expr2)]
    # down greater than up, reset up
    expr1 <- parse(text = paste(dcolnames[[i]], ">", ucolnames[[i]]))
    expr2 <- parse(text = paste(ucolnames[[i]], ":= 0"))
    predtable[eval(expr1), eval(expr2)]
  }

  # deprecated by above approach
  #predtable[,c(cols2,cols3)] <- t(apply(predtable[,c(cols2,cols3)], 1, filterLessSignificant, qinmode=qinmode))

  # convert to data frame
  predtable <- as.data.frame(predtable)

  if (rmzeroaspects) {
    predtable[,cols1] <- t(apply(predtable[,c(cols1,cols2,cols3)], 1, filterZeroAspects))
  }

  if (binarize) {
    # set active aspects to 1 so correlation fits better
    predtable[,c(cols1,cols2,cols3)] <- apply(predtable[,c(cols1,cols2,cols3)], 2, function(x) ifelse(x > 0, 1, x))
  }

  #predtable[,cols2] <- t(apply(predtable[,cols2], 1, function(x) ifelse(x >= x[which.max(x)]/3, 1, 0)))
  #predtable[,cols3] <- t(apply(predtable[,cols3], 1, function(x) ifelse(x >= x[which.max(x)]/3, 1, 0)))
  predtable$udis <- apply(predtable, 1, function(x) dist(rbind(x[cols1], x[cols2]), method=cor_method))
  predtable$ddis <- apply(predtable, 1, function(x) dist(rbind(x[cols1], x[cols3]), method=cor_method))
  # TODO: if all(c(cols1, cols2) == 0) then udis/ddis/corEff = NA we can't predict it
  predtable$corEff <- apply(predtable[,c('udis','ddis')], 1, function(x) ifelse(x[1] != x[2], ifelse(x[1] < x[2], 'up', 'down'), NA))
  predtable$ucor <- round(apply(predtable[,c(cols1, cols2, cols3)], 1, function(x) cor(x[cols1-2], x[cols2-2], method='pearson')), digits=2)
  predtable$dcor <- round(apply(predtable[,c(cols1, cols2, cols3)], 1, function(x) cor(x[cols1-2], x[cols3-2], method='pearson')), digits=2)

  data.table(predtable)
}

openPlanets <- function(planets.file, cusorbs, cusaspects, lonby=1, spby=60) {
  planets.file <- npath(planets.file)
  planets <- fread(planets.file, sep="\t", na.strings="", verbose = F)
  setkey(planets, 'Date')
  planets[, Date := as.Date(planets$Date, format="%Y-%m-%d")]
  planets[, DateMT4 := as.character(format(Date, "%Y.%m.%d"))]
  planets[, Year := as.character(format(Date, "%Y"))]

  if (hasArg('cusorbs')) {
    orbs <- cusorbs
    if (length(cusorbs) != length(orbs)) {
      stop("There are missing custom orbs")
    }
  }

  if (hasArg('cusaspects')) {
    aspects <- cusaspects
  }

  # calculate longitudinal differences
  for (i in 1:length(planetsCombLon)) {
    combname <- paste(planetsCombLon[[i]][1], planetsCombLon[[i]][2], sep='')
    comborbs <- orbs[[planetsCombLon[[i]][1]]] + orbs[[planetsCombLon[[i]][2]]]
    planets[, c(combname) := paste('a', diffDeg(get(planetsCombLon[[i]][1]), get(planetsCombLon[[i]][2]), comborbs, aspects), sep='')]
  }

  for (loncol in planetsLonCols) {
    planets[, c(paste(loncol, 'G', sep='')) := cut(get(loncol), seq(0, 360, by=lonby))]
  }

  for (spcol in planetsSpCols) {
    planets[, c(paste(spcol, 'G', sep='')) := cut(get(spcol), breaks=spby)]
  }

  return(planets)
}

planetsVarsSignificance <- function(planets, currency, threshold) {
  planets  <- merge(planets, currency, by='Date')
  spcols <- paste(planetsSpCols, 'G', sep='')
  loncols <- paste(planetsLonCols, 'G', sep='')
  cols <- c(loncols, spcols, planetsCombLonCols)
  significance <- data.table()

  for (curcol in cols) {
    idx <- length(significance)+1
    t1 <- planets[, cbind(as.list(prop.table(table(Eff))), as.list(table(Eff))), by=curcol]
    t1[, c('pdiff', 'variable') := list(V2-V1, curcol)]
    setnames(t1, curcol, 'key')
    t1[, key := as.character(key)]
    t1 <- t1[pdiff >= threshold | pdiff <= -threshold]
    significance <- rbind(significance, t1)
  }

  significance <- subset(significance, key != 'anon')
  return(significance)
}

planetsDaySignificance <- function(planets.day, significance, planetsAnalogy, answer=T, verbose=F, iprev=0, inext=0,
                                   sigtype='count', uselon=1, usesp=1, useasp=1, energymode=0, energyweight=0) {
  significance.day <- data.frame()
  cols <- c()

  if (uselon == 1) {
    cols <- c(cols, planetsLonGCols)
  }

  if (usesp == 1) {
    cols <- c(cols, planetsSpGCols)
  }

  if (useasp == 1) {
    cols <- c(cols, planetsCombLonCols)
  }

  #init <- as.numeric( sub("\\((.+),.*", "\\1", planets.day[curcol]))
  #keyranges <- apply(matrix(seq(init, init+8), ncol=2, byrow=T), 1, function(x) return(paste('(', x[1], ',', x[2], ']', sep='')))
  for (curcol in cols) {
    res <- data.frame()
    curidx <- which(keyranges==planets.day[[curcol]])
    if (length(curidx) > 0) {
      indexes <- c(curidx)
      if (iprev != 0) indexes <- c(indexes, seq(curidx, curidx-iprev))
      if (inext != 0) indexes <- c(indexes, seq(curidx, curidx+inext))
      indexes <- ifelse(indexes < 0, length(keyranges)+indexes+1, ifelse(indexes > length(keyranges), indexes-length(keyranges)+1, indexes))
      indexes <- unique(indexes)
      degroups <- keyranges[indexes]
      # search by analogy & direct variable
      if (!is.null(planetsAnalogy[[curcol]])) {
        res <- significance[key %in% degroups & variable %in% planetsAnalogy[[curcol]]]
      }
    }
    else {
      res <- significance[key == planets.day[[curcol]] & variable == curcol]
    }

    if (nrow(res) > 0) {
      res <- cbind(res, origin=curcol)
      significance.day <- rbind(significance.day, res)
    }
  }

  significance.day <- data.table(significance.day)
  patterns <- paste(strtrim(unique(significance.day$origin), 5), collapse='|', sep='')
  activecols <- planetsCombLonCols[grep(patterns, planetsCombLonCols, perl=T)]
  planets.day.asp <- planets.day[planets.day != "anon" & names(planets.day) %in% activecols]
  energy <- list(SULONG = 1, MOLONG = 1, MELONG = 1, VELONG = 1, MALONG = 1, JULONG = 1, SALONG = 1,
                 URLONG = 1, NELONG = 1, PLLONG = 1, NNLONG = 1, SNLONG = 1)
  energy.pos <- list(SULONG = 1, MOLONG = 1, MELONG = 1, VELONG = 1, MALONG = 1, JULONG = 1, SALONG = 1,
                     URLONG = 1, NELONG = 1, PLLONG = 1, NNLONG = 1, SNLONG = 1)
  energy.neg <- list(SULONG = 1, MOLONG = 1, MELONG = 1, VELONG = 1, MALONG = 1, JULONG = 1, SALONG = 1,
                     URLONG = 1, NELONG = 1, PLLONG = 1, NNLONG = 1, SNLONG = 1)

  for (curcol in names(planets.day.asp)) {
    loncol1 <- paste(substr(curcol, 1, 5), 'G', sep='')
    loncol2 <- paste(substr(curcol, 6, 10), 'G', sep='')
    aspname <- planets.day.asp[[curcol]]

    if (energyweight == 0) {
      aspweights <- list(a0 = 1, a30 = 1, a45 = 1, a60 = 1, a90 = 1, a120 = 1, a135 = 1, a150 = 1, a180 = 1)
    }
    else if (energyweight == 1) {
      aspweights <- list(a0 = 4, a30 = 1, a45 = 2, a60 = 2, a90 = 3, a120 = 2, a135 = 1, a150 = 2, a180 = 4)
    }
    else if (energyweight == 2) {
      aspweights <- list(a0 = 4, a30 = 1, a45 = 1, a60 = 2, a90 = 3, a120 = 3, a135 = 1, a150 = 2, a180 = 4)
    }
    else if (energyweight == 3) {
      aspweights <- list(a0 = 4, a30 = 1, a45 = 1, a60 = 2, a90 = 2, a120 = 3, a135 = 1, a150 = 2, a180 = 4)
    }
    else if (energyweight == 4) {
      aspweights <- list(a0 = 5, a30 = 1, a45 = 1, a60 = 3, a90 = 3, a120 = 4, a135 = 1, a150 = 1, a180 = 5)
    }
    else if (energyweight == 5) {
      aspweights <- list(a0 = 6, a30 = 1, a45 = 1, a60 = 3, a90 = 3, a120 = 4, a135 = 1, a150 = 1, a180 = 5)
    }
    else if (energyweight == 6) {
      aspweights <- list(a0 = 7, a30 = 1, a45 = 1, a60 = 3, a90 = 3, a120 = 5, a135 = 1, a150 = 1, a180 = 7)
    }
    else if (energyweight == 7) {
      # David Williams
      aspweights <- list(a0 = 10, a30 = 2, a45 = 2, a60 = 4, a90 = 4, a120 = 6, a135 = 2, a150 = 2, a180 = 8)
    }
    else if (energyweight == 8) {
      aspweights <- list(a0 = 10, a30 = 2, a45 = 4, a60 = 5, a90 = 5, a120 = 5, a135 = 4, a150 = 2, a180 = 8)
    }
    else if (energyweight == 9) {
      aspweights <- list(a0 = 10, a30 = 2, a45 = 2, a60 = 4, a90 = 4, a120 = 6, a135 = 2, a150 = 2, a180 = 10)
    }
    else if (energyweight == 10) {
      aspweights <- list(a0 = 10, a30 = 2, a45 = 2, a60 = 4, a90 = 7, a120 = 5, a135 = 2, a150 = 2, a180 = 8)
    }
    else if (energyweight == 11) {
      # Demetrio Santos
      aspweights <- list(a0 = 10, a30 = 2, a45 = 2, a60 = 3, a90 = 6, a120 = 4, a135 = 2, a150 = 2, a180 = 10)
    }
    else {
      stop("No valid energy mode was provided.")
    }

    # determine aspect weight
    aspweight <- aspweights[[aspname]]

    if (aspname %in% c('a30', 'a60', 'a120')) {
      energy.pos[[loncol1]] <- energy.pos[[loncol1]] + aspweight
      energy.pos[[loncol2]] <- energy.pos[[loncol2]] + aspweight
    }
    else if (aspname %in% c('a45', 'a90', 'a135', 'a150', 'a180')) {
      energy.neg[[loncol1]] <- energy.neg[[loncol1]] + aspweight
      energy.neg[[loncol2]] <- energy.neg[[loncol2]] + aspweight
    }
    else if (aspname == 'a0') {
      if (planetsConjPolarity[[curcol]] == 1) {
        energy.pos[[loncol1]] <- energy.pos[[loncol1]] + aspweight
        energy.pos[[loncol2]] <- energy.pos[[loncol2]] + aspweight
      }
      else if (planetsConjPolarity[[curcol]] == 0) {
        energy.neg[[loncol1]] <- energy.neg[[loncol1]] + aspweight
        energy.neg[[loncol2]] <- energy.neg[[loncol2]] + aspweight
      }
    }

    energy[[loncol1]] <- energy[[loncol1]] + aspweight
    energy[[loncol2]] <- energy[[loncol2]] + aspweight
  }

  if (nrow(significance.day) > 0) {
    if ( energymode > 0 ) {
      for (curcol in names(energy)) {
        if (energymode == 1) {
          # add more energy to the planets with more aspects
          significance.day[origin == curcol, c('V3', 'V4') := list(V3 * energy[[curcol]], V4 * energy[[curcol]])]
        }
        else if ( energymode == 2 ) {
          # add more energy to the lower part based on bad aspects and to the upper part with good aspects
          significance.day[origin == curcol & V3 > V4, V3 := V3 * energy.pos[[curcol]]]
          significance.day[origin == curcol & V4 > V3, V4 := V4 * energy.pos[[curcol]]]
          significance.day[origin == curcol & V3 < V4, V3 := V3 * energy.neg[[curcol]]]
          significance.day[origin == curcol & V4 < V3, V4 := V4 * energy.neg[[curcol]]]
        }
        else if ( energymode == 3 ) {
          # add more energy to the lower part based on good aspects and to the upper part with bad aspects
          significance.day[origin == curcol & V3 > V4, V3 := V3 * energy.neg[[curcol]]]
          significance.day[origin == curcol & V4 > V3, V4 := V4 * energy.neg[[curcol]]]
          significance.day[origin == curcol & V3 < V4, V3 := V3 * energy.pos[[curcol]]]
          significance.day[origin == curcol & V4 < V3, V4 := V4 * energy.pos[[curcol]]]
        }
        else if ( energymode == 4 ) {
          # add more energy only to lower part based on bad aspects
          significance.day[origin == curcol & V3 < V4, V3 := V3 * energy.neg[[curcol]]]
          significance.day[origin == curcol & V4 < V3, V4 := V4 * energy.neg[[curcol]]]
        }
        else if ( energymode == 5 ) {
          # add more energy only to lower part based on good aspects
          significance.day[origin == curcol & V3 < V4, V3 := V3 * energy.pos[[curcol]]]
          significance.day[origin == curcol & V4 < V3, V4 := V4 * energy.pos[[curcol]]]
        }
        else if ( energymode == 6 ) {
          # add more energy only to upper part based on bad aspects
          significance.day[origin == curcol & V3 > V4, V3 := V3 * energy.neg[[curcol]]]
          significance.day[origin == curcol & V4 > V3, V4 := V4 * energy.neg[[curcol]]]
        }
        else if ( energymode == 7 ) {
          # add more energy only to upper part based on good aspects
          significance.day[origin == curcol & V3 > V4, V3 := V3 * energy.pos[[curcol]]]
          significance.day[origin == curcol & V4 > V3, V4 := V4 * energy.pos[[curcol]]]
        }
        else if ( energymode == 8 ) {
          # add more energy to lower part based in all aspects
          significance.day[origin == curcol & V3 < V4, V3 := V3 * energy[[curcol]]]
          significance.day[origin == curcol & V4 < V3, V4 := V4 * energy[[curcol]]]
        }
        else if ( energymode == 9 ) {
          # add more energy to upper part based in all aspects
          significance.day[origin == curcol & V3 > V4, V3 := V3 * energy[[curcol]]]
          significance.day[origin == curcol & V4 > V3, V4 := V4 * energy[[curcol]]]
        }
        else {
          stop("No valid energy mode was provided.")
        }
      }
    }

    # recalculate the percent energy to take in account the aspect energy changes
    significance.day[, c('V1', 'V2') := list((V3/(V3+V4))*100, (V4/(V3+V4))*100)]
    significance.day[, pdiff := V2-V1]

    if (sigtype == 'count') {
      trend <- as.integer(significance.day[, sum(V4)-sum(V3)])
    }
    else if (sigtype == 'percent') {
      trend <- as.integer(significance.day[, sum(pdiff)])
    }
    else {
      stop("No valid trend type was provided.")
    }

    if (verbose) {
      # TODO: print the energy lists as tables
      cat("=============================================================\n")
      cat("Date:", planets.day[['Date']], "\n")
      print(significance.day)
      cat("Aspect Table\n")
      print(planets.day.asp)
      cat("Total Aspects:\n")
      print(t(energy))
      cat("Positive Aspects:\n")
      print(t(energy.pos))
      cat("Negative Aspects:\n")
      print(t(energy.neg))
      cat("\n")
      print(planets.day[planetsSpCols])
      cat("\n")
      cat("###  =", trend, "\n")
    }

    # the result could return as answer (factor) or numeric (kind of probability)
    if (answer) {
      if (trend > 0) {
        return('up')
      }
      else if (trend < 0) {
        return('down')
      }
      else {
        return('none')
      }
    }
    else {
      return(trend)
    }
  }

  return(0)
}

openPlanetsZod <- function(planets.file, currency, sdate, edate, threshold, planetsLonCols, planetsGridZodCols, aspects, cusorbs) {
  planets.file <- npath(planets.file)
  planets <- fread(planets.file, sep="\t", na.strings="", verbose = F)
  planets$Date <- as.Date(planets$Date, format="%Y-%m-%d")
  planets <- subset(planets, Date >= as.Date(sdate))

  if (hasArg('cusorbs')) {
    orbs <- cusorbs
    if (length(cusorbs) != length(orbs)) {
      stop("There are missing custom orbs")
    }
  }

  for (currentCol in planetsLonCols) {
    zodDegreesCols <- paste(currentCol, zodDegrees, sep='_')
    exp1 <- parse(text = paste('c(zodDegreesCols) := list(', paste('diffDeg(get(currentCol),', zodDegrees, ',orbs, aspects)', sep='', collapse=', '), ')', sep=''))
    planets[, eval(exp1)]
  }

  # convert to long format and then to wide to build a column for each planetrad+planet+aspect
  planets <- data.table(melt(planets, id.var=c('Date'), measure.var=planetsGridZodCols))
  planets[, idx := paste(variable, value, sep='_')]
  setkey(planets, 'Date', 'variable', 'value', 'idx')
  planets <- planets[!is.na(value)]
  planets <- merge(planets, currency, by='Date')

  # split test and train data
  planets.train <- planets[Date < as.Date(edate)]
  planets.test <- planets[Date >= as.Date(edate)]
  # calculate the significance of the aspects
  significance.prop <- planets.train[, as.list(prop.table(table(Eff))), by='idx']
  significance.count <- planets.train[, as.list(table(Eff)), by='idx']
  significance <- merge(significance.prop, significance.count, by='idx')
  significance[, c('trend', 'count') := list(up.x-down.x, down.y+up.y)]
  # filter significance by 40% of trend
  significance <- significance[trend > threshold | trend < -threshold]
  #planets <- data.table(dcast(planets, Date ~ variable+value))
  # for the test data leave only the significant aspects
  planets.test <- planets.test[idx %in% significance$idx]

  return(list(sig=significance, test=planets.test))
}

funtionizethis  <- function() {
  # hourly rate history
  eurusd.hour <- read.table("~/trading/EURUSD_hour.csv", header = T, sep=",")
  names(eurusd.hour) <- c('Ticker', 'Date', 'Time', 'Open', 'Low', 'High', 'Close')
  eurusd.hour <- eurusd.hour[,-1]
  eurusd.hour$val <- eurusd.hour$Close - eurusd.hour$Open
  eurusd.hour$Date <- timeDate(paste(eurusd.hour$Date, eurusd.hour$Time), format = "%Y%m%d %H:%M:%S", zone = "CET", FinCenter = "CET")
  eurusd.hour.xts <- xts(eurusd.hour[,-1:-2], order.by=eurusd.hour[,1])

  # euro usd currency daily
  eurusd_full <- openCurrency("~/trading/EURUSD_day.csv")
  #eurusd <- openSecurity("~/trading/")
  # usd cad currency daily
  usdcad <- openCurrency("~/trading/USDCAD_day.csv")
  # transits USD chart
  trans.usa = openTrans("~/trading/transits_usa/USA_1997-2014_trans_orb1.tsv")
  trans.usa2 = openTrans("~/trading/transits_usa/USA_coinage_1997-2014_trans_orb1.tsv")
  # transits EUR chart
  # transits CAD chart
  trans.cad <- openTransXts("~/trading/1990-2015_trans_CAD.tsv")
  # test trans
  trans.test <- openTransXts("~/trading/transits_eur/test.tsv")
  # currency - transits EURUSD
  trans.eurusd.usa  <- mergeTrans(trans.usa, eurusd)
  trans.eurusd.usa2  <- mergeTrans(trans.usa2, eurusd)
  # currency - transits USDCAD
  trans.usdcad.usa  <- mergeTrans(trans.usa, usdcad)

  planets.eur <- read.table("~/trading/EUR_2000-2014_planets_20130518.tsv", header = T, sep="\t")
  planets.eur$Date <- as.Date(planets.eur$Date, format="%Y-%m-%d")
  write.csv(planets.eur, file=paste("~/mt4/planets1.csv", sep=''), eol="\r\n", quote=FALSE, row.names=FALSE)

  planets.usa <- read.table("~/trading/USA_2012_2014_planets_20130503.tsv", header = T, sep="\t")
  planets.usa$Date <- as.Date(planets.usa$Date, format="%Y-%m-%d")
  write.csv(planets.usa, file=paste("~/mt4/planets2.csv", sep=''), eol="\r\n", quote=FALSE, row.names=FALSE)

  #mars.jupiter[order(venus.jupiter$val)]
  eurusd.aggregated = aggregate(trans.price.eur$val, list(Ptran = trans.price.eur$Ptran, Aspect = trans.price.eur$Aspect, Prad = trans.price.eur$Prad, Sign = trans.price.eur$Sign), mean)
  # rename sex by sextil
  Aspect = gsub('sex', 'sextil', Aspect)
  # split string
  # strsplit(Aspect, 'x')
  # remove rows with missing variables
  # na.omit(sun.neptune)

  # system time
  nowDate = Sys.time()
  # exctract individual components of date
  wkday = weekdays(nowDate)
  month = months(nowDate)
  quarter = quarters(nowDate)

  # Calculate the moving average with a window of 10 points
  mov.avg <- ma(values, 1, 10, FALSE)

  ## calculate peaks and valleys
  currency.planets = merge(usdcad, planets, by = "Date")
  a <- currency.planets$Open
  ## detect local maxima and minima
  maxmin <- msExtrema(a, 50)
  ## visualize the result
  par(mfrow=c(1,1))
  plot(a, type="l")
  points((1:length(a))[maxmin$index.max],
         a[maxmin$index.max], col=2, pch=1)
  points((1:length(a))[maxmin$index.min],
         a[maxmin$index.min], col=3, pch=2)
  if (!is.R()){
    legend(x=18, y=3, col=2:3, marks=1:2, legend=c("maxima", "minima"))
  } else {
    legend(x=18, y=3, col=2:3, pch=1:2, legend=c("maxima", "minima"))
  }

  currency.max <- currency.planets[maxmin$index.max,]
  currency.min <- currency.planets[maxmin$index.min,]

  # max reshape by longitude
  l.max <- reshape(currency.max, varying = c("SULON", "MOLON", "MELON", "VELON", "MALON", "JULON", "SALON"), v.names = "lon", times = c("SULON", "MOLON", "MELON", "VELON", "MALON", "JULON", "SALON"),  direction = "long")
  #p2 <- ggplot(l.max, aes(x=lon, y=10, colour=time, shape=time)) + scale_x_continuous(breaks=seq(from=0, to=360, by=5)) + coord_polar(start = 1.57, direction=1) + scale_shape_manual("",values=c(1,2,3,4,5,6,7,8,9,10)) +  geom_jitter(position = position_jitter(height = .5))
  p2 <- qplot(y=l.max$lon, colour=l.max$time, shape=l.max$time) + scale_y_continuous(breaks=seq(from=1, to=360, by=10)) + scale_shape_manual("",values=c(1,2,3,4,5,6,7,8,9,10))

  # max reshape by latitude
  l.max.lat <- reshape(currency.max, varying = c("SULAT", "MOLAT", "MELAT", "VELAT", "MALAT", "JULAT", "SALAT", "URLAT", "NELAT", "PLLAT"), v.names = "lat", times = c("SULAT", "MOLAT", "MELAT", "VELAT", "MALAT", "JULAT", "SALAT", "URLAT", "NELAT", "PLLAT"),  direction = "long")
  p2 <- qplot(y=l.max.lat$lat, colour=time, shape=time, data=l.max.lat) + scale_y_continuous(breaks=seq(from=-10, to=10, by=2)) + scale_shape_manual("",values=c(1,2,3,4,5,6,7,8,9,10))

  # max reshape by speed
  l.max.sp <- reshape(currency.max, varying = c("SUSP", "MOSP", "MESP", "VESP", "MASP", "JUSP", "SASP", "URSP", "NESP", "PLSP"), v.names = "speed", times = c("SUSP", "MOSP", "MESP", "VESP", "MASP", "JUSP", "SASP", "URSP", "NESP", "PLSP"),  direction = "long")
  p2 <- qplot(y=l.max.sp$speed, colour=time, shape=time, data=l.max.sp) + scale_y_continuous(breaks=seq(from=-1, to=1.3, by=0.05), limits=c(-1,1.5)) + scale_shape_manual("",values=c(1,2,3,4,5,6,7,8,9,10))

  # min reshape by longitude
  l.min <- reshape(currency.min, varying = c("SULON", "MOLON", "MELON", "VELON", "MALON", "JULON", "SALON"), v.names = "lon", times = c("SULON", "MOLON", "MELON", "VELON", "MALON", "JULON", "SALON"),  direction = "long")
  p1 <- qplot(y=l.min$lon, colour=l.min$time, shape=l.min$time) + scale_y_continuous(breaks=seq(from=1, to=360, by=10)) + scale_shape_manual("",values=c(1,2,3,4,5,6,7,8,9,10))

  # min reshape by longitude
  l.min.lat <- reshape(currency.min, varying = c("SULAT", "MOLAT", "MELAT", "VELAT", "MALAT", "JULAT", "SALAT", "URLAT", "NELAT", "PLLAT"), v.names = "lat", times = c("SULAT", "MOLAT", "MELAT", "VELAT", "MALAT", "JULAT", "SALAT", "URLAT", "NELAT", "PLLAT"),  direction = "long")
  p1 <- qplot(y=l.min.lat$lat, colour=l.min.lat$time, shape=l.min.lat$time) + scale_y_continuous(breaks=seq(from=-10, to=10, by=2)) + scale_shape_manual("",values=c(1,2,3,4,5,6,7,8,9,10))

  # min reshape by speed
  l.min.sp <- reshape(currency.min, varying = c("SUSP", "MOSP", "MESP", "VESP", "MASP", "JUSP", "SASP", "URSP", "NESP", "PLSP"), v.names = "speed", times = c("SUSP", "MOSP", "MESP", "VESP", "MASP", "JUSP", "SASP", "URSP", "NESP", "PLSP"),  direction = "long")
  p1 <- qplot(y=l.min.sp$speed, colour=l.min.sp$time, shape=l.min.sp$time) + scale_y_continuous(breaks=seq(from=-1, to=1.3, by=0.05), limits=c(-1,1.5)) + scale_shape_manual("",values=c(1,2,3,4,5,6,7,8,9,10))

  # density plot
  p1 <- ggplot(l.min, aes(x = l.min$lon)) + scale_x_continuous(breaks=seq(from=0, to=360, by=5), limits=c(0,360)) + geom_density(adjust=1/12) + coord_polar(start = 1.57, direction=-1) + theme_update(axis.text.x = theme_text(angle = -30, size = 6))
  p2 <- ggplot(l.max, aes(x = l.max$lon)) + scale_x_continuous(breaks=seq(from=0, to=360, by=5), limits=c(0,360)) + geom_density(adjust=1/12) + coord_polar(start = 1.57, direction=-1) + theme_update(axis.text.x = theme_text(angle = -30, size = 6))

  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 2)))
  print(p1, vp = vplayout(1, 1))
  print(p2, vp = vplayout(1, 2))

  d <- ggplot(l, aes(x = lon))
  d + geom_density(adjust=5)

  d + geom_density2d()
  d + scale_colour_hue("clarity")
  d + scale_colour_hue(expression(clarity[beta]))

  # Adjust luminosity and chroma
  d + scale_colour_hue(l=40, c=30)
  d + scale_colour_hue(l=70, c=30)
  d + scale_colour_hue(l=70, c=150)
  d + scale_colour_hue(l=80, c=150)

  # Change range of hues used
  d + scale_colour_hue(h=c(0, 360, l=100, c=200))

  ggplot(filld, aes(xmin = start, xmax = end, ymin = 4, ymax = 5, fill = label)) + geom_rect() + geom_segment(aes(x = 4, y = 0, xend = 4, yend = 5, colour = label), size = 2, show_guide = FALSE) + geom_text(aes(x = p, y = 4.5, label = label), colour = "white", size = 10) + coord_polar() + scale_y_continuous(limits = c(0, 5))
}

splitdf <- function(dataframe, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dataframe)
  trainindex <- sample(index, trunc(length(index)/1.5))
  trainset <- dataframe[trainindex, ]
  testset <- dataframe[-trainindex, ]
  list(trainset=trainset,testset=testset)
}

loadTrans <- function(datefix, aspmode, asptype, maxasp) {
  trans <- openTrans("~/trading/t_eurfix/trans_30.tsv", datefix, aspmode, asptype)
  trans <- trans[,names(trans) %ni% 'endEffect']
  trans <- data.table(trans)
  selected <- trans[,idx[which(S2 >= maxn(S2, maxasp))], by=c('Date')]
  setnames(selected, 'V1', 'idx')
  trans <- merge(selected, trans, by=c('Date', 'idx'))
  as.data.frame(trans)
}

loadTransHist <- function(datefix, aspmode, asptype, maxasp) {
  trans <- loadTrans(datefix, asptype, asptype, maxasp)
  eurusd <- openSecurity("~/trading/EURUSD_day_fxpro_20130611.csv")
  # merge with the currency history
  mergeTrans(trans, eurusd)
}

buildTransOpts <- function() {
  dateFix <- c(0,1)
  maxAspects <- seq(1,10)
  expand.grid(dateFix, aspModes, aspTypes, maxAspects)
}

testGAOptimization <- function() {
  trans.hist <- loadTransHist(0, 'all', 'all', 7)
  trans.training <- subset(trans.hist, Date > as.Date("1999-01-01") & Date < as.Date("2011-01-01"))
  trans.testing <- subset(trans.hist, Date > as.Date("2012-01-01"))
  cols <- c('PT', 'AS', 'PR', 'HR', 'SI', 'LON', 'LAT', 'SP', 'PRLON', 'S', 'S2', 'WD',
            'SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'SA', 'UR', 'NE', 'PL',
            'SUR', 'MOR', 'MER', 'VER', 'MAR', 'JUR', 'SAR', 'URR', 'NER', 'PLR',
            'SULO', 'MOLO', 'MELO', 'VELO', 'MALO', 'JULO', 'SALO', 'URLO', 'NELO', 'PLLO',
            'MOSP', 'MESP', 'VESP', 'MASP', 'JUSP', 'SASP', 'URSP', 'NESP', 'PLSP',
            'MOS', 'MES', 'VES', 'MAS', 'JUS', 'SAS', 'URS', 'NES', 'PLS',
            'MOLA', 'MELA', 'VELA', 'MALA', 'JULA', 'SALA', 'PLLA')

  optimizeRandomForest <- function(string) {
    inc <- which(string == 1)
    selcols <- cols[inc]
    model <- randomForest(Eff ~ ., data = trans.training[,c('Eff',selcols)], importance=TRUE, keep.forest=TRUE, ntree=300)
    err <- median(model$err.rate[,1])
    trans.testing$predicted <- predict(model, newdata=trans.testing[,selcols])
    t1 <- prop.table(table(trans.testing$Eff == trans.testing$predicted))
    tdiff <- as.numeric(round(abs(t1['TRUE']-t1[['FALSE']])*100, digits=2))
    print(tdiff)
    tdiff
  }

  fitnessmax <- function(x) optimizeRandomForest(x)
  fitnessmin <- function(x) -optimizeRandomForest(x)

  # selection
  #gabin_lrSelection - 0.25
  #gabin_nlrSelection
  #gabin_rwSelection - 0.30
  #gabin_tourSelection
  # crossovers
  #gabin_spCrossover(object, parents, ...)
  #gabin_uCrossover(object, parents, ...)
  # mutation
  #gabin_raMutation
  ga("binary", fitness=fitnessmax, nBits=length(cols), names=cols,
     monitor=gaMonitor, maxiter=50, run=10, popSize=100,
     selection=gabin_tourSelection, elitism=0.1)
}

bestRandomForest <- function() {
  threshold=0.65
  trans.hist <- loadTransHist(0, 'all', 'all', 7)
  trans.training <- subset(trans.hist, Date > as.Date("1999-01-01") & Date < as.Date("2009-01-01"))
  trans.testing <- subset(trans.hist, Date > as.Date("2009-01-01") & Date < as.Date("2009-06-01"))
  # start prediction model
  #splits <- splitdf(trans.hist)
  cols <- c("SI", "LON", "SP", "WD", "SU", "ME", "VE", "MA", "JU", "SA",
            "PL", "SULO", "MOLO", "SALO", "URLO", "NELO", "SASP", "PLSP",
            "MOS", "VES", "MAS", "JUS", "SAS", "URS", "NES", "MOLA", "MELA",
            "VELA", "MALA", "JULA")

  #model <- randomForest(Eff ~ ., data = trans.training[,cols], importance=TRUE, keep.forest=TRUE)
  model <- randomForest(Eff ~ ., data = trans.training[,c('Eff',cols)], importance=TRUE, keep.forest=TRUE, ntree=300)
  print(model)
  trans.testing$predicted <- predict(model, newdata=trans.testing[,cols])
  print(prop.table(table(trans.testing$Eff == trans.testing$predicted)))
  print(table(trans.testing$Eff == trans.testing$predicted))
  trans.testing <- data.table(trans.testing)
  setkey(trans.testing, 'Date')
  pred.table <- trans.testing[,as.list(table(predicted)), by=c('Date')]
  pred.table[,prop := (down-up)/(down+up)]
  eurusd <- openSecurity("~/trading/EURUSD_day_fxpro_20130611.csv")
  eurusd <- data.table(eurusd)
  pred.table <- merge(pred.table, eurusd, by='Date')
  pred.table[, predEff := lapply(prop, function(x) ifelse(x > threshold, 'down', ifelse(x < -1*threshold, 'up', NA)))]
  print(table(pred.table$Eff==pred.table$predEff,useNA='always'))
  print(prop.table(table(pred.table$Eff==pred.table$predEff,useNA='always')))
  model
}

predTableCount <- function(pred.table, threshold) {
  pred.table <- data.table(pred.table)
  setkey(pred.table, 'Date')
  pred.table <- pred.table[,as.list(table(predicted)), by=c('Date')]
  pred.table[,prop := (down-up)/(down+up)]
  pred.table[, predEff := lapply(prop, function(x) ifelse(x > threshold, 'down', ifelse(x < -1*threshold, 'up', NA)))]
}

testRandomForest <- function(sinkfile, sigthreshold) {
  if (!hasArg('sinkfile')) stop("Provide a sink filename.")
  sinkfile <- paste("~/trading/predict/", sinkfile, ".txt", sep='')
  sink(npath(sinkfile), append=TRUE)
  transOpts <- buildTransOpts()

  for (n in 1:nrow(transOpts)) {
    datefix <- 0
    aspmode <- as.character(transOpts[[n,2]])
    asptype <- as.character(transOpts[[n,3]])
    maxasp <- transOpts[[n,4]]
    cat("==========================================================================")
    cat("\ntransOpts #", n, " of ", nrow(transOpts), " -  datefix =", datefix, "-  asptype =", aspmode, "-  asptype =", asptype, "-  maxasp =", maxasp, "\n")
    trans.hist <- loadTransHist(datefix, aspmode, asptype, maxasp)

    # start prediction model
    splits <- splitdf(trans.hist)
    trans.training <- splits$trainset
    trans.testing <- splits$testset
    cols <- c('Eff', 'PT', 'SI', 'SP', 'HR',
              "SU", "MO", "ME", "VE", "MA", "JU", "SA", "UR", "NE", "PL",
              #"SUR", "MOR", "MER", "VER", "MAR", "JUR", "SAR", "URR", "NER", "PLR"
              #"SULO", "MOLO", "VELO", "MALO", "JULO", "SALO", "URLO", "NELO", "PLLO",
              "MOSP", "MESP", "VESP", "MASP", "JUSP", "SASP", "URSP", "NESP", "PLSP",
              "MOLA", "MELA", "VELA", "MALA", "JULA", "SALA", "PLLA"
              )

    #model <- randomForest(Eff ~ ., data = trans.training[,cols], importance=TRUE, keep.forest=TRUE)
    model <- randomForest(Eff ~ ., data = trans.training[,cols], importance=TRUE, keep.forest=TRUE, ntree=300)

    #trans.testing$predicted <- predict(model, newdata=trans.testing[,cols])
    #setkey(trans.testing, 'Date')
    #trans.testing[,c('up','down') := as.list(table(Eff)), by=c('idx')]
    #trans.testing[,count := down+up]

    # generate from testing data some random samples and check the prediction results
    terr <- list()
    tdiffs <- list()
    tl1 <- list()
    tl2 <- list()
    samples <- generateSamples(trans.testing, 3)
    for (mysample in samples) {
      predicted <- predict(model, newdata=mysample[,cols])
      t1 <- prop.table(table(mysample$Eff == predicted))
      t2 <- table(mysample$Eff == predicted)
      tdiff <- round(abs(t1['TRUE']-t1[['FALSE']])*100, digits=2)

      if (!is.na(tdiff) & tdiff >= sigthreshold) {
        err.rate <- median(model$err.rate[,1])
        terr[[length(terr)+1]] <- err.rate
        tdiffs[[length(tdiffs)+1]] <- tdiff
        tl1[[length(tl1)+1]] <- t1
        tl2[[length(tl2)+1]] <- t2
      }
    }

    # if the differences are significant in all the test samples
    if (length(tdiffs) == length(samples)) {
      for (i in 1:length(tdiffs)) {
        # print the diffs and the tables
        cat("===================== diff=", tdiffs[[i]], "-  error=", terr[[i]], "\n")
        print(tl1[[i]])
        print(tl2[[i]])
        cat("\n")
      }
      # display the average
      cat("%%%", median(unlist(tdiffs)), "%%%%%%%%%%%%%%%%%%%%\n\n")
    }
    else {
      writeLines("\t\t\tInsignificant")
      cat("\n")
    }


    #varImpPlot(model, type=1, sort=FALSE, n.var=40)
    # remove garbage
    gc()
  }

  sink()
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

#dput(as.character(selcols))

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

gaint_raMutation <- function(object, parent) {
  mutate <- parent <- as.vector(object@population[parent, ])
  n <- length(parent)
  j <- sample(1:n, size = 1)
  mutate[j] <- sample(object@min[j]:object@max[j], 1)
  return(mutate)
}

gaint_rwSelection <- function (object, ...) {
  prob <- abs(object@fitness)/sum(abs(object@fitness))
  sel <- sample(1:object@popSize, size = object@popSize, prob = pmin(pmax(0, prob), 1, na.rm = TRUE), replace = TRUE)
  out <- list(population = object@population[sel, , drop = FALSE],
              fitness = object@fitness[sel])
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

processTrans <- function(trans, currency, aspnames, asptypes) {
  currency.train <- subset(currency, Date > as.Date("1998-01-01") & Date < as.Date("2012-01-01"))
  currency.test <- subset(currency, Date > as.Date("2012-01-01"))
  # select only the specified aspects
  trans.cur <- subset(trans, AS %in% aspnames)
  # reset the aspects that are not in the required types
  trans.cur[,planetsList[[1]]] <- t(apply(trans.cur[,c(planetsList[[1]],aspectTypesCols)], 1, removeAspectsOutType, asptype=asptypes))
  # merge with currency data splitting by test & train data
  trans.cur.train <- mergeTrans(trans.cur, currency.train)
  trans.cur.test <- mergeTrans(trans.cur, currency.test)
  list(all=trans.cur, train=trans.cur.train, test=trans.cur.test)
}

processPlanets <- function(planets, currency, sdate, edate) {
  currency <- data.table(currency)
  currency.train <- subset(currency, Date > as.Date(sdate) & Date < as.Date(edate))
  currency.test <- subset(currency, Date > as.Date(edate))
  # merge with currency data splitting by test & train data
  planets.train <- merge(currency.train, planets, by='Date')
  planets.test <- merge(currency.test, planets, by='Date')
  list(all=planets, train=planets.train, test=planets.test)
}

colNLon <- function() {
  c(planetsLonCols)
}

colNLat <- function() {
  c(planetsLatCols)
}

colNSp <- function() {
  c(planetsSpCols)
}

colNLonGrid <- function() {
  c(planetsGridLonCols)
}

colMix1 <- function() {
  c(planetsLonCols, planetsGridLonCols)
}

colMix2 <- function() {
  c(planetsLonCols, planetsGridLonCols, planetsSpCols)
}

colMix3 <- function() {
  c(planetsLonCols, planetsGridLonCols, planetsSpCols)
}

colAll <- function() {
  c(planetsLonCols, planetsSpCols, planetsLatCols, planetsCombLonCols, planetsGridLonCols)
}

testDailyRandomForest <- function(sinkfile, planetsdir, fileno, securitydir, securityfile,
                                  sdate, edate, modelfun='randomForest', predproc=FALSE, colNames, chartfile, ...) {
  # Init clock
  ptm <- proc.time()
  if (!hasArg('sinkfile')) stop("Provide a sink filename.")
  sinkfile <- paste("~/trading/predict/", sinkfile, ".txt", sep='')
  sink(npath(sinkfile), append=TRUE)
  # currency data
  currency <- openSecurity(paste("~/trading/", securitydir, "/", securityfile, ".csv", sep=''))
  if (hasArg('chartfile')) chart <- fread(paste("~/trading/charts/", chartfile, '.tsv', sep=''), header = T, sep="\t", na.strings="")

  # process planets
  if (exists('chart')) {
    planets <- openPlanets(paste("~/trading/", planetsdir, "/planets_", fileno, ".tsv", sep=''), chart)
  }
  else {
    planets <- openPlanets(paste("~/trading/", planetsdir, "/planets_", fileno, ".tsv", sep=''))
  }

  cat("\n")

  dailyRFFitness <- function(string) {
    looptm <- proc.time()
    # the chromosome that indicate us which columns to include
    inc <- which(string == 1)
    selcols <- colNames[inc]

    cat("---------------------------------------------------------------------------------\n")
    cat("testDailyRandomForestSolution(planetsdir=", shQuote(planetsdir), ", fileno=", fileno, ",\n", sep='')
    cat("\t securitydir=", shQuote(securitydir), ", securityfile=", shQuote(securityfile), sep='')
    cat(", sdate=", shQuote(sdate), ", edate=", shQuote(edate), ", modelfun=", shQuote(modelfun), sep='')
    cat(", predproc=", predproc, ", chartfile=", shQuote(chartfile), ",\n", sep='')
    cat('\t selcols=c(', paste(shQuote(selcols), collapse=","), "))\n", sep='')

    # process planets
    planets.sp <- processPlanets(planets, currency, sdate, edate)

    # build the samples from testing data
    samples <- generateSamples(planets.sp$test, 3)
    # build model
    modelfun <- get(modelfun)
    model <- modelfun(Eff ~ ., data = planets.sp$train[,c('Eff',selcols), with=FALSE], ...)

    # test the samples
    fitness <- list()
    for (i in 1:length(samples)) {
      predEff <- predict(model, newdata=samples[[i]][,selcols, with=FALSE])
      if (predproc) {
        predEff <- predEffProcessFactor(predEff)
      }
      samples[[i]][, predEff := predEff]
      # day aggregated predictions
      test.result <- testDailyRandomForestPredictions(samples[[i]])
      cat("\t Total = ", test.result$totdays, "/ Trend = ", test.result$tredays, "/ % = ", test.result$percent, "\n")
      fitness[[length(fitness)+1]] <- test.result$tredays
    }

    # fitted value
    evfitness <- fitnessMeanStability(fitness)
    # how long taked to calculate
    cat("\t Predict execution/loop time: ", proc.time()-ptm, " - ", proc.time()-looptm, "\n")
    # output
    cat("### = ", evfitness, "\n")
    # collect garbage
    rm(planets.sp, samples, model)
    gc()
    # return fit
    evfitness
  }

  ga("binary", fitness=dailyRFFitness, names=colNames, nBits=length(colNames),
     monitor=gaMonitor, maxiter=1000, run=50, popSize=500,
     selection=gabin_rwSelection)

  sink()
}

testDailyRandomForestSolution <- function(planetsdir, fileno, securitydir, securityfile,
                                          sdate, edate, selcols, modelfun='randomForest', predproc=FALSE, chartfile, ...) {
  # currency data
  currency <- openSecurity(paste("~/trading/", securitydir, "/", securityfile, ".csv", sep=''))
  colNames <- c(paste(planetsBaseCols, 'LON', sep=''), paste(planetsBaseCols, 'LAT', sep=''), paste(planetsBaseCols, 'SP', sep=''))
  if (hasArg('chartfile')) chart <- fread(paste("~/trading/charts/", chartfile, '.tsv', sep=''), header = T, sep="\t", na.strings="")

  # process planets
  if (exists('chart')) {
    planets <- openPlanets(paste("~/trading/", planetsdir, "/planets_", fileno, ".tsv", sep=''), chart)
  }
  else {
    planets <- openPlanets(paste("~/trading/", planetsdir, "/planets_", fileno, ".tsv", sep=''))
  }

  # process planets
  planets.sp <- processPlanets(planets, currency, sdate, edate)

  # build the samples from testing data
  samples <- generateSamples(planets.sp$test, 3)
  # build model
  #model <- rpart(as.factor(Eff) ~ ., data = planets.sp$train[,c('Eff',selcols), with=FALSE], method='class')
  modelfun <- get(modelfun)
  model <- modelfun(Eff ~ ., data = planets.sp$train[,c('Eff',selcols), with=FALSE], ...)

  # test the samples
  for (i in 1:length(samples)) {
    predEff <- predict(model, newdata=samples[[i]][,selcols, with=FALSE])
    if (predproc) {
      predEff <- predEffProcessFactor(predEff)
    }
    samples[[i]][, predEff := predEff]
    # day aggregated predictions
    test.result <- testDailyRandomForestPredictions(samples[[i]])
    cat("Total = ", test.result$totdays, "/ Trend = ", test.result$tredays, "/ % = ", test.result$percent, "\n")
  }
  return(model)
}

predEffProcessFactor <- function(predEff) {
  prediffs <- predEff[,1]-predEff[,2]
  ifelse(prediffs >= 0,  'down', 'up')
}

testCorrelationOptimization <- function(sinkfile, directory, fileno) {
  # Init clock
  ptm <- proc.time()
  if (!hasArg('sinkfile')) stop("Provide a sink filename.")
  sinkfile <- paste("~/trading/predict/", sinkfile, ".txt", sep='')
  sink(npath(sinkfile), append=TRUE)
  # correlation methods to test
  corMethods <- c('canberra','euclidian','binary')
  # binarize
  binModes <- c(0,1)
  # remove zero aspects
  zeroaspectsModes <- c(0,1)
  # quitile modes
  qinModes <- c('q1','q2','q3','q4','q5')
  # max aspects modes
  maxaspModes <- c(1,2,3,4,5,6,7)
  # predict Thresholds
  predThresholds <- seq(0.1, 0.9, by=0.1)
  # currency data
  currency <- openSecurity("~/trading/EURUSD_day_fxpro_20130611.csv")
  trans <- openTrans(paste("~/trading/", directory, "/trans_", fileno, ".tsv", sep=''), 1)

  corFitness <- function(x) {
    looptm <- proc.time()
    # build the parameters based on GA indexes
    aspnames <- aspectsCombList[[x[1]]]
    asptypes <- aspectTypesList[[x[2]]]
    cormethod <- as.character(corMethods[[x[3]]])
    binarize <- binModes[[x[4]]]
    rmzeroaspects <- zeroaspectsModes[[x[5]]]
    qinmode <- as.character(qinModes[[x[6]]])
    maxasp <- maxaspModes[[x[7]]]
    kplanets <- planetsCombList[[x[8]]]
    kaspects <- aspectsCombList[[x[9]]]
    predtreshold <- predThresholds[[x[10]]]

    cat("---------------------------------------------------------------------------------\n")
    cat("testCorrelationSolution(directory=", shQuote(directory), ", fileno=", fileno, ",\n", sep='')
    cat('\t aspnames=c(', paste(shQuote(aspnames), collapse=","), "),\n", sep='')
    cat('\t asptypes=c(', paste(shQuote(asptypes), collapse=","), "),\n", sep='')
    cat("\t cormethod=", shQuote(cormethod), ", binarize=", binarize, ", rmzeroaspects=",
        rmzeroaspects, ", qinmode=", shQuote(qinmode), ", maxasp=",
        maxasp, ", predtreshold=", predtreshold, ",\n", sep='')
    cat('\t kplanets=c(', paste(shQuote(kplanets), collapse=","), "),\n", sep='')
    cat('\t kaspects=c(', paste(shQuote(kaspects), collapse=","), "))\n", sep='')

    # process transits
    trans.sp <- processTrans(trans, currency, aspnames, asptypes)
    # build the samples from testing data
    samples <- generateSamples(trans.sp$test, 3)
    # generate the predict table
    predict.table <- predictTransTable(trans.sp$all, trans.sp$train, cormethod, kplanets, kaspects, binarize, rmzeroaspects, qinmode, maxasp)

    # test the samples
    fitness <- list()
    for (i in 1:length(samples)) {
      # day aggregated predictions
      predict.table.aggr <- aggregatePredictTransTable(predict.table, predtreshold)
      test.result <- testAggregatedPredictTransTable(predict.table.aggr, samples[[i]], currency)
      cat("\t Total = ", test.result$totdays, "/ Trend = ", test.result$tredays, "/ % = ", test.result$percent, "\n")
      fitness[[length(fitness)+1]] <- test.result$tredays
    }

    # fitted value
    evfitness <- fitnessMeanStability(fitness)
    # how long taked to calculate
    cat("\t Predict execution/loop time: ", proc.time()-ptm, " - ", proc.time()-looptm, "\n")
    # output
    cat("### = ", evfitness, "\n")
    # collect garbage
    gc()
    # return fit
    evfitness
  }

  minvals <- c(1,1,1,1,1,1,1,1,1,1)
  maxvals <- c(length(aspectsCombList), length(aspectTypesList), length(corMethods),
               length(binModes), length(zeroaspectsModes), length(qinModes),
               length(maxaspModes), length(planetsCombList), length(aspectsCombList), length(predThresholds))
  varnames <- c('aspnames', 'asptypes', 'cormethod', 'binarize', 'rmzeroaspects', 'qinmode', 'maxasp', 'kplanets', 'kaspects', 'predtreshold')

  ga("real-valued", fitness=corFitness, names=varnames,
     monitor=gaMonitor, maxiter=500, run=30, popSize=400, min=minvals, max=maxvals,
     selection=gaint_rwSelection, mutation=gaint_raMutation,
     crossover=gareal_laCrossover, population=gaint_Population)

  sink()
}

testCorrelationSolution <- function(directory, fileno, aspnames, asptypes, cormethod, binarize,
                                    rmzeroaspects, qinmode, maxasp, kplanets, kaspects, predtreshold) {
  # currency data
  currency <- openSecurity("~/trading/EURUSD_day_fxpro_20130611.csv")
  trans <- openTrans(paste("~/trading/", directory, "/trans_", fileno, ".tsv", sep=''), 1)

  # process transits
  trans.sp <- processTrans(trans, currency, aspnames, asptypes)
  # build the samples from testing data
  samples <- generateSamples(trans.sp$test, 5)
  # generate the predict table
  predict.table <- predictTransTable(trans.sp$all, trans.sp$train, cormethod, kplanets, kaspects, binarize, rmzeroaspects, qinmode, maxasp)

  # test the samples
  fitness <- list()
  for (i in 1:length(samples)) {
    # day aggregated predictions
    predict.table.aggr <- aggregatePredictTransTable(predict.table, predtreshold)
    test.result <- testAggregatedPredictTransTable(predict.table.aggr, samples[[i]], currency)
    print(test.result$t1)
    print(test.result$t2)
    cat("\n Total = ", test.result$totdays, "/ Trend = ", test.result$tredays, "/ % = ", test.result$percent, "\n\n")
    fitness[[length(fitness)+1]] <- test.result$tredays
  }

  # fitted value
  cat("\t ### = ", fitnessMeanStability(fitness), "\n\n")
}

fitnessMeanStability <- function(fitness) {
  # last fitness is greater due considers the complete test data we need to
  # adjust dividing by 2 that is the rule used to generate samples.
  fitness[[length(fitness)]] <- fitness[[length(fitness)]]/2
  # penalize the less stable solutions removing a proportional part depending
  # how much are deviated.
  fitness <- unlist(fitness)
  round(mean(fitness) * (1 - sd(fitness) / mean(fitness)))
}

completeEffectList <- function(X) {
  effects <- c('up','down')
  missidx <- effects[effects %ni% names(X)]
  X[missidx] <- 0
  X
}

testDailyRandomForestPredictions <- function(sample.predicted) {
  # less predictions over the number of trading days decay
  totdays <- length(unique(sample.predicted$Date))
  # compare the prediction with the day effect
  testPrediction <- function(x) ifelse(is.na(x[[1]]) | is.na(x[[2]]), NA, x[[1]]==x[[2]])
  sample.predicted[, test := apply(.SD, 1, testPrediction), .SDcols=c('predEff','Eff')]
  # process results
  t1 <- prop.table(table(sample.predicted$test, useNA='always'))
  t2 <- addmargins(table(sample.predicted$test, useNA='always'))
  fitness <- tredays <- percent <- 0
  # only if there are results complete results
  if (all(c('TRUE', 'FALSE', NA) %in% names(t2))) {
    tredays <- abs(as.numeric(t2['TRUE'])-as.numeric(t2['FALSE']))
    percent <- round((tredays/totdays)*100, digits=2)
  }

  list(percent=percent, totdays=totdays, tredays=tredays, t1=t1, t2=t2)
}

testAggregatedPredictTransTable <- function(predict.table.aggr, trans.test, currency) {
  predict.table.aggr <- subset(predict.table.aggr, Date %in% trans.test$Date)
  predict.table.aggr <- merge(predict.table.aggr, currency, by='Date')
  # less predictions over the number of trading days decay
  totdays <- length(unique(trans.test$Date))
  # compare the prediction with the day effect
  testPredictAggr <- function(x) ifelse(is.na(x[[1]]) | is.na(x[[2]]), NA, x[[1]]==x[[2]])
  predict.table.aggr[, test := apply(.SD, 1, testPredictAggr), .SDcols=c('predEff','Eff')]
  # process results
  t1 <- prop.table(table(predict.table.aggr$test, useNA='always'))
  t2 <- addmargins(table(predict.table.aggr$test, useNA='always'))
  fitness <- tredays <- percent <- 0
  # only if there are results complete results
  if (all(c('TRUE', 'FALSE', NA) %in% names(t2))) {
    tredays <- abs(as.numeric(t2['TRUE'])-as.numeric(t2['FALSE']))
    percent <- round((tredays/totdays)*100, digits=2)
  }

  list(percent=percent, totdays=totdays, tredays=tredays, t1=t1, t2=t2)
}

aggregatePredictTransTable <- function(predict.table, threshold) {
  predict.table <- data.table(predict.table)
  setkey(predict.table, 'Date')
  predict.table.aggr <- predict.table[,as.list(completeEffectList(table(corEff))), by=c('Date')]
  predict.table.aggr[,prop := (down-up)/(down+up)]
  # proportion determines a trend that overpass the threshold
  predict.table.aggr[, predEff := lapply(prop, function(x) ifelse(x > threshold, 'down', ifelse(x < -threshold, 'up', NA)))]
}

diffDeg <- function(x, y, comborbs, aspects) {
  vals <- abs(((x-y+180) %% 360) - 180)
  for (i in 1:length(aspects)) {
    vals[vals >= aspects[i]-comborbs[i] & vals <= aspects[i]+comborbs[i]] <- aspects[i]
  }
  vals[vals %ni% aspects] <- 'non'
  return(vals)
}

#ev <- evtree(choice ~ ., data = BBBClub, minbucket = 10, maxdepth = 2)
#rp <- as.party(rpart(choice ~ ., data = BBBClub, minbucket = 10))
#ct <- ctree(choice ~ ., data = BBBClub, minbucket = 10, mincrit = 0.99)
runtest <- function() {
  testDailyRandomForest('output21','dplanets',2,'currency','EURUSD_fxpro','1998','2012','rpart',T,colMix1(),'bceborn')
}

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

plotGridAspectsEffect <- function(plotfile, ds, threshold, resplot=T) {
  selCols <- planetsGridAspCols[planetsGridAspCols %in% colnames(ds)]
  tdiffs <- list()
  if (resplot) pdf(npath(paste("~/", plotfile, ".pdf", sep='')), width = 11, height = 8, family='Helvetica', pointsize=12)
  for (i in 1:length(selCols)) {
    dsp <- subset(ds, get(selCols[[i]]) >= 0)
    if (nrow(dsp) > 0) {
      t1 <- prop.table(table(dsp$Eff))
      tdiff <- as.numeric(round(abs(t1['down']-t1[['up']])*100, digits=2))
      if (tdiff >= threshold) {
        tdiffs[[selCols[[i]]]] <- tdiff
        if (resplot) {
          dsp[, selCols[[i]] := as.factor(get(dsp[,selCols[[i]]]))]
          p1 <- ggplot(aes_string(x=selCols[[i]], fill="Eff"), data=dsp) + geom_bar(position="fill") + xlab(selCols[[i]]) + scale_y_continuous(breaks=seq(from=0, to=1, by=0.1))
          p2 <- ggplot(aes_string(x=selCols[[i]], fill="Eff"), data=dsp) + geom_bar() + xlab(selCols[[i]])
          grid.newpage()
          pushViewport(viewport(layout = grid.layout(1, 2)))
          print(p1, vp = vplayout(1, 1))
          print(p2, vp = vplayout(1, 2))
        }
      }
    }
  }
  if (resplot) dev.off()
  return(tdiffs)
}

testDailyPlanetsOrbsGA <- function(sinkfile, planetsdir, fileno, securitydir, securityfile, sdate, edate, chartfile) {
  # sink output
  if (!hasArg('sinkfile')) stop("Provide a sink filename.")
  sinkfile <- paste("~/trading/predict/", sinkfile, ".txt", sep='')
  sink(npath(sinkfile), append=TRUE)
  # currency data
  currency <- openSecurity(paste("~/trading/", securitydir, "/", securityfile, ".csv", sep=''))
  chart <- fread(npath(paste("~/trading/charts/", chartfile, '.tsv', sep='')), sep="\t", na.strings="")
  itest <- 1
  # Init clock
  ptm <- proc.time()

  testDailyPlanetsOrbs <- function(cusorbs) {
    looptm <- proc.time()
    # round orbs to 2 decimals
    cusorbs <- round(cusorbs, digits=2)
    planets <- openPlanets(paste("~/trading/", planetsdir, "/planets_", fileno, ".tsv", sep=''), chart, cusorbs)
    # process planets
    planets.sp <- processPlanets(planets, currency, sdate, edate)
    # process analysis
    fitness <- plotGridAspectsEffect(paste(chartfile, '-', itest), planets.sp$train, 30, F)

    cat("\n---------------------------------------------------------------------------------\n")
    cat("Test #", itest, "\n", sep='')
    cat("testDailyPlanetsOrbsSolution(planetsdir=", shQuote(planetsdir), ", fileno=", fileno, sep='')
    cat(", securitydir=", shQuote(securitydir), ",\n\t securityfile=", shQuote(securityfile), sep='')
    cat(", sdate=", shQuote(sdate), ", edate=", shQuote(edate), ", chartfile=", shQuote(chartfile), sep='')
    cat(",\n\t cusorbs=c(", paste(cusorbs, collapse=","), "))\n", sep='')
    cat("\t Predict execution/loop time: ", proc.time()-ptm, " - ", proc.time()-looptm, "\n")
    cat("###", length(fitness), " / %%% =", median(unlist(fitness)), "\n\n")
    # how long taked to calculate
    itest <<- itest+1

    # collect garbage
    rm(planets, planets.sp)
    gc()
    return(length(fitness))
  }

  minvals = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
  maxvals = c(4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0)
  varnames = c('0', '30', '45', '60', '72', '90', '120', '135', '144', '150', '180', '18', '40', '52', '80', '104', '108', '155', '160')
  solutions <- matrix(NA, nrow = 100, ncol = length(minvals))

  for (j in 1:length(minvals)) {
    solutions[,j] <- runif(100, minvals[j], maxvals[j])
  }

  suggested <- c(3.0, 1.0, 1.0, 1.0, 1.0, 3.0, 3.0, 1.0, 1.0, 1.0, 3.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                 3.0, 0.5, 0.5, 0.5, 0.5, 3.0, 3.0, 0.5, 0.5, 0.5, 3.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                 3.0, 1.5, 1.5, 1.5, 1.5, 3.0, 3.0, 1.5, 1.5, 1.5, 3.0, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5,
                 4.0, 1.0, 1.0, 1.0, 1.0, 4.0, 4.0, 1.0, 1.0, 1.0, 4.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                 4.0, 2.0, 2.0, 2.0, 2.0, 4.0, 4.0, 2.0, 2.0, 2.0, 4.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0, 2.0,
                 2.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 1.0, 1.0, 1.0, 2.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                 2.0, 0.5, 0.5, 0.5, 0.5, 2.0, 2.0, 0.5, 0.5, 0.5, 2.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                 1.0, 0.5, 0.5, 0.5, 0.5, 1.0, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                 1.0, 0.3, 0.3, 0.3, 0.3, 1.0, 1.0, 0.3, 0.3, 0.3, 1.0, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3,
                 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
                 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
                 1.5, 0.5, 0.5, 0.5, 0.5, 1.5, 1.5, 0.5, 0.5, 0.5, 3.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5)

  srows <- length(suggested)/19
  solutions[1:srows,] <- matrix(nrow=srows, ncol=19, byrow=T, suggested)

  ga("real-valued", fitness=testDailyPlanetsOrbs, names=varnames,
     monitor=gaMonitor, maxiter=100, run=30, popSize=100, pcrossover = 0.7, pmutation = 0.3,
     min=minvals, max=maxvals, selection=gareal_rwSelection, suggestions=solutions)

  sink()
}

testDailyPlanetsOrbsSolution <- function(planetsdir, fileno, securitydir, securityfile,
                                         sdate, edate, chartfile, cusorbs, resplot=F) {
  # currency data
  currency <- openSecurity(paste("~/trading/", securitydir, "/", securityfile, ".csv", sep=''))
  chart <- fread(npath(paste("~/trading/charts/", chartfile, '.tsv', sep='')), sep="\t", na.strings="")
  cusorbs <- round(cusorbs, digits=2)
  planets <- openPlanets(paste("~/trading/", planetsdir, "/planets_", fileno, ".tsv", sep=''), chart, cusorbs)
  # process planets
  planets.sp <- processPlanets(planets, currency, sdate, edate)
  # process analysis
  fitness <- plotGridAspectsEffect(paste(securityfile, chartfile, sdate, edate, paste(cusorbs, collapse='-')), planets.sp$train, 30, resplot)
  # binarize
  selcols <- names(fitness)
  planets.sp$train[, c(selcols) := lapply(.SD, function(x) ifelse(is.na(x), 0, 1)), .SDcols=selcols]
  planets.sp$test[, c(selcols) := lapply(.SD, function(x) ifelse(is.na(x), 0, 1)), .SDcols=selcols]

  cat("###", length(fitness), " / %%% =", median(unlist(fitness)), "\n")
  cat("Cols:", names(fitness), "\n\n")
  return(list(selcols=names(fitness), data=planets.sp))
}

testZodDegAspectsGA <- function(sinkfile) {
  if (!hasArg('sinkfile')) stop("Provide a sink filename.")
  sinkfile <- paste("~/trading/predict/", sinkfile, ".txt", sep='')
  sink(npath(sinkfile), append=TRUE)

  currency <- openSecurity("~/trading/currency/EURUSD_fxpro.csv")
  testZodDegAspectsFitness <- function(string) {
    inc1 <- which(string[1:12] == 1)
    inc2 <- which(string[13:32] == 1)
    selAspects <- aspects[inc1]
    selPlanetsLonCols <- planetsLonCols[inc2]
    planetsGridZodCols <- as.character(apply(expand.grid(selPlanetsLonCols, zodDegrees), 1, function(x) paste(x[1], '_', as.numeric(x[2]), sep='')))
    res <- openPlanetsZod("~/trading/dplanets/planets_2.tsv", currency, "2000-01-01", "2011-01-01", 0.40, selPlanetsLonCols, planetsGridZodCols, selAspects)
    ds <- merge(res$test, res$sig, by='idx')
    ds <- ds[count > quantile(ds$count)[2]]
    ds.sum <- ds[, list(sum(down.y), sum(up.y)), by='Date']
    ds.sum <- merge(ds.sum, currency, by='Date')
    ds.sum[, predEff := ifelse(V1 > V2, 'down', 'up')]
    t1 <- table(ds.sum$Eff==ds.sum$predEff)
    fitness <- abs(t1['TRUE']-t1[['FALSE']])
    cat(paste(shQuote(selAspects), collapse=","), "\n")
    cat(paste(shQuote(selPlanetsLonCols), collapse=","), "\n")
    cat("### = ", fitness, "\n")
    return(fitness)
  }

  ga("binary", fitness=testZodDegAspectsFitness, names=as.character(aspects), nBits=length(aspects),
     monitor=gaMonitor, maxiter=500, run=50, popSize=100,
     selection=gabin_rwSelection)

  sink()
}

testPlanetsSignificanceGA <- function(sinkfile, securitydir, securityfile, planetsfile, execfunc, ...) {
  if (!hasArg('sinkfile')) stop("Provide a sink filename.")
  if (!hasArg('execfunc')) stop("Provide a GA function to execute")
  sinkfile <- paste("~/trading/predict/", sinkfile, ".txt", sep='')
  sink(npath(sinkfile), append=TRUE)
  ptm <- proc.time()

  aspects = c(0, 30, 45, 60, 90, 120, 135, 150, 180)

  orbs = list(SULON = c(1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5),
              MOLON = c(4.0, 2.0, 2.0, 2.0, 4.0, 2.0, 2.0, 2.0, 2.0),
              MELON = c(1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5),
              VELON = c(1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5),
              MALON = c(1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5),
              JULON = c(1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5),
              SALON = c(1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5),
              URLON = c(1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5),
              NELON = c(1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5),
              PLLON = c(1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5),
              NNLON = c(1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5),
              SNLON = c(1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 0.5))

  tsdate <- '1999-01-01'
  tedate <- '2011-01-01'
  vsdate <- '2012-01-01'
  vedate <- '2013-05-01'
  planetsLonGCols = c('SULONG', 'MOLONG', 'MELONG', 'VELONG', 'MALONG', 'JULONG', 'SALONG', 'URLONG', 'NELONG', 'PLLONG', 'NNLONG')
  planetsLonCols <- paste(c("SU", "MO", "ME", "VE", "MA", "JU", "SA", "UR", "NE", "PL", "NN"), 'LON', sep="")
  planetsSpCols <- paste(c("SU", "MO", "ME", "VE", "MA", "JU", "SA", "UR", "NE", "PL"), 'SP', sep="")
  planetsLonCols2 <- paste(c("SU", "MO", "ME", "VE", "MA", "JU", "SA", "UR", "NE", "PL", "NN"), 'LON', sep="")
  currency <- openSecurity(paste("~/trading/", securitydir, "/", securityfile, ".csv", sep=""))
  planetsCombLon <- combn(planetsLonCols2, 2, simplify=F)
  planetsCombLonCols <- as.character(lapply(planetsCombLon, function(x) paste(x[1], x[2], sep='')))
  planets <- openPlanets(paste("~/trading/dplanets/", planetsfile, ".tsv", sep=""), orbs, aspects, 2, 50)
  significance <- planetsVarsSignificance(planets[Date >= as.Date(tsdate) & Date < as.Date(tedate)], currency, 0.10)
  setkey(significance, 'key', 'variable', 'V3', 'V4')
  keyranges <- mixedsort(unique(significance[variable %in% planetsLonGCols]$key))

  testPlanetsAnalogyFitness <- function(string) {
    looptm <- proc.time()
    panalogy <- list(SULONG = planetsLonGCols[which(string[1  :11 ] == 1)],
                     MOLONG = planetsLonGCols[which(string[12 :22 ] == 1)],
                     MELONG = planetsLonGCols[which(string[23 :33 ] == 1)],
                     VELONG = planetsLonGCols[which(string[34 :44 ] == 1)],
                     MALONG = planetsLonGCols[which(string[45 :55 ] == 1)],
                     JULONG = planetsLonGCols[which(string[56 :66 ] == 1)],
                     SALONG = planetsLonGCols[which(string[67 :77 ] == 1)],
                     URLONG = planetsLonGCols[which(string[78 :88 ] == 1)],
                     NELONG = planetsLonGCols[which(string[89 :99 ] == 1)],
                     PLLONG = planetsLonGCols[which(string[100:110] == 1)],
                     NNLONG = planetsLonGCols[which(string[111:121] == 1)])

    planets.train <- data.table(planets[Date >= as.Date(tsdate) & Date <= as.Date(tedate)])
    predEff <- apply(planets.train, 1, function(x) planetsDaySignificance(x, significance, panalogy, T, F))
    planets.train <- cbind(planets.train, predEff=predEff)
    fitness1 <- predEffProcess(planets.train, list(panalogy), looptm)

    planets.test <- data.table(planets[Date >= as.Date(vsdate) & Date <= as.Date(vedate)])
    predEff2 <- apply(planets.test, 1, function(x) planetsDaySignificance(x, significance, panalogy, T, F))
    planets.test <- cbind(planets.test, predEff=predEff2)
    fitness2 <- predEffProcess(planets.test, NA, looptm)

    cat("\n=================================================================================\n")
    rm(panalogy, predEff, predEff2, planets.train, planets.test)
    gc()
    return(fitness1)
  }

  weightSignificance <- function(significance, krweights) {
    names(krweights) <- keyranges
    krweights <- as.list(krweights)
    # clone the significance table to weight on it preserving the original
    significance.w <- data.table(significance)
    setkey(significance.w, 'key', 'variable', 'V3', 'V4')

    # alter the significance weights
    for (keyrange in keyranges) {
      significance.w[key == keyrange, c('V3', 'V4') := list(V3 * krweights[[keyrange]], V4 * krweights[[keyrange]])]
    }

    return(significance.w)
  }

  testDegreesWeightFitness <- function(krweights) {
    looptm <- proc.time()
    panalogy <- solutionAnalogyEURUSD()
    krweights <- round(krweights, digits=3)
    significance.w <- weightSignificance(significance, krweights)
    planets.test <- data.table(planets[Date >= as.Date('2011-01-01') & Date <= as.Date('2013-04-01')])
    predEff <- apply(planets.test, 1, function(x) planetsDaySignificance(x, significance.w, panalogy, T, F))
    planets.test <- cbind(planets.test, predEff=predEff)
    fitness <- predEffProcess(planets.test, list(panalogy, krweights), looptm)
    rm(significance.w, planets.test)
    gc()
    return(fitness)
  }

  predEffProcess <- function(planets.test, optvariables, looptm) {
    planets.test <- merge(planets.test, currency, by='Date')
    t1 <- table(planets.test$Eff == planets.test$predEff)
    fitness <- t1['TRUE']-t1[['FALSE']]
    if (class(optvariables) == 'list') {
      for (optvariable in optvariables) {
        dput(optvariable)
      }
    }
    cat("securitydir=", shQuote(securitydir), ", securityfile=", shQuote(securityfile), ", planetsfile=", shQuote(planetsfile), "\n", sep="")
    print(t1)
    cat("\t Predict execution/loop time: ", proc.time()-ptm, " - ", proc.time()-looptm, "\n")
    cat("### = ", fitness, "\n")
    # garbage
    rm(planets.test, t1)
    return(fitness)
  }

  gaPlanetsAnalogy <- function() {
    ga("binary", fitness=testPlanetsAnalogyFitness, nBits=121,
       monitor=gaMonitor, maxiter=200, run=50, popSize=100, pcrossover = 0.6, pmutation = 0.3,
       selection=gabin_rwSelection, crossover=gabin_spCrossover)
  }

  gaDegreesWeight <- function() {
    panalogy <- solutionAnalogyEURUSD2()
    minvals <- rep(0, 180)
    maxvals <- rep(2, 180)
    solutions <- matrix(NA, nrow = 100, ncol = length(minvals))

    for (j in 1:length(minvals)) {
      solutions[,j] <- runif(100, minvals[j], maxvals[j])
    }

    suggested <- c(rep(1, 180), solutionWeigthEURUSD2())
    srows <- length(suggested)/180
    solutions[1:srows,] <- matrix(nrow=srows, ncol=180, byrow=T, suggested)

    ga("real-valued", fitness=testDegreesWeightFitness, names=keyranges,
       monitor=gaMonitor, maxiter=200, run=50, popSize=100, pcrossover = 0.7, pmutation = 0.2,
       min=minvals, max=maxvals, selection=gareal_rwSelection, suggestions=solutions)
  }

  testPredictAnalogy <- function(sdate, edate, verbose=F) {
    looptm <- proc.time()
    panalogy <- solutionAnalogyEURUSD3()
    planets.test <- data.table(planets[Date >= as.Date(sdate) & Date <= as.Date(edate)])
    setkey(planets.test, 'Date')
    predEff <- apply(planets.test, 1, function(x) planetsDaySignificance(x, significance, panalogy, T, verbose))
    planets.test <- cbind(planets.test, predEff=predEff)
    fitness <- predEffProcess(planets.test, list(panalogy), looptm)
  }

  testPredictAnalogyWeigths <- function(sdate, edate, verbose=F) {
    looptm <- proc.time()
    panalogy <- solutionAnalogyEURUSD2()
    krweights <- solutionWeigthEURUSD2()
    significance.w <- weightSignificance(significance, krweights)
    planets.test <- data.table(planets[Date >= as.Date(sdate) & Date <= as.Date(edate)])
    setkey(planets.test, 'Date')
    predEff <- apply(planets.test, 1, function(x) planetsDaySignificance(x, significance, panalogy, T, verbose))
    planets.test <- cbind(planets.test, predEff=predEff)
    fitness <- predEffProcess(planets.test, list(panalogy, krweights), looptm)
  }

  generatePredict <- function(sdate, edate, predfile, verbose=T) {
    if (!hasArg('predfile')) stop("Provide a predfile filename.")
    looptm <- proc.time()
    panalogy <- solutionAnalogyEURUSD()
    planets.test <- data.table(planets[Date >= as.Date(sdate) & Date <= as.Date(edate)])
    setkey(planets.test, 'Date')
    predEff <- apply(planets.test, 1, function(x) planetsDaySignificance(x, significance, panalogy, F, verbose))
    planets.test <- cbind(planets.test, predEff=predEff)
    write.csv(planets.test[, c('Date', 'predEff'), with=F], file=paste("~/trading/predict/", predfile, ".csv", sep=''), eol="\r\n", quote=FALSE, row.names=FALSE)
  }

  optimizeSignificance <- function() {
    thresholds <- seq(0, 1, by=0.05)
    krweights <- solutionWeigthEURUSD()
    panology <- solutionAnalogyEURUSD()
    for (threshold in thresholds) {
      looptm <- proc.time()
      significance <- planetsVarsSignificance(planets[Date > as.Date('1999-01-01') & Date < as.Date('2011-01-01')], currency, threshold)
      significance.w <- weightSignificance(significance, krweights)
      planets.test <- data.table(planets[Date >= as.Date('2011-01-01') & Date <= as.Date('2013-04-01')])
      predEff <- apply(planets.test, 1, function(x) planetsDaySignificance(x, significance.w, panalogy, T, F))
      planets.test <- cbind(planets.test, predEff=predEff)
      fitness <- predEffProcess(planets.test, list(threshold), looptm)
      rm(significance, significance.w, planets.test, predEff)
      gc()
    }
  }

  solutionWeigthEURUSD <- function() {
    solution <- c(1.053,  1.014,  0.991,  0.832,  1.075,  1.136,  1.162,  0.876,  1.075,  0.839,  0.962,  1.19,  1.044,  0.83,  0.605,  0.993,  0.79,  1.248,  1.379,  1.161,  1.128,  1.164,  0.885,  0.969,  1.049,  0.812,  1.184,  0.908,  1.25,  1.268,  0.821,  0.919,  1.163,  1.004,  0.743,  1.251,  0.785,  0.912,  1.202,  1.347,  0.976,  1.032,  1.076,  0.792,  1.162,  1.183,  0.579,  1.077,  0.942,  0.707,  0.925,  1.241,  0.975,  1.141,  1.338,  1.304,  1.316,  0.858,  0.89,  1.08,  0.786,  1.132,  0.859,  0.728,  1.064,  1.214,  1.117,  0.855,  1.138,  0.979,  0.981,  0.912,  0.838,  1.001,  1.023,  0.852,  0.937,  0.878,  1.122,  1.146,  0.892,  0.793,  1.137,  0.97,  1.114,  1.156,  1.079,  1.209,  0.839,  1.048,  0.917,  1.044,  0.713,  1.111,  1.083,  0.911,  1.321,  0.861,  0.696,  1.249,  0.827,  1.151,  1.014,  1.031,  0.796,  0.98,  1.315,  0.909,  1.131,  0.788,  1.285,  0.927,  1.321,  0.718,  1.146,  0.881,  1.101,  0.933,  0.635,  1.151,  0.835,  1.328,  1.29,  0.972,  1.104,  1.06,  1.013,  0.863,  1.127,  0.925,  1.059,  0.979,  1.169,  1.045,  1.171,  1.086,  1.133,  1.081,  1.029,  1.211,  0.787,  0.704,  1.062,  1.015,  1.063,  0.824,  1.011,  0.693,  1.142,  0.964,  0.938,  0.964,  1.151,  1.4,  0.594,  0.828,  1.091,  0.962,  0.781,  0.668,  1.022,  1.192,  1.272,  0.754,  1.185,  1.039,  1.023,  1.264,  1.378,  1.106,  0.89,  0.889,  1.23,  0.965,  0.753,  1.059,  1.309,  1.115,  0.914,  1.186)
    return(solution)
  }

  solutionWeigthEURUSD2 <- function() {
    weights <- list( 0.98, 1, 1.006, 0.914, 1.096, 1.052, 1.059, 0.841, 1.033, 1.139, 0.942, 1.072, 1.062, 0.853, 1.016, 0.916, 1.03, 1.21, 0.973, 1.194, 1.095, 0.865, 0.972, 0.842, 1.083, 0.841, 1.147, 0.888, 1.092, 1.156, 0.9, 1.037, 1.098, 1.079, 0.941, 1.254, 0.736, 0.96, 1.178, 1.059, 1.088, 1.086, 0.922, 0.945, 0.981, 1.152, 0.831, 1.044, 1.015, 0.783, 1.005, 1.173, 1.1, 0.863, 1.109, 1.181, 1.302, 1.06, 0.878, 0.999, 0.847, 1.19, 0.925, 0.957, 1.145, 1.173, 0.938, 1.002, 1.07, 0.872, 0.946, 1.001, 0.955, 1.043, 1.001, 0.886, 0.877, 1, 1.098, 1.031, 0.848, 0.972, 0.91, 0.909, 0.947, 1.008, 1.05, 1.147, 0.934, 0.981, 0.972, 1.006, 0.932, 1.021, 1.04, 0.997, 1.088, 0.99, 0.886, 1.14, 0.853, 1.201, 1.042, 1.033, 0.752, 1.374, 1.123, 0.816, 1.104, 0.766, 1.257, 0.936, 1.009, 0.954, 1.057, 0.99, 1.156, 1.012, 0.867, 1.091, 0.969, 1.16, 1.157, 1.066, 1.098, 1.222, 1.115, 0.907, 1.175, 1.006, 1.013, 0.912, 1.08, 0.977, 1.126, 1.02, 1.078, 1.093, 0.982, 1.039, 0.965, 0.835, 0.991, 1.084, 1.04, 1.015, 0.993, 0.761, 1.043, 1.044, 0.902, 0.995, 1.077, 0.83, 0.993, 0.947, 1.013, 0.948, 0.915, 0.83, 0.987, 1.193, 1.14, 0.795, 1.208, 1.061, 1.145, 1.092, 1.259, 1.222, 0.947, 0.916, 1.149, 0.908, 0.917, 1.06, 1.127, 1.122, 0.922, 1.075)
    return(weights)
  }

  solutionAnalogyEURUSD <- function() {
    panalogy <- list(SULONG = c("SULONG", "MOLONG", "MALONG", "SALONG", "URLONG", "NNLONG"),
                     MOLONG = c("MOLONG", "MELONG", "MALONG", "SALONG", "URLONG", "SNLONG"),
                     MELONG = c("VELONG", "JULONG", "SALONG"),
                     VELONG = c("SULONG", "MOLONG", "MELONG", "JULONG", "PLLONG", "NNLONG"),
                     MALONG = c("MOLONG", "MELONG", "MALONG", "NNLONG", "SNLONG"),
                     JULONG = c("MOLONG", "VELONG", "MALONG", "URLONG", "PLLONG"),
                     SALONG = c("SULONG", "MELONG", "VELONG", "JULONG", "SALONG", "NELONG", "NNLONG", "SNLONG"),
                     URLONG = c("MOLONG", "VELONG", "SALONG", "PLLONG"),
                     NELONG = c("MOLONG", "MALONG", "JULONG", "NNLONG", "SNLONG"),
                     PLLONG = c("VELONG", "SALONG", "PLLONG"),
                     NNLONG = c("MELONG", "VELONG", "JULONG"))
    return(panalogy)
  }

  solutionAnalogyEURUSD2 <- function() {
    panalogy <- list(SULONG = c("SULONG", "MOLONG", "MALONG", "SALONG", "URLONG", "NELONG", "NNLONG"), MOLONG = c("MOLONG", "MELONG", "MALONG", "SALONG", "URLONG", "NELONG", "SNLONG"), MELONG = c("VELONG", "JULONG", "SALONG"), VELONG = c("SULONG", "MOLONG", "JULONG", "NNLONG"), MALONG = c("MOLONG", "VELONG", "MALONG", "NNLONG", "SNLONG"), JULONG = c("MOLONG", "VELONG", "MALONG", "URLONG", "PLLONG"), SALONG = c("MELONG", "VELONG", "JULONG", "SALONG", "NELONG"), URLONG = c("MOLONG", "VELONG", "SALONG", "URLONG", "PLLONG"), NELONG = c("MOLONG", "MALONG", "JULONG", "PLLONG", "NNLONG", "SNLONG"), PLLONG = c("VELONG", "SALONG", "PLLONG"), NNLONG = c("MELONG", "VELONG", "MALONG", "JULONG", "URLONG", "NNLONG"))
    return(panalogy)
  }

  solutionAnalogyEURUSD3 <- function() {
    panalogy <- list(SULONG = c("SULONG", "MALONG", "JULONG", "SALONG", "NELONG", "NNLONG", "SNLONG"), MOLONG = c("SULONG", "MOLONG", "SALONG", "URLONG", "NNLONG", "SNLONG"), MELONG = c("VELONG", "JULONG", "URLONG", "PLLONG", "NNLONG"), VELONG = c("SULONG", "NELONG", "PLLONG", "SNLONG"), MALONG = c("MOLONG", "JULONG", "URLONG", "NNLONG"), JULONG = c("MOLONG", "MELONG", "VELONG", "MALONG", "URLONG", "SNLONG"), SALONG = c("MOLONG", "JULONG", "URLONG", "NELONG", "PLLONG"), URLONG = c("JULONG", "NELONG", "PLLONG"), NELONG = c("SULONG", "MOLONG", "VELONG", "MALONG", "JULONG", "SALONG", "SNLONG"), PLLONG = c("SULONG", "MOLONG", "VELONG", "MALONG", "JULONG", "PLLONG"), NNLONG = c("SULONG", "MOLONG", "MELONG", "VELONG", "MALONG", "JULONG", "SNLONG"))
    return(panalogy)
  }

  execfunc <- get(get('execfunc'))
  execfunc(...)

  sink()
}

testPlanetsSignificanceRelative <- function(execfunc, sinkfile, ...) {
  if (hasArg('sinkfile')) sink(npath(paste("~/trading/predict/", sinkfile, ".txt", sep='')), append=T)
  if (!hasArg('execfunc')) stop("Provide function to execute")
  if (!hasArg('dateformat')) stop("A dateformat is needed.")
  ptm <- proc.time()
  planetsLonGCols = c('SULONG', 'MOLONG', 'MELONG', 'VELONG', 'MALONG', 'JULONG', 'SALONG', 'URLONG', 'NELONG', 'PLLONG', 'NNLONG')

  relativeTrend <- function(securityfile, planetsfile, tsdate, tedate, vsdate, vedate, csdate, cedate, iprev, inext,
                            mapredslow, maprice, mapredtype, mapricetype, sigtype, predtype, cordir, degsplit, spsplit,
                            threshold, uselon, usesp, useasp, energymode, energyweight, dateformat, alignmove=0, pricetype,
                            pricemadir, verbose=F) {
    looptm <- proc.time()
    mapricefunc <- get(get('mapricetype'))
    mapredfunc <- get(get('mapredtype'))
    planets <- openPlanets(paste("~/trading/dplanets/", planetsfile, ".tsv", sep=""), orbs, aspects, degsplit, spsplit)
    setkey(planets, 'Date')
    security <- openSecurity(paste("~/trading/", securityfile, ".csv", sep=''), mapricetype, maprice, dateformat, pricetype, pricemadir)
    significance <- planetsVarsSignificance(planets[Date >= as.Date(tsdate) & Date <= as.Date(tedate)], security, threshold)
    keyranges <<- mixedsort(unique(significance[variable %in% planetsLonGCols]$key))
    setkey(significance, 'key', 'variable', 'V3', 'V4')

    panalogy <- list(SULONG = c("SULONG"),
                     MOLONG = c("nouse"),
                     MELONG = c("MELONG"),
                     VELONG = c("VELONG"),
                     MALONG = c("MALONG"),
                     JULONG = c("JULONG"),
                     SALONG = c("SALONG"),
                     URLONG = c("URLONG"),
                     NELONG = c("NELONG"),
                     PLLONG = c("PLLONG"),
                     NNLONG = c("NNLONG"))

    planets[, wday := format(Date, "%w")]
    pltitle <- paste(securityfile, "maprice=", maprice, "mapricetype=", mapricetype, "mapredslow=", mapredslow, "mapredtype=", mapredtype,
                     "iprev=", iprev, "inext=", inext, "\nsigtype=", sigtype, "predtype=", predtype, "degsplit=", degsplit, "spsplit=", spsplit,
                     "threshold=", threshold, "energymode=", energymode, "energyweight=", energyweight, "\nuselon=", uselon, "usesp=", usesp,
                     "useasp=", useasp, "alignmove=", alignmove, "pricetype=", pricetype, "pricemadir=", pricemadir)
    planets.test <- planets[Date > as.Date(vsdate) & Date <= as.Date(vedate) & wday %in% c(1, 2, 3, 4, 5)]
    fitness <- list()
    volatility <- list()
    correlation <- list()

    # compute predictions by year an calculate fitness by the mean to meter the solution stability
    for (curyear in unique(planets.test$Year)) {
      res <- processPredictions(planets.test=planets.test[Year == curyear], security=security, significance=significance, panalogy=panalogy,
                                 iprev=iprev, inext=inext, sigtype=sigtype, predtype=predtype, mapredfunc=mapredfunc, mapredslow=mapredslow,
                                 cordir=cordir, pltitle=pltitle, uselon=uselon, usesp=usesp, useasp=useasp, energymode=energymode,
                                 energyweight=energyweight, alignmove=alignmove, verbose=verbose)
      fitness[[length(fitness)+1]] <- res$fitness
      volatility[[length(volatility)+1]] <- res$volatility
      correlation[[length(correlation)+1]] <- res$correlation
    }

    fitness <- unlist(fitness)
    meanfitness <- round(mean(fitness))
    meansdfitness <- meanfitness * (1 - sd(fitness) / meanfitness)
    avgvolatility <- mean(unlist(volatility))
    avgcorrelation <- mean(unlist(correlation))

    planets.test2 <- planets[Date > as.Date(csdate) & Date <= as.Date(cedate) & wday %in% c(1, 2, 3, 4, 5)]
    cat("\n--- Confirmation tests ---\n")

    for (curyear in unique(planets.test2$Year)) {
      res2 <- processPredictions(planets.test=planets.test2[Year == curyear], security=security, significance=significance, panalogy=panalogy,
                                 iprev=iprev, inext=inext, sigtype=sigtype, predtype=predtype, mapredfunc=mapredfunc, mapredslow=mapredslow,
                                 cordir=cordir, pltitle=pltitle, uselon=uselon, usesp=usesp, useasp=useasp, energymode=energymode,
                                 energyweight=energyweight, alignmove=alignmove, verbose=verbose)
      cat("\nconfirmation test: volatility =", res2$volatility, " - correlation =", res2$correlation, " - fitness =", res2$fitness, "\n")
    }

    cat("\n\t Predict execution/loop time: ", proc.time()-ptm, " - ", proc.time()-looptm, "\n")
    cat("volatility =", avgvolatility, " - correlation =", avgcorrelation, " - meanfitness =", meanfitness, " - ### = ", meansdfitness, "\n")
    return(list(fitness=meanfitness, planets=res2$planets, security=security))
  }

  processPredictions <- function(planets.test, security, significance, panalogy, iprev, inext, sigtype, predtype,
                                 mapredfunc, mapredslow, cordir, pltitle, uselon, usesp, useasp, energymode, energyweight, alignmove, verbose) {
    predEff <- apply(planets.test, 1, function(x)
                     planetsDaySignificance(x, significance, panalogy, F, verbose, iprev, inext, sigtype, uselon,
                                            usesp, useasp, energymode, energyweight))

    # in case that all predictions are 0 we skip this solution
    if (all(predEff == 0)) {
      # very bad fitness to make this solution disappear
      fitness <- 0
      correlation <- 0
      volatility <- 0
    }
    else {
      # add raw prediction to data table
      planets.test[, predraw := predEff]
      # normalize data
      predEff <- data.Normalization(predEff, type="n3")

      if (all(is.nan(predEff))) {
        fitness <- 0
        correlation <- 0
        volatility <- 0
      }
      else {
        # negative correlation
        if (cordir == 1) {
          predEff <- predEff * -1
        }

        #planets.test[, predval := predEff]
        predEffSmoth <- mapredfunc(predEff, mapredslow)

        if (alignmove != 0) {
          # align prediction
          predEffSmoth <- c(predEffSmoth[alignmove:length(predEffSmoth)], rep(NA, alignmove-1))
        }

        planets.test[, predval := predEffSmoth]
        planets.test <- planets.test[!is.na(predval)]

        if (predtype == 'absolute') {
          planets.test[, predEff := predval]
        }
        else if (predtype == 'relative') {
          planets.test[, predEff := c(NA, diff(predval, lag=1, differences=1))]
          planets.test <- planets.test[!is.na(predEff)]
        }
        else {
          stop("No valid prediction type was provided.")
        }

        planets.test[, predFactor := cut(predEff, c(-10, 0, 10), labels=c('down', 'up'), right=FALSE)]
        planets.test.security <- merge(planets.test, security, by='Date')
        volatility <- mean(planets.test.security$Mid) / sd(planets.test.security$Mid)
        planets.test.security[, 'Mid' := data.Normalization(Mid, type="n3")]
        planets.test.security[, 'MidMAF' := data.Normalization(MidMAF, type="n3")]
        planets.test.security[, 'MidMAS' := data.Normalization(MidMAS, type="n3")]
        interval <- abs(as.integer((min(planets.test$Date)-max(planets.test$Date))/80))
        x_dates <- seq(min(planets.test$Date), max(planets.test$Date), by=interval)

        if (nrow(planets.test.security) == 0) {
          p1 <- ggplot(planets.test, aes(Date, predval)) + geom_line() + theme(axis.text.x = element_text(angle = 90, size = 7)) + ggtitle(pltitle) + scale_fill_grey() + scale_shape_identity() + scale_x_date(breaks=x_dates)
          correlation <- NA
        }
        else {
          p1 <- ggplot(planets.test, aes(Date, predval)) + geom_line() + geom_line(data = planets.test.security, aes(Date, Mid), colour="red", show_guide=F) + geom_line(data = planets.test.security, aes(Date, MidMAF), colour="blue", show_guide=F) + geom_line(data = planets.test.security, aes(Date, MidMAS), colour="green", show_guide=F) + theme(axis.text.x = element_text(angle = 90, size = 7)) + ggtitle(pltitle) + scale_fill_grey() + scale_shape_identity() + scale_x_date(breaks=x_dates)
          correlation <- round(cor(planets.test.security$predval, planets.test.security$Mid,  use = "complete.obs", method='spearman'), digits=2)
        }

        print(p1)
        t1 <- with(planets.test.security, table(Eff==predFactor))
        print(t1)

        if (all(c('TRUE', 'FALSE') %in% names(t1))) {
          fitness <- t1[['TRUE']]-t1[['FALSE']]
        }
        else if ('TRUE' %in% names(t1)) {
          fitness <- t1[['TRUE']]
        }
        else if ('FALSE' %in% names(t1)) {
          fitness <- -t1[['FALSE']]
        }
        else {
          fitness <- NA
        }
      }
    }

    return(list(fitness=fitness, correlation=correlation, volatility=volatility, planets=planets.test))
  }

  testSolution <- function(predfile, ...) {
    pdf(paste("~/chart_", predfile, ".pdf", sep=""), width = 11, height = 8, family='Helvetica', pointsize=12)
    res <- relativeTrend(...)
    planets.security <- merge(res$planets, res$security, by='Date')

    if (list(...)$verbose) {
      cols <- c('Date', 'Open', 'Close', 'Mid', 'MidMAF', 'MidMAS', 'val', 'predraw', 'predval', 'predEff', 'predFactor', 'Eff')
      print(as.data.frame(planets.security[, cols, with=F]))
    }

    dev.off()
    write.csv(res$planets[, c('DateMT4', 'predval'), with=F], file=paste("~/trading/predict/", predfile, ".csv", sep=''), eol="\r\n", quote=FALSE, row.names=FALSE)
  }

  relativeTrendFitness <- function(x, securityfile, planetsfile, tsdate, tedate, vsdate, vedate, csdate, cedate, dateformat) {
    # build the parameters based on GA indexes
    mapricetypes <- c('SMA', 'EMA', 'WMA', 'ZLEMA')
    sigtypes <- c('count',  'percent')
    predtypes <- c('absolute',  'relative')
    pricetypes <- c('averages',  'daily', 'priceaverage')
    iprev <- x[1]
    inext <- x[2]
    mapredslow <- x[3]
    maprice <- x[4]
    mapredtype <- mapricetypes[[x[5]]]
    mapricetype <- mapricetypes[[x[6]]]
    sigtype <- sigtypes[[x[7]]]
    predtype <- predtypes[[x[8]]]
    cordir <- x[9]
    degsplit <- x[10]
    spsplit <- x[11]
    threshold <- x[12]/100
    uselon <- x[13]
    usesp <- x[14]
    useasp <- x[15]
    energymode <- x[16]
    energyweight <- x[17]
    alignmove <- x[18]
    pricetype <- pricetypes[[x[19]]]
    pricemadir <- x[20]

    cat("\n---------------------------------------------------------------------------------\n")
    cat("testPlanetsSignificanceRelative('testSolution', securityfile=", shQuote(securityfile), ", planetsfile=", shQuote(planetsfile), sep="")
    cat(", tsdate=", shQuote(tsdate), ", tedate=", shQuote(tedate), ", vsdate=", shQuote(vsdate), ", vedate=", shQuote(vedate), sep="")
    cat(", csdate=", shQuote(csdate), ", cedate=", shQuote(cedate), sep="")
    cat(", iprev=", iprev, ", inext=", inext, ", mapredslow=", mapredslow, ", maprice=", maprice, sep="")
    cat(", mapredtype=", shQuote(mapredtype), ", mapricetype=", shQuote(mapricetype), ", sigtype=", shQuote(sigtype), sep="")
    cat(", predtype=", shQuote(predtype), ", cordir=", cordir, ", degsplit=", degsplit, ", spsplit=", spsplit, ", threshold=", threshold, sep="")
    cat(", uselon=", uselon, ", usesp=", usesp, ", useasp=", useasp, ", energymode=", energymode, ", energyweight=", energyweight, sep="")
    cat(", alignmove=", alignmove, ", pricetype=", shQuote(pricetype), ", dateformat=", shQuote(dateformat), ", verbose=F", sep="")
    cat(", pricemadir=", pricemadir, ")\n", sep="")
    res <- relativeTrend(securityfile=securityfile, planetsfile=planetsfile, tsdate=tsdate, tedate=tedate, vsdate=vsdate, vedate=vedate,
                         csdate=csdate, cedate=cedate, iprev=iprev, inext=inext, mapredslow=mapredslow, maprice=maprice, mapredtype=mapredtype,
                         mapricetype=mapricetype, sigtype=sigtype, predtype=predtype, cordir=cordir, degsplit=degsplit, spsplit=spsplit, threshold=threshold,
                         uselon=uselon, usesp=usesp, useasp=useasp, energymode=energymode, energyweight=energyweight, dateformat=dateformat,
                         alignmove=alignmove, pricetype=pricetype, pricemadir=pricemadir, verbose=F)

    return(res$fitness)
  }

  optimizeRelativeTrend <- function(securityfile, planetsfile, tsdate, tedate, vsdate, vedate, csdate, cedate, dateformat) {
    pdf(paste("~/chart_", securityfile, "_", planetsfile, "_", vsdate, "-", vedate, ".pdf", sep=""), width = 11, height = 8, family='Helvetica', pointsize=12)
    minvals <- c(0, 0,  2,  2, 1, 1, 1, 1, 0, 1,  2,  0, 1, 0, 0, 0,  0,  0, 1, 1)
    maxvals <- c(1, 1, 10, 20, 4, 4, 1, 2, 1, 3, 70, 30, 1, 0, 0, 9, 11, 20, 3, 4)
    varnames <- c('iprev', 'inext', 'mapredslow', 'maprice', 'mapredtype', 'mapricetype', 'sigtype', 'predtype', 'cordir',
                  'degsplit', 'spsplit', 'threshold', 'uselon', 'usesp', 'useasp', 'energymode', 'energyweight', 'alignmove',
                  'pricetype', 'pricemadir')

    ga("real-valued", fitness=relativeTrendFitness, names=varnames,
       monitor=gaMonitor, maxiter=200, run=50, popSize=400, min=minvals, max=maxvals, pcrossover = 0.7, pmutation = 0.2,
       selection=gaint_rwSelection, mutation=gaint_raMutation, crossover=gaint_blxCrossover, population=gaint_Population,
       securityfile=securityfile, planetsfile=planetsfile, tsdate=tsdate, tedate=tedate, vsdate=vsdate, vedate=vedate,
       csdate=csdate, cedate=cedate, dateformat)

    dev.off()
  }

  execfunc <- get(get('execfunc'))
  execfunc(...)

  #planets.security <- merge(planets, security, by='Date')
  #planets.security <- subset(planets.security, !is.na(Eff) & Date >= as.Date(tsdate) & Date <= as.Date(tedate))
  #pdf(npath(paste("~/", plotfile, "_SMA", maprice, ".pdf", sep='')), width = 11, height = 8, family='Helvetica', pointsize=12)
  #for (curcol in planetsLonGCols) {
  #  p1 <- ggplot(aes_string(x=curcol, fill="Eff"), data=planets.security) + geom_bar(position="fill") + theme(axis.text.x = element_text(angle = 90, size = 5)) + xlab(curcol)  + ggtitle(paste("Significance Planets LONG groups SMA", maprice)) + geom_hline(yintercept=seq(0, 1, by=0.1)) + scale_fill_grey()
  #  #p1 <- qplot(x=get(curcol), y=val2, geom='boxplot', data=planets.security) + theme(axis.text.x = element_text(angle = 85, size = 7)) + xlab(curcol) + ggtitle(paste("Significance Planets LONG groups SMA", maprice))
  #  print(p1)
  #}
  #dev.off()
  if (hasArg('sinkfile')) sink()
}

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
