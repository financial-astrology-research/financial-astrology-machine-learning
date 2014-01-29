library(GA)
library(compiler)
library(data.table)
library(ggplot2)
library(gtools)
library(microbenchmark)
library(plyr)
library(quantmod)
library(randomForest)
library(reshape2)
library(rpart)
library(RSQLite)
library(splus2R)
library(timeDate)
library(xts)
# no scientific notation
options(scipen=100)
options(width=130)
options(error=recover)
enableJIT(3)

`%ni%` <- Negate(`%in%`)

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
planetsBaseCols <- c("SU", "MO", "ME", "VE", "MA", "JU", "SA", "UR", "NE", "PL", "NN", "CE", "JN", "VS", "PA", "CH")

# Aspects and orbs
aspects = c(0, 30, 60, 90, 120, 150, 180)
deforbs <- c(4.0, 4.0, 4.0, 4.0, 4.0, 4.0, 4.0)
defaspectsenergy <- c(10, 2, 3, 7, 5, 2, 10)
defaspectspolarity <- c(1, 1, 0, 1, 0, 0)

aspOrbsCols <- as.character(apply(expand.grid(aspects, planetsBaseCols[1:(length(planetsBaseCols)-1)]), 1, function(x) paste(x[2], x[1], sep='')))
zodDegrees <- seq(0, 360, by=2)

# planets columns names
planetsLonCols <- paste(planetsBaseCols, 'LON', sep='')
planetsLonGCols <- paste(planetsLonCols, 'G', sep='')
planetsLatCols <- paste(planetsBaseCols, 'LAT', sep='')
planetsSpCols <- paste(planetsBaseCols, 'SP', sep='')
planetsSpGCols <- paste(planetsSpCols, "G", sep="")
planetsCombLon <- combn(planetsLonCols, 2, simplify=F)
planetsCombLonCols <- as.character(lapply(planetsCombLon, function(x) paste(x[1], x[2], sep='')))
planetsCombLonOrbCols <- paste(planetsCombLonCols, 'ORB', sep='')
planetsGridLon <- expand.grid(planetsLonCols, planetsLonCols)
planetsGridLonCols <- as.character(apply(planetsGridLon, 1, function(x) paste(x[1], 'r', x[2], sep='')))
planetsGridAspCols <- as.character(apply(expand.grid(planetsGridLonCols, aspects), 1, function(x) paste(x[1], '_', as.numeric(x[2]), sep='')))
planetsAspCombCols <- as.character(apply(expand.grid(planetsCombLonCols, aspects), 1, function(x) paste(x[1], '_', as.numeric(x[2]), sep='')))
planetsGridZodCols <- as.character(apply(expand.grid(planetsLonCols, zodDegrees), 1, function(x) paste(x[1], '_', as.numeric(x[2]), sep='')))
aspectsEnergyCols <- paste(aspects, 'E', sep='')
planetsEnergyCols <- paste(planetsBaseCols, 'E', sep='')

# aspects types
aspectTypesList <- list(c('A', 'AE', 'SE', 'S'),
                        c('A', 'AE', 'SE'),
                        c('AE', 'SE'),
                        c('A', 'AE'),
                        c('SE', 'S'))

npath <- function(path) {
  normalizePath(path.expand(path))
}

deforbsmatrix = matrix(deforbs, nrow = 1, ncol = length(deforbs), dimnames = list('orbs', aspects))
defconjpolarity <- list(SULONMOLON=1, SULONMELON=1, SULONVELON=1, SULONMALON=0, SULONJULON=1, SULONSALON=0, SULONURLON=1, SULONNELON=0, SULONPLLON=0,
                        SULONNNLON=1, MOLONMELON=1, MOLONVELON=1, MOLONMALON=0, MOLONJULON=1, MOLONSALON=0, MOLONURLON=0, MOLONNELON=1, MOLONPLLON=0,
                        MOLONNNLON=1, MELONVELON=1, MELONMALON=0, MELONJULON=1, MELONSALON=0, MELONURLON=1, MELONNELON=1, MELONPLLON=0, MELONNNLON=1,
                        VELONMALON=0, VELONJULON=1, VELONSALON=0, VELONURLON=1, VELONNELON=1, VELONPLLON=1, VELONNNLON=1, MALONJULON=0, MALONSALON=0,
                        MALONURLON=0, MALONNELON=0, MALONPLLON=0, MALONNNLON=0, JULONSALON=0, JULONURLON=1, JULONNELON=1, JULONPLLON=0, JULONNNLON=1,
                        SALONURLON=0, SALONNELON=0, SALONPLLON=0, SALONNNLON=0, URLONNELON=0, URLONPLLON=0, URLONNNLON=0, NELONPLLON=0, NELONNNLON=0,
                        PLLONNNLON=0)

defconjpolaritymatrix <- matrix(0, nrow = length(planetsCombLonCols), ncol = 1, dimnames = list(planetsCombLonCols, c('0')))
# set default polarity for aspects
defconjpolaritymatrix[names(defconjpolarity), '0'] <- as.integer(defconjpolarity)
defconjpolarity <- as.integer(defconjpolaritymatrix)
defpolarity <- c(defconjpolarity, defaspectspolarity)
aspectspolaritycols <- aspects[2:length(aspects)]

defplanetsenergy <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
defpanalogy <- list(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
                    c(2, 3, 4, 5, 6, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
                    c(2, 3, 4, 5, 6, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4),
                    c(2, 3, 4, 5, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5),
                    c(2, 3, 4, 5, 6, 7, 8, 1, 1, 1, 1, 1, 1, 1, 1, 1),
                    c(2, 3, 4, 5, 6, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

zodSignsCols <- c('AR', 'TA', 'GE', 'CA', 'LE', 'VI', 'LI', 'SC', 'SA', 'CA', 'AC', 'PI')
planetsZodEnergyCols <- as.character(apply(expand.grid(planetsLonCols, zodSignsCols), 1, function(x) paste(x[1], '_', x[2], sep='')))
zodenergy.su <- c( 10,  01,  01,  01,  10,  01, -10,  01,  01,  01, -10,  01)
zodenergy.mo <- c( 01,  10,  01,  10,  01,  01,  01, -10,  01, -10,  01,  01)
zodenergy.me <- c( 01,  01,  10,  01,  01,  10,  01,  01, -10,  01,  01, -10)
zodenergy.ve <- c(-10,  10,  01,  01,  01, -10,  10, -10,  01,  01,  01,  10)
zodenergy.ma <- c( 10,  01,  01, -10,  01,  01, -10,  01,  01,  10,  01,  01)
zodenergy.ju <- c( 01,  01, -10,  10,  01, -10,  01,  01,  10, -10,  01,  10)
zodenergy.sa <- c(-10,  01,  01, -10, -10,  01,  10,  01,  01,  10,  10,  01)
zodenergy.ur <- c( 01,  01,  01,  01, -10,  01,  01,  01,  01,  01,  10,  01)
zodenergy.ne <- c( 01,  01,  01,  01,  01, -10,  01,  01,  01,  01,  01,  10)
zodenergy.pl <- c( 01, -10,  01,  01,  01,  01,  01,  10,  01,  01,  01,  01)
zodenergy.nn <- c( 01,  01,  01,  01,  10,  01,  01,  01,  01,  01,  01,  01)
zodenergy.ce <- c( 01,  01,  01,  01,  10,  01,  01,  01,  01,  01,  01,  01)
zodenergy.ch <- c( 01,  01,  01,  01,  10,  01,  01,  01,  01,  01,  01,  01)
zodenergy.jn <- c( 01,  01,  01,  01,  10,  01,  01,  01,  01,  01,  01,  01)
zodenergy.pa <- c( 01,  01,  01,  01,  10,  01,  01,  01,  01,  01,  01,  01)
zodenergy.vs <- c( 01,  01,  01,  01,  10,  01,  01,  01,  01,  01,  01,  01)

defplanetszodenergy <- c(zodenergy.su, zodenergy.mo, zodenergy.me, zodenergy.ve, zodenergy.ma, zodenergy.ju, zodenergy.sa,
                         zodenergy.ur, zodenergy.ne, zodenergy.pl, zodenergy.nn, zodenergy.ce, zodenergy.ch, zodenergy.jn,
                         zodenergy.pa, zodenergy.vs)

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

openSecurity <- function(security_file, mapricetype, mapricefs, mapricesl, dateformat="%Y.%m.%d", pricemadir=1) {
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

gaint_Population <- function (object, ...) {
  pdefpanalogy <- 0.5
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

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

cmpTestPlanetsSignificanceRelative <- function(execfunc, sinkfile, ...) {
  if (hasArg('sinkfile')) {
    sinkpathfile <- npath(paste("~/trading/predict/", sinkfile, ".txt", sep=''))
    sink(sinkpathfile, append=T)
    # output to master process
    #parallel:::setDefaultClusterOptions(outfile=sinkpathfile)
  }
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
    res <- dbGetPreparedQuery(db, sql, bind.data = data.frame(count=value))
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
      res <- dbGetPreparedQuery(db, sql, bind.data = fitness.best)
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

  openPlanets <- function(planets.file) {
    Date=DateMT4=Year=NULL
    planets.file <- npath(planets.file)
    planets <- fread(planets.file, sep="\t", na.strings="", verbose = F)
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
      planets[, c(curcol) := abs(((get(col1) - get(col2) + 180) %% 360) - 180)]
    }

    exprcopy <- paste("c(planetsCombLonOrbCols) := list(", paste(planetsCombLonCols, collapse=","), ")", sep="")
    planets[, eval(parse(text = exprcopy))]
    return(planets)
  }

  # Open multiple planets tables for different degsplit
  multipleOpenPlanets <- function(planetsfile, degsplits) {
    planetslist <- list()
    planetsorig <- openPlanets(paste("~/trading/dplanets/", planetsfile, ".tsv", sep=""))
    # calculate the lon deg splits
    calculateLonGroups <- function(x, degsplit) {
      return(cut(x, seq(0, 360, by=degsplit)))
    }

    for (degsplit in degsplits) {
      planets <- copy(planetsorig)
      planets[, c(planetsLonGCols) := lapply(.SD, calculateLonGroups, degsplit=degsplit), .SDcols=planetsLonCols]
      planetslist[[as.character(degsplit)]] <- planets
    }

    return(planetslist)
  }

  # calculate the planets aspects for a given solution
  processPlanetsAspects <- function(planetsorig, cusorbs) {
    # clone original to ensure is no modified
    planets <- copy(planetsorig)

    calculateAspects <- function(x) {
      allidx <- rep(FALSE, length(x))
      for (aspect in aspects) {
        comborb <- cusorbs['orbs', as.character(aspect)]
        rstart <- aspect-comborb
        rend <- aspect+comborb
        idx <- x >= rstart & x <= rend
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
        rstart <- aspect-comborb
        rend <- aspect+comborb
        idx <- x >= rstart & x <= rend
        x[idx] <- aspect - x[idx]
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

  planetsVarsSignificance <- function(planets, currency, threshold) {
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
    significance.full <- rbindlist(lapply(planetsLonGCols, planetTableAanalogy))
    significance.full[, c('pdiff', 'keyidx') := list(V2-V1, paste(key, variable, origin, sep='_'))]
    significance.full <- significance.full[pdiff >= threshold | pdiff <= -threshold]
    significance.full <- significance.full[!is.na(key)]
    significance.full[, Eff := cut(pdiff, c(-1000, 0, 1000), labels=c('down', 'up'), right=FALSE)]
    setkey(significance.full, 'keyidx', 'V1', 'V2')
    return(significance.full)
  }

  # build the daily signficance table
  buildDailySignificance <- function(significance, planets.pred, panalogymatrix) {
    # build daily significance indexes
    buildDailySignificanceIdxs <- function(planets.day) {
      curdate <- planets.day[['Date']]
      sigidxs <- paste(planets.day[planetsLonGCols], panalogymatrix['analogy', planetsLonGCols], planetsLonGCols, sep='_')
      datelist <- rep(curdate, length(sigidxs))
      day.idxs <- list(Date=datelist, keyidx=sigidxs)
      return(day.idxs)
    }

    significance.days.idxs <- rbindlist(apply(planets.pred, 1, buildDailySignificanceIdxs))
    setkeyv(significance.days.idxs, c('Date', 'keyidx'))
    significance.days <- merge(significance.days.idxs, significance, by='keyidx')
    # build daily planets lon & sp tables
    planets.lon <- data.table(melt(planets.pred, id.var=c('Date'), measure.var=planetsLonCols))
    planets.lon[, origin := gsub('LON', 'LONG', variable)]
    planets.lon[, Date := as.character(Date, format="%Y-%m-%d")]
    setnames(planets.lon, c('Date', 'variable', 'lon', 'origin'))
    planets.sp <- data.table(melt(planets.pred, id.var=c('Date'), measure.var=planetsSpCols))
    planets.sp[, origin := gsub('SP', 'LONG', variable)]
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
  dayAspectsEnergy <- function(planets.day, panalogy, aspectspolarity, conjpolarity, aspectsenergy, planetsenergy, energygrowthsp) {
    #planets.day <- trim(planets.day)
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
      curcol <- names(planets.day.asp[idx])
      # ignore aspects between nodes that happens ever
      if (curcol == 'SNLONNNLON') next
      col1 <- substr(curcol, 1, 2)
      col2 <- substr(curcol, 6, 7)
      # longitude col names
      loncol1 <- paste(col1, 'LONG', sep='')
      loncol2 <- paste(col2, 'LONG', sep='')
      planetenergy1 <- planetsenergy['energy', loncol1]
      planetenergy2 <- planetsenergy['energy', loncol2]
      aspect <- as.character(as.numeric(planets.day.asp[idx]))
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
      curcolorb <- paste(curcol, 'ORB', sep='')
      distance <- as.numeric(planets.day[[curcolorb]])
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

      energy[[length(energy)+1]] <- list(Date=curdate, origin=loncol1, up=up, down=down)
      energy[[length(energy)+1]] <- list(Date=curdate, origin=loncol2, up=up, down=down)
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
        watchdog[length(watchdog)+1] <- paste(energy, by.row$origin, ' Re')
      }
      # add the planet zodiacal energy
      zodenergy <- planetszodenergy[by.row$origin, significance.row$zsign]
      watchdog[length(watchdog)+1] <- paste(zodenergy, by.row$origin, ' zod ', significance.row$zsign)
      energy <- energy + zodenergy
      return(list(energy=energy, watchdog=paste(watchdog, collapse=' / ')))
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
    orbsmatrix <- matrix(args$cusorbs, nrow = 1, ncol = length(aspects), byrow = TRUE,
                         dimnames = list('orbs', aspects))

    # aspects polarities
    pol1 <- args$aspectspolarity[1:length(defconjpolarity)]
    pol2 <- args$aspectspolarity[(length(defconjpolarity)+1):length(args$aspectspolarity)]
    conjpolaritymatrix <- matrix(pol1, nrow = 1, ncol = length(planetsCombLonCols), byrow = TRUE, dimnames = list('polarity', planetsCombLonCols))
    aspectspolaritymatrix <- matrix(pol2, nrow = 1, ncol = length(aspectspolaritycols), byrow = TRUE, dimnames = list('polarity', aspectspolaritycols))

    aspectsenergymatrix <- matrix(args$aspectsenergy, nrow = 1, ncol = length(args$aspectsenergy), byrow = TRUE,
                                  dimnames = list(c('energy'), aspects))

    planetsenergymatrix <- matrix(args$planetsenergy, nrow = 1, ncol = length(args$planetsenergy), byrow = TRUE,
                                  dimnames = list(c('energy'), planetsLonGCols))

    planetszodenergymatrix <- matrix(args$planetszodenergy, nrow = length(planetsLonGCols), ncol = 12, byrow = TRUE,
                                     dimnames = list(planetsLonGCols, zodSignsCols))

    panalogymatrix <- matrix(args$panalogy, nrow = 1, ncol = length(args$panalogy), byrow = TRUE,
                             dimnames = list(c('analogy'), planetsLonGCols))

    sout <- with(args, paste("testPlanetsSignificanceRelative('testSolution', securityfile=", shQuote(securityfile), ", planetsfile=", shQuote(planetsfile),
                             ", tsdate=", shQuote(tsdate), ", tedate=", shQuote(tedate), ", vsdate=", shQuote(vsdate), ", vedate=", shQuote(vedate),
                             ", csdate=", shQuote(csdate), ", cedate=", shQuote(cedate),
                             ", mapredsm=", mapredsm, ", mapricefs=", mapricefs, ", mapricesl=", mapricesl,
                             ", mapricetype=", shQuote(mapricetype),
                             ", pricemadir=", pricemadir, ", degsplit=", degsplit, ", threshold=", threshold,
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
    planets <- processPlanetsAspects(args$planetslist[[as.character(args$degsplit)]], orbsmatrix)
    planets.train <- planets[Date > rdates[1] & Date <= rdates[2] & wday %in% c(1, 2, 3, 4, 5)]
    planets.pred <- planets[Date > rdates[3] & Date <= rdates[6] & wday %in% c(1, 2, 3, 4, 5)]
    security <- with(args, openSecurity(paste("~/trading/", securityfile, ".csv", sep=''), mapricetype, mapricefs, mapricesl, dateformat, pricemadir))
    significance <- with(args, planetsVarsSignificance(planets.train, security, threshold))
    years.test <- format(seq(rdates[3], rdates[4], by='year'), '%Y')
    years.conf <- format(seq(rdates[5], rdates[6], by='year'), '%Y')
    # get significance days
    significance.days <- buildDailySignificance(significance, planets.pred, panalogymatrix)
    significance.days <- dailySignificanceEnergy(significance.days, args$energyret, planetszodenergymatrix)
    significance.patterns <- significance.days[, buildDaySignificancePatterns(origin), by=Date]
    significance.patterns[, Date := as.Date(Date, format="%Y-%m-%d")]
    planets.pred <- merge(planets.pred, significance.patterns, by='Date')
    # helper function to process day aspects energy
    processPlanesDaySignificance <- function(x) {
      dayAspectsEnergy(x, panalogymatrix, aspectspolaritymatrix, conjpolaritymatrix, aspectsenergymatrix, planetsenergymatrix, args$energygrowthsp)
    }

    energy.days <- rbindlist(apply(planets.pred, 1, processPlanesDaySignificance))
    # calculate prediction
    prediction <- calculatePrediction(significance.days, energy.days, args$energymode)
    planets.pred <- planets.pred[prediction]
    planets.pred <- security[planets.pred]
    setkeyv(planets.pred, c('Date', 'Year.1'))
    snippet <- paste(strwrap(sout, width=170), collapse="\n")

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
    if (args$doplot) plotSolutionSnippet(snippet)
    # helper function to process predictions by year
    pltitle <- paste('Yearly prediction VS price movement for ', args$securityfile)
    processYearPredictions <- function(x) processPredictions(x, pltitle, args$doplot)
    # compute test predictions by year
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
    return(list(fitness=fitness))
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

    res <- list(correlation=correlation, volatility=volatility, matches.t=matches.t, matches.f=matches.f, matches.d=matches.d)
    return(res)
  }

  relativeTrendFitness <- function(x, planetslist, securityfile, planetsfile, tsdate, tedate, vsdate, vedate, csdate, cedate, fittype, dateformat) {
    # build the parameters based on GA indexes
    mapricetypes <- c('SMA', 'EMA', 'WMA', 'ZLEMA')
    predtypes <- c('absolute',  'relative')
    pricetypes <- c('averages',  'daily', 'priceaverage')
    analogytypes <- c(NA, 'SULONG', 'MOLONG', 'MELONG', 'VELONG', 'MALONG')
    pa.e = 12+length(planetsBaseCols)
    co.e = pa.e+length(deforbs)
    api.e = co.e+length(defpolarity)
    ae.e = api.e+length(defaspectsenergy)
    pe.e = ae.e+length(defplanetsenergy)
    pze.e = pe.e+length(defplanetszodenergy)

    args <-list(planetslist=planetslist,
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
                pricemadir=x[11],
                panalogy=analogytypes[x[12:(pa.e-1)]],
                cusorbs=x[pa.e:(co.e-1)],
                aspectspolarity=x[co.e:(api.e-1)],
                aspectsenergy=adjustEnergy(x[api.e:(ae.e-1)]),
                planetsenergy=adjustEnergy(x[ae.e:(pe.e-1)]),
                planetszodenergy=adjustEnergy(x[pe.e:(pze.e-1)]))

    res <- relativeTrend(args)
    return(res$fitness)
  }

  adjustEnergy <- function(x) {
    ifelse(x >= 0, (10 + x) / 10, (-10 + x) / 10)
  }

  optimizeRelativeTrend <- function(securityfile, planetsfile, tsdate, tedate, vsdate, vedate, csdate, cedate, fittype, dateformat) {
    cat("---------------------------- Initialize optimization ----------------------------------\n\n")
    dsmin <- 1
    dsmax <- 5
    panalogymin <- rep(1, length(planetsBaseCols))
    panalogymax <- rep(6, length(planetsBaseCols))
    orbsmin <- rep(1, length(deforbs))
    orbsmax <- rep(12, length(deforbs))
    polaritymin <- rep(0, length(defpolarity))
    polaritymax <- rep(1, length(defpolarity))
    aspectenergymin <- rep(0, length(defaspectsenergy))
    aspectenergymax <- rep(20, length(defaspectsenergy))
    planetenergymin <- rep(0, length(defplanetsenergy))
    planetenergymax <- rep(20, length(defplanetsenergy))
    planetzodenergymin <- rep(-20, length(defplanetszodenergy))
    planetzodenergymax <- rep(20, length(defplanetszodenergy))

    minvals <- c( 2,  2, 3, 1, dsmin,  0, 1, 0, -20, -20, 1, panalogymin, orbsmin, polaritymin, aspectenergymin,
                 planetenergymin, planetzodenergymin)
    maxvals <- c(10, 20, 6, 4, dsmax, 30, 2, 9,  20,  20, 2, panalogymax, orbsmax, polaritymax, aspectenergymax,
                 planetenergymax, planetzodenergymax)

    panalogyCols <- planetsLonGCols[5:length(planetsLonGCols)]
    varnames <- c('mapredsm', 'mapricefs', 'mapricesl', 'mapricetype', 'degsplit', 'threshold', 'energymode',
                  'energygrowthsp', 'energyret', 'alignmove', 'pricemadir', panalogyCols, aspOrbsCols, planetsCombLonCols, aspectspolaritycols,
                  aspectsEnergyCols, planetsEnergyCols, planetsZodEnergyCols)

    degsplits <- seq(dsmin, dsmax)
    planetslist <- multipleOpenPlanets(planetsfile, degsplits)

    ga("real-valued", fitness=relativeTrendFitness, names=varnames, parallel=TRUE,
       monitor=gaMonitor, maxiter=200, run=50, popSize=500, min=minvals, max=maxvals, pcrossover = 0.4, pmutation = 0.3,
       selection=gaint_rwSelection, mutation=gaint_raMutation, crossover=gaint_spCrossover, population=gaint_Population,
       planetslist=planetslist, securityfile=securityfile, planetsfile=planetsfile, tsdate=tsdate, tedate=tedate, vsdate=vsdate,
       vedate=vedate, csdate=csdate, cedate=cedate, fittype=fittype, dateformat=dateformat)
  }

  testSolution <- function(predfile, ...) {
    args <- list(...)
    if (!hasArg('dateformat')) stop("A dateformat is needed.")
    if (args$doplot) pdf(paste("~/chart_", predfile, ".pdf", sep=""), width = 11, height = 8, family='Helvetica', pointsize=12)
    degsplits <- c(args$degsplit)
    planetslist <- multipleOpenPlanets(args$planetsfile, degsplits)
    args[['planetslist']] <- planetslist
    relativeTrend(args)
    if (args$doplot) dev.off()
  }

  testSolutionDebug <- function(planetsfile, securityfile, tsdate, tedate, vsdate, vedate, csdate, cedate, fittype, dateformat, predfile, x) {
    # build the parameters based on GA indexes
    mapricetypes <- c('SMA', 'EMA', 'WMA', 'ZLEMA')
    predtypes <- c('absolute',  'relative')
    pricetypes <- c('averages',  'daily', 'priceaverage')
    analogytypes <- c(NA, 'SULONG', 'MOLONG', 'MELONG', 'VELONG', 'MALONG')
    pa.e = 12+length(planetsBaseCols)
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
                pricemadir=x[11],
                panalogy=analogytypes[x[12:(pa.e-1)]],
                cusorbs=x[pa.e:(co.e-1)],
                aspectspolarity=x[co.e:(api.e-1)],
                aspectsenergy=adjustEnergy(x[api.e:(ae.e-1)]),
                planetsenergy=adjustEnergy(x[ae.e:(pe.e-1)]),
                planetszodenergy=adjustEnergy(x[pe.e:(pze.e-1)]))

    if (!hasArg('dateformat')) stop("A dateformat is needed.")
    degsplits <- c(args$degsplit)
    planetslist <- multipleOpenPlanets(args$planetsfile, degsplits)
    args[['planetslist']] <- planetslist
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
  if (hasArg('sinkfile')) sink()
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
