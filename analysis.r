library(xts)
library(timeDate)
library(quantmod)
library(ggplot2)
#library(msProcess)
library(plyr)
library(data.table)
library(fields)
library(reshape)
library(randomForest)
`%ni%` <- Negate(`%in%`)
# no scientific notation
options(scipen=100)
#options(error=recover)

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
                    c("SU", "SUR", "ME", "MER", "VE", "VER", "JU", "JUR"),
                    c("SU", "SUR", "VE", "VER", "JU", "JUR"),
                    c("MA", "MAR", "SA", "SAR", "UR", "URR", "NE", "NER", "PL", "PLR"))

aspectsList <- list(c('a0', 'a30', 'a45', 'a60', 'a72', 'a90', 'a120', 'a135', 'a144', 'a150', 'a180', 'a18', 'a33', 'a36', 'a40', 'a51', 'a80', 'a103', 'a108', 'a154', 'a160'),
                    c('a0', 'a45', 'a60', 'a90', 'a120', 'a150', 'a180'),
                    c('a0', 'a60', 'a90', 'a120', 'a180'),
                    c('a0', 'a30', 'a45', 'a60', 'a90', 'a120', 'a135', 'a150', 'a180'),
                    c('a30', 'a45', 'a72', 'a135', 'a144', 'a51', 'a103'),
                    c('a30', 'a45', 'a72', 'a135', 'a144', 'a18', 'a33', 'a36', 'a40', 'a51', 'a80', 'a103', 'a108', 'a154', 'a160'))

# aspect types cols names
aspectTypesCols <- c('SUT', 'MOT', 'MET', 'VET', 'MAT', 'JUT', 'SAT', 'URT', 'NET', 'PLT')
# aspects modes
aspModes <- c('all', 'majors', 'traditional', 'minmajors', 'myminors', 'minors')
# aspects types
aspTypes <- c('all', 'apsepexact', 'exact', 'apexact', 'sepexact')

npath <- function(path) {
  normalizePath(path.expand(path))
}

# a function that returns the position of n-th largest
maxn <- function(x, n) {
  order_x <- order(x, decreasing = TRUE)
  if ( length(order_x) < n ) {
    n = length(order_x)
  }
  x[order_x[n]]
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
  currency$val <- currency$Mid - currency$Open
  currency$Eff = cut(currency$val, c(-1, 0, 1), labels=c('down', 'up'), right=FALSE)
  currency$Date <- as.Date(as.character(currency$Date), format="%Y%m%d")
  currency
}

openCurrency2 <- function(currency_file) {
  currency_file <- npath(currency_file)
  currency <- read.table(currency_file, header = F, sep=",")
  names(currency) <- c('Date', 'Time', 'Open', 'High', 'Low', 'Close', 'Volume')
  currency <- currency[,-7]
  currency$Mid <- (currency$High + currency$Low + currency$Close + currency$Open) / 4
  currency$val <- currency$Mid - currency$Open
  #currency$val <- currency$Close - currency$Mid
  currency$Eff = cut(currency$val, c(-1, 0, 1), labels=c('down', 'up'), right=FALSE)
  currency$Date <- as.Date(as.character(currency$Date), format="%Y.%m.%d")
  currency
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
openTrans <- function(trans_file, effcorrection=1, aspmode='all', asptype='all') {
  trans_file <- npath(trans_file)

  switch(aspmode,
    all = aspectsNames <- c('a0', 'a30', 'a45', 'a60', 'a72', 'a90', 'a120', 'a135', 'a144', 'a150', 'a180', 'a18', 'a33', 'a36', 'a40', 'a51', 'a80', 'a103', 'a108', 'a154', 'a160'),
    majors = aspectsNames <- c('a0', 'a45', 'a60', 'a90', 'a120', 'a150', 'a180'),
    traditional = aspectsNames <- c('a0', 'a60', 'a90', 'a120', 'a180'),
    minmajors = aspectsNames <- c('a0', 'a30', 'a45', 'a60', 'a90', 'a120', 'a135', 'a150', 'a180'),
    myminors = aspectsNames <- c('a30', 'a45', 'a72', 'a135', 'a144', 'a51', 'a103'),
    minors <- aspectsNames <- c('a30', 'a45', 'a72', 'a135', 'a144', 'a18', 'a33', 'a36', 'a40', 'a51', 'a80', 'a103', 'a108', 'a154', 'a160'))

  if (!exists('aspectsNames')) {
    stop("No valid asptype value was provided.")
  }

  #TODO: the minor aspects that are very near are overlapping due orbs

  switch(asptype,
         all = types <- c('A', 'AE', 'SE', 'S'),
         apsepexact = types <- c('A', 'AE', 'SE'),
         exact = types <- c('AE', 'SE'),
         apexact = types <- c('A', 'AE'),
         sepexact = types <- c('SE', 'S'))

  if (!exists('types')) {
    stop("The provided asptype is not valid.")
  }

  # transits
  trans <- read.table(trans_file, header = T, sep="\t", na.strings = "")
  trans$Date <- as.Date(trans$Date, format="%Y-%m-%d")
  trans$S2 <- with(trans, {S+SUS+MOS+MES+VES+JUS+SAS+URS+NES+PLS})
  trans$endEffect <- timeDate(paste(trans$Date, trans$Hour), format = "%Y-%m-%d %H:%M:%S", zone = "UTC", FinCenter = "CET")
  trans$WD <- format(trans$endEffect, "%w");
  trans$H <- as.numeric(format(trans$endEffect, "%H"));
  # remove the slow planets and other points that complicate prediction
  trans <- subset(trans, PT %ni% c('MO', 'JU', 'SA', 'UR', 'NE', 'PL') & PR %ni% c('Asc', 'MC'));

  if (effcorrection) {
    # substract one day to the aspects tha are early in the day
    # the effect should be produced the previous day
    trans$Date <- as.double(strptime(trans$Date, "%Y-%m-%d"))
    trans$Date <- as.Date(as.POSIXlt(ifelse(trans$H < 4, trans$Date-86400, trans$Date), origin="1970-01-01"), format="%Y-%m-%d")
  }

  # select only the specified aspects
  if (aspmode != 'all') {
    cat("\tFiltering by:", aspectsNames, "\n")
    trans <- subset(trans, AS %in% aspectsNames);
  }

  # reset the aspects that are not in the required types
  if (asptype != 'all') {
    trans[,planetsList[[1]]] <- t(apply(trans[,c(planetsList[[1]],aspectTypesCols)], 1, removeAspectsOutType, asptype=types))
  }

  # replace NA aspects by none due randomForest need a value
  trans[,planetsList[[1]]] <- apply(trans[,planetsList[[1]]], 2, function(x) ifelse(is.na(x), 'none', x))
  # set as factors
  trans[,planetsList[[1]]] <- lapply(trans[,planetsList[[1]]], as.factor)

  # Convert the riseset times to correct timezone for Cyprus
  #tzcorrect <- format(as.POSIXlt(paste(trans$Date, trans$ASC1), format = "%Y-%m-%d %H:%M") + 3600 * 2.5, "%H:%M")
  #trans$ASC1 <- tzcorrect
  trans$PC <- trans$PT
  trans$PC <- factor(trans$PC, levels = c('SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'SA', 'UR', 'NE', 'PL'), labels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  # give each transit type an index
  trans$idx <- with(trans, paste(PT, AS, PR, SI, sep=''))
  trans$idx2 <- with(trans, paste(Date, PT, AS, PR, SI, sep=''))
  trans
}

openTransXts <- function(trans_file) {
  trans <- openTrans(trans_file)
  # xts need to receive a unique Date column so we leave only Date col
  # as Date type.
  trans$endEffect <- as.character(trans$endEffect)
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

predColNames = c('Date', 'idx', 'down', 'up', 'count', 'endEffect', 'S2', 'PC', 'SUR', 'SURD', 'MOR', 'MORD', 'MER', 'MERD', 'VER', 'VERD', 'MAR', 'MARD', 'JUR', 'JURD', 'SAR', 'SARD', 'URR', 'URRD', 'NER', 'NERD', 'PLR', 'PLRD', 'udis', 'ddis', 'corEff', 'ucor', 'dcor')

predictTransTable <- function(trans, trans_hist, cor_method, kplanets, kaspects, binarize=1, rmzeroaspects=1, qinmode='q3', maxasp=2) {
  # convert to data table
  trans_hist <- data.table(trans_hist)
  # get the maximum S2 transit by day
  #ddply(testMatrix, .(GroupID), summarize, Name=Name[which.max(Value)])
  # get the maximun and the ones that are in a max(S2)-5 threshold
  # ddply(trans, .(Date), summarize, Name=S2[which(S2 > max(S2)-3)])
  #selected <- ddply(trans, .(Date), summarize, idx=idx[which(S2 >= maxn(S2, maxasp))])
  predict_table <- predictAspectsTable(trans_hist)
  # to allow data table setkey
  trans$endEffect <- as.character(trans$endEffect)
  # create data table
  trans <- data.table(trans)
  setkey(trans, 'Date')
  selected <- trans[, idx[which(S2 >= maxn(S2, maxasp))], by=c('Date')]
  names(selected) <- c('Date', 'idx')
  # get the aspects correlation table
  aspect_dates_cor <- historyAspectsCorrelation(trans, trans_hist, cor_method, kplanets, kaspects, binarize, rmzeroaspects, qinmode)
  # merge
  aspect_dates_predict <- merge(selected, predict_table, by='idx')
  aspect_dates_predict <- merge(aspect_dates_predict, trans, by=c('Date', 'idx'))
  aspect_dates_predict <- merge(aspect_dates_predict, aspect_dates_cor, by=c('Date', 'idx'))
  # sort by date
  aspect_dates_predict <- arrange(aspect_dates_predict, desc(Date))
  #col_names = c('Date', 'idx', 'down', 'up', 'count', 'endEffect', 'JUR', 'SAR', 'URR', 'NER', 'PLR')
  aspect_dates_predict <- as.data.frame(aspect_dates_predict)
  aspect_dates_predict[predColNames]
}

predictTransTableWrite <- function(trans, trans_hist, cor_method, kplanets, kaspects, binarize, rmzeroaspects, qinmode, filename) {
  aspect_dates_predict <- predictTransTable(trans, trans_hist, cor_method, kplanets, kaspects, binarize, rmzeroaspects, qinmode, 2)
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

historyAspectsCorrelation <- function(trans, trans_hist, cor_method, kplanets, kaspects, binarize=1, rmzeroaspects=1, qinmode='q3') {
  if (qinmode %ni% c('q1', 'q2', 'q3', 'q4', 'q5')) stop("Provide a valid value for qinmode")
  #remove no needed cols to save memory
  trans <- trans[,c('Date', 'idx', kplanets), with=FALSE]
  trans_hist <- trans_hist[,c('Date', 'idx', 'Eff', kplanets), with=FALSE]
  #kplanets <- kplanets[plamode:length(kplanets)]
  dsasp <- reshape(trans_hist, varying = kplanets, v.names = "aspect", times = kplanets,  direction = "long")
  # select only the aspects to consider
  dsasp <- subset(dsasp, aspect %in% kaspects)
  # adjust factor levels
  dsasp$aspect <- factor(dsasp$aspect)
  # separate the up/down tables
  dsup <- subset(dsasp, Eff=='up')
  setkey(dsup, 'idx', 'aspect')
  dsdown <- subset(dsasp, Eff=='down')
  setkey(dsdown, 'idx', 'aspect')
  dsup <- dsup[, as.list(completeAspectList((table(aspect)), kaspects=kaspects)), by = c('idx')]
  ucolnames <- paste(names(dsup)[2:length(names(dsup))], 'u', sep='.')
  names(dsup) <- c('idx', ucolnames)
  dsdown <- dsdown[, as.list(completeAspectList((table(aspect)), kaspects=kaspects)), by = c('idx')]
  dcolnames <- paste(names(dsdown)[2:length(names(dsdown))], 'd', sep='.')
  # before set dcolnames build the substract colnames
  scolnames <- paste(names(dsdown)[2:length(names(dsdown))], 's', sep='.')
  names(dsdown) <- c('idx', dcolnames)
  # merge_recurse(apply(trans[,kplanets], 2, function(x) count(x)))
  # by(trans.eur[,kplanets], trans.eur$idx, function(x) { table(apply(x, 1, function(x) x)) })
  # reshape the transits data and generate aspect table
  dscurrent <- reshape(trans, varying = kplanets, v.names = "aspect", times = kplanets,  direction = "long")
  # select only the aspects to consider
  dscurrent <- subset(dscurrent, aspect %in% kaspects)
  # adjust factor levels
  dscurrent$aspect <- factor(dscurrent$aspect)
  setkey(dscurrent, 'Date', 'idx', 'aspect')
  dscurrent <- dscurrent[, as.list(completeAspectList((table(aspect)), kaspects=kaspects)), by = c('Date','idx')]
  ccolnames <- paste(names(dscurrent)[3:length(names(dscurrent))], 'c', sep='.')
  names(dscurrent) <- c('Date', 'idx', ccolnames)

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
  names(qtiles) <- c('Date', 'idx', 'q1', 'q2', 'q3', 'q4', 'q5')
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

predictTransTableTest <- function(directory, predict_table, currency_samples, sigthreshold=10, ret=FALSE) {
  tdiffs <- list()
  tl1 <- list()
  tl2 <- list()
  output <- list()

  for (i in 1:length(currency_samples)) {
    ptt <- merge(predict_table, currency_samples[[i]], by=c('Date'))
    # ignore if not enough predictions
    if (nrow(ptt) < nrow(currency_samples[[i]])/3) break
    # compare the prediction with the day effect
    ptt$test1 <- apply(ptt[,c('corEff','Eff')], 1, function(x) ifelse(is.na(x[1]) | is.na(x[2]), NA, x[1]==x[2]))
    t1 <- prop.table(table(ptt$test1, useNA='always'))
    t2 <- addmargins(table(ptt$test1, useNA='always'))
    # only if there are results for TRUE and FALSE
    if (all(c('TRUE', 'FALSE') %in% names(t1))) {
      tdiff <- round(abs(t1['TRUE']-t1[['FALSE']])*100, digits=2)
      if (!is.na(tdiff) & tdiff >= sigthreshold) {
        # append diff & tables
        tdiffs[[length(tdiffs)+1]] <- tdiff
        tl1[[length(tl1)+1]] <- t1
        tl2[[length(tl2)+1]] <- t2
        if (ret) output[[length(output)+1]] <- ptt[,!names(ptt) %in% c('Time', 'Mid')]
      }
    }
  }

  # if the differences are significant in all the test samples
  if (length(tdiffs) == length(currency_samples)) {
    for (i in 1:length(tdiffs)) {
      # print the diffs and the tables
      cat("=====================", tdiffs[[i]], "=====================\n")
      print(tl1[[i]])
      print(tl2[[i]])
      cat("\n")
    }
    # display the average
    cat("%%%", directory, "%%%", median(unlist(tdiffs)), "%%%%%%%%%%%%%%%%%%%%\n\n")
  }
  else {
    writeLines("\t\t\tInsignificant")
    cat("\n")
  }

  #predict_table$couEff <- apply(predict_table[,3:5], 1, function(x) ifelse(x[1] != x[2], ifelse(x[1] > x[2], 'down', 'up'), NA))
  #predict_table$test2 <- apply(predict_table[,c('Eff','couEff')], 1, function(x) ifelse(is.na(x[1]) | is.na(x[2]), 'none', x[1]==x[2]))
  #print(prop.table(table(predict_table$test2)))
  #print(addmargins(table(predict_table$test2)))
  #cat("\n")
  if (ret) output
}

testCorrelations <- function(sink_filename, directories, fileno, start1=0, start2=0, sigthreshold=10) {
  if (class(directories) != 'list') stop("Provide a valid directories list.")
  if (!hasArg('sink_filename')) stop("Provide a sink filename.")
  sink_filename <- paste("~/trading/predict/", sink_filename, ".txt", sep='')
  sink(npath(sink_filename), append=TRUE)
  # Start the clock!
  ptm <- proc.time()
  # currency data
  eurusd_full <- openCurrency2("~/trading/EURUSD_day_fxpro_20130611.csv")
  eurusd <- subset(eurusd_full, Date > as.Date("1998-01-01") & Date < as.Date("2012-01-01"))
  eurusd_test <- subset(eurusd_full, Date > as.Date("2012-01-01"))
  # all transit options
  transOpts <- expand.grid(aspModes, aspTypes)
  totTransOpts <- nrow(transOpts)
  # all pred options
  predOpts <- buildPredictOptions()
  totPredOpts <- nrow(predOpts)

  for (n1 in 1:totTransOpts) {
    if (n1 < start1) next
    # if a specific start option is set just apply it for the first transits cycle
    # then for the next cycle start again from 0 option
    if (n1 > start1) start2 <- 0
    # convert the opts to plain format
    aspmode <- as.character(transOpts[n1,1])
    asptype <- as.character(transOpts[n1,2])
    cat("####################################################\n")
    cat("Transits File #", fileno, "transOpts aspmode =", aspmode, "asptype =", as.character(asptype), "\n")
    cat("Directories: ", unlist(directories), "\n")

    # iterate trnas directories
    ltrans <- list()
    ltrans_hist <- list()
    for (tn in 1:length(directories)) {
      ltrans[[tn]] <- openTrans(paste("~/trading/", directories[[tn]], "/trans_", fileno, ".tsv", sep=''), 1, aspmode, asptype)
      ltrans_hist[[tn]] <- mergeTrans(ltrans[[tn]], eurusd)
      # found aspects in the transit data frame
      cat("\tFound aspects:", sort(as.character(unique(ltrans[[tn]]$AS))), "\n")
      gc()
    }

    cat("Transits execution time: ", proc.time()-ptm, "\n\n")
    cat("####################################################\n")

    # combn(keys, 2, simplify=FALSE)
    # Init time for fisrt loop
    looptm <- proc.time()

    for (n2 in 1:totPredOpts) {
      if (n2 < start2) next
      cormethod <- as.character(predOpts[n2,1])
      binarize <- predOpts[[n2,2]]
      rmzeroaspects <- predOpts[[n2,3]]
      qinmode <- as.character(predOpts[n2,4])
      maxasp <- predOpts[[n2,5]]
      kplanets <- predOpts[[n2,6]]
      kaspects <- predOpts[[n2,7]]
      samples <- generateSamples(eurusd_test, 3)

      cat("Trans Opts #", n1, " of ", totTransOpts, " -- Pred Opts #", n2, " of ", totPredOpts, "\n")
      cat("Transits File #", fileno, "transOpts aspmode=", aspmode, "asptype=", as.character(asptype), "\n")
      cat("\tPredict Opts cormethod=", cormethod, 'binarize=', binarize, 'rmzeroaspects=', rmzeroaspects, 'qinmode=', qinmode, 'maxasp=', maxasp, "\n")
      cat("\tPlanets mode:", kplanets, "\n")
      cat("\tAspects mode:", kaspects, "\n\n")

      for (tn in 1:length(directories)) {
        cat("\t", directories[[tn]], "\n")
        predt <- predictTransTable(ltrans[[tn]], ltrans_hist[[tn]], cormethod, kplanets, kaspects, binarize, rmzeroaspects, qinmode, maxasp)
        predtt <- predictTransTableTest(directories[[tn]], predt, samples, sigthreshold)
      }

      cat("Predict execution/loop time: ", proc.time()-ptm, " - ", proc.time()-looptm, "\n\n")
      looptm <- proc.time()
    }

    # collect garbage
    gc()
  }

  sink()
}

buildPredictOptions <- function() {
  # correlation methods to test
  corMethods <- c('canberra', 'euclidian', 'binary')
  # aspects modes
  aspModes <- c('all', 'majors', 'traditional', 'minmajors', 'myminors', 'minors')
  # aspects types
  aspTypes <- c('all', 'apsepexact', 'exact', 'apexact')
  # binarize
  binModes <- c(1)
  # remove zero aspects
  zeroaspectsModes <- c(0, 1)
  # quitile modes
  qinModes <- c('q3', 'q4')
  # max aspects modes
  maxaspModes <- c(1, 2)
  expand.grid(corMethods, binModes, zeroaspectsModes, qinModes, maxaspModes, planetsList, aspectsList)
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
  #eurusd <- openCurrency2("~/trading/")
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

initEuroPredict <- function() {
  predictTrans <- predictTransTable(trans.eur, trans.eurusd.eur, 'euclidian', planetsList[[2]], aspectsList[[4]], 0, 0, 3, 2)

  eurusd_full <- openCurrency2("~/trading/EURUSD_day_fxpro.csv")
  eurusd <- subset(eurusd_full, Date < as.Date("2012-01-01"))
  eurusd_test <- subset(eurusd_full, Date > as.Date("2012-01-01"))
  trans.eur <- openTrans("~/trading/transits_eur/EUR_1997-2014_trans_orb29.tsv", 1, 'traditional', 'all')
  trans.eurusd.eur  <- mergeTrans(trans.eur, eurusd)

  predcor <- historyAspectsCorrelation(trans.eur, trans.eurusd.eur, 'binary', planetsList[[1]], aspectsList[[1]], 1, 0, 4)
  p1 <- predcor[, sum(udis), by=Date]
  p2 <- predcor[, sum(ddis), by=Date]
  pt1 <- merge(p1, p2, by='Date')
  names(pt1) <- c('Date', 'up', 'down')
  pt1$corEff <- apply(pt1[,c('up','down'),with=FALSE], 1, function(x) ifelse(x[1] != x[2], ifelse(x[1] < x[2], 'up', 'down'), NA))
  pt1 <- merge(pt1, eurusd)
  prop.table(table(pt1$corEff==pt1$Eff, useNA="always"))

  planets.eur <- read.table("~/trading/EUR_2000-2014_planets_20130518.tsv", header = T, sep="\t")
  planets.eur$Date <- as.Date(planets.eur$Date, format="%Y-%m-%d")

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
  eurusd <- openCurrency2("~/trading/EURUSD_day_fxpro_20130611.csv")
  # merge with the currency history
  mergeTrans(trans, eurusd)
}

buildTransOpts <- function() {
  dateFix <- c(0,1)
  maxAspects <- seq(1,10)
  expand.grid(dateFix, aspModes, aspTypes, maxAspects)
}

bestRandomForest <- function(threshold=0.7) {
  trans.hist <- loadTransHist(0, 'all', 'all', 7)
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

  trans.testing$predicted <- predict(model, newdata=trans.testing[,cols])
  print(prop.table(table(trans.testing$Eff == trans.testing$predicted)))
  print(table(trans.testing$Eff == trans.testing$predicted))
  trans.testing <- data.table(trans.testing)
  setkey(trans.testing, 'Date')
  pred.table <- trans.testing[,as.list(table(predicted)), by=c('Date')]
  pred.table[,prop := (down-up)/(down+up)]
  eurusd <- openCurrency2("~/trading/EURUSD_day_fxpro_20130611.csv")
  eurusd <- data.table(eurusd)
  pred.table <- merge(pred.table, eurusd, by='Date')
  pred.table[, predEff := lapply(prop, function(x) ifelse(x > threshold, 'down', ifelse(x < -1*threshold, 'up', NA)))]
  print(table(pred.table$Eff==pred.table$predEff,useNA='always'))
  print(prop.table(table(pred.table$Eff==pred.table$predEff,useNA='always')))
  model
}

testRandomForest <- function(sink_filename, sigthreshold) {
  if (!hasArg('sink_filename')) stop("Provide a sink filename.")
  sink_filename <- paste("~/trading/predict/", sink_filename, ".txt", sep='')
  sink(npath(sink_filename), append=TRUE)
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

bestSolution <- function(n) {
  predOpts <- buildPredictOptions()
  cormethod <- as.character(predOpts[n,1])
  binarize <- predOpts[[n,2]]
  rmzeroaspects <- predOpts[[n,3]]
  qinmode <- as.character(predOpts[n,4])
  maxasp <- predOpts[[n,5]]
  kplanets <- predOpts[[n,6]]
  kaspects <- predOpts[[n,7]]

  eurusd_full <- openCurrency2("~/trading/EURUSD_day_fxpro_20130611.csv")
  eurusd <- subset(eurusd_full, Date > as.Date("1998-01-01") & Date < as.Date("2012-01-01"))
  eurusd_test <- subset(eurusd_full, Date > as.Date("2012-01-01"))
  samples <- generateSamples(eurusd_test, 3)

  trans.eur <- openTrans("~/trading/transits_eur/EUR_1997-2014_trans_orb26.tsv", 1, 'traditional', 'all')
  trans.eurusd.eur  <- mergeTrans(trans.eur, eurusd)
  ptt <- predictTransTable(trans.eur, trans.eurusd.eur, cormethod, kplanets, kaspects, binarize, rmzeroaspects, qinmode, maxasp)
  aspect_dates_cor <- historyAspectsCorrelation(data.table(trans.eur), data.table(trans.eurusd.eur), cormethod, kplanets, kaspects, binarize, rmzeroaspects, qinmode)
  ptt.test <- predictTransTableTest(ptt, samples, 5, TRUE)
  list(aspect_dates_cor, ptt.test)
}

#model <- randomForest(Eff ~ ., data = training, importance=TRUE, keep.forest=TRUE)
