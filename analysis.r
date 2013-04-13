library(xts)
library(timeDate)
library(quantmod)
library(ggplot2)
library(msProcess)

dsPlanetsData <- function(ds) {
  # remove observations that don't have enough market reaction (mondays with less than 12 hours)
  fds <- subset(ds, !(dayOfWeek(ds$endEffect) == 'Mon' & as.numeric(format(ds$endEffect, '%H')) < 12))
  # convert date as same format of planets days
  fds$Date <- as.Date(as.character(fds$endEffect), format="%Y-%m-%d")
  # build new data frame with the important columns
  fds <- subset(fds, select=c("PT", "Date", "val"))
  # calculate effect in categorical type
  fds$effect = cut(fds$val, c(-1, 0, 1), labels=c('down', 'up'), right=FALSE)
  # merge planets and filtered data set
  fds <- merge(fds, planets, by = "Date")
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
  fds.lon <- reshape(fds, varying = c("SULON", "MOLON", "MELON", "VELON", "MALON", "JULON", "SALON", "URLON", "NELON", "PLLON"), v.names = "lon", times = c("SULON", "MOLON", "MELON", "VELON", "MALON", "JULON", "SALON", "URLON", "NELON", "PLLON"),  direction = "long")
  p1 <- ggplot(fds.lon, aes(x=lon, y=id, colour=time, shape=time, facet= . ~ effect)) + facet_grid(. ~ effect) + geom_point() + scale_x_continuous(breaks=seq(from=0, to=359, by=5), limits=c(0,359)) + scale_shape_manual("",values=c(1,2,3,4,5,6,7,8,9,10)) + coord_polar(start = 1.57, direction=-1) + scale_y_continuous(breaks = seq(-5, 15, 1), limits = c(-5, 15)) + theme_update(axis.text.x = theme_text(angle = -30, size = 6))
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

dsPlanetsReport <- function(ds, data_name) {
  pdf("~/Desktop/plots.pdf", width = 11, height = 8, family='Helvetica', pointsize=12)
  plot(0:10, type = "n", xaxt="n", yaxt="n", bty="n", xlab = "", ylab = "")
  report_title <- paste("Chart ", data_name, " : ", ds[1,c('PT')], " ", ds[1,c('AS')], " ", ds[1,c('PR')])
  text(5, 10, report_title)
  obs_dates <- paste(fds[,c('Date')], collapse='\n')
  text(5, 4, obs_dates)
  fds <- dsPlanetsData(ds)
  # chart the hourly currency price for each date
  chart_dates = paste(fds$Date-1, fds$Date, sep="/")
  for (j in 1:length(chart_dates)) {
    if (nrow(eurusd.hour.xts[chart_dates[j]]) > 0) {
      chartSeries(eurusd.hour.xts, subset=chart_dates[j], major.ticks='hours', show.grid=TRUE, theme='white.mono')
    }
  }
  p1 <- qplot(data=fds, x=val, binwidth=0.0025) + scale_x_continuous(breaks=seq(from=-0.05, to=0.05, by=0.0025))
  print(p1)
  dsPlanetsLon(fds)
  dsPlanetsLat(fds)
  dsPlanetsSpeed(fds)
  dev.off()
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
  currency <- read.table(currency_file, header = T, sep=",")
  names(currency) <- c('Ticker', 'Date', 'Time', 'Open', 'Low', 'High', 'Close')
  currency <- currency[,-1]
  currency$val <- currency$Open - currency$Close
  currency$Eff = cut(currency$val, c(-1, 0, 1), labels=c('down', 'up'), right=FALSE)
  currency$Date <- as.Date(as.character(currency$Date), format="%Y%m%d")
  currency
}

openCurrency2 <- function(currency_file) {
  currency <- read.table("~/Desktop/daily EURUSD.txt", header = T, sep=",")
  currency <- currency[,-7:-8]
  currency$val <- currency$Close - currency$Open
  currency$Eff = cut(currency$val, c(-1, 0, 1), labels=c('down', 'up'), right=FALSE)
  currency$Date <- as.Date(as.character(currency$Date), format="%m/%d/%Y")
  currency
}

openCurrencyXts <- function(currency_file) {
  currency <- openTrans(currency_file)
  xts(currency[,-1:-2], order.by=currency[,1])
}

# Open a Transits table and merge with a currency table
openTrans <- function(trans_file) {
  # transits
  trans <- read.table(trans_file, header = T, sep="\t")
  trans$Date <- as.Date(trans$Date, format="%Y-%m-%d")
  trans$S2 <- with(trans, {SUS+MOS+MES+VES+JUS+SAS+URS+NES+PLS})
  #trans$H <- as.numeric(format(timeDate(paste(trans$Date, trans$Hour), format = "%Y-%m-%d %H:%M:%S", zone = "GMT", FinCenter = "GMT"), format="%H"))
  trans$endEffect <- round(timeDate(paste(trans$Date, trans$Hour), format = "%Y-%m-%d %H:%M:%S", zone = "GMT", FinCenter = "GMT"), 'h')
  trans <- subset(trans, PT != 'MO')
  # give each transit type an index
  trans$idx <- with(trans, paste(PT, AS, PR, SI, sep=''))
  trans
}

openTransXts <- function(trans_file) {
  trans <- openTrans(trans_file)
  xts(trans[,c(-1:-2,-62)], order.by=trans$endEffect)
}

# build merged table for currency and transits
mergeTrans <- function(trans_file, currency_table) {
  trans_table <- openTrans(trans_file)
  trans.price = merge(currency_table, trans_table, by = "Date")
  trans.price[,c(-1:-2,-9)]
}

# hourly rate history
eurusd.hour <- read.table("~/Desktop/trading/EURUSD_hour.csv", header = T, sep=",")
names(eurusd.hour) <- c('Ticker', 'Date', 'Time', 'Open', 'Low', 'High', 'Close')
eurusd.hour <- eurusd.hour[,-1]
eurusd.hour$val <- eurusd.hour$Close - eurusd.hour$Open
eurusd.hour$Date <- timeDate(paste(eurusd.hour$Date, eurusd.hour$Time), format = "%Y%m%d %H:%M:%S", zone = "GMT", FinCenter = "GMT")
eurusd.hour.xts <- xts(eurusd.hour[,-1:-2], order.by=eurusd.hour[,1])
# chart a day
chartSeries(eurusd.hour.xts, subset='2012-12-07', major.ticks='hours', show.grid=TRUE)

# euro usd currency daily
eurusd_2001 <- openCurrency("~/Desktop/trading/EURUSD_day.csv")
eurusd <- openCurrency2("~/Desktop/daily EURUSD.txt")
# usd cad currency daily
usdcad <- openCurrency("~/Desktop/trading/USDCAD_day.csv")
# transits USD chart
trans.usa = openTransXts("~/Desktop/trading/2001-2014_trans_USA.tsv")
# transits EUR chart
trans.eur <- openTransXts("~/Desktop/trading/EUR_1995_2015_trans.tsv")
# transits CAD chart
trans.cad <- openTransXts("~/Desktop/trading/1990-2015_trans_CAD.tsv")
# currency - transits EURUSD
trans.eurusd.usa  <- mergeTrans("~/Desktop/trading/2001-2014_trans_USA.tsv", eurusd)
trans.eurusd.eur  <- mergeTrans("~/Desktop/trading/EUR_1995_2015_trans.tsv", eurusd)
# currency - transits USDCAD
trans.usdcad.usa  <- mergeTrans("~/Desktop/trading/2001-2014_trans_USA.tsv", usdcad)

planets <- read.table("~/Desktop/trading/planets_test.tsv", header = T, sep="\t")
planets$Date <- as.Date(planets$Date, format="%Y-%m-%d")


# analysis of mars & jupiter aspects
mars.jupiter <- subset(trans.price.eur, Ptran == 'Mars' & Prad == 'Jupiter')
tapply(mars.jupiter$val, mars.jupiter$Aspect, mean)

mars.neptune <- subset(trans.price.eur, Ptran == 'Mars' & Prad == 'Neptune')
tapply(mars.neptune$val, mars.neptune$Aspect, mean)

# count the up/down observations
table(cut(mars.jupiter$val, c(-1, 0, 1), labels=c('down', 'up'), right=FALSE))
# order the dataset by val
#mars.jupiter[order(venus.jupiter$val)]
eurusd.aggregated = aggregate(trans.price.eur$val, list(Ptran = trans.price.eur$Ptran, Aspect = trans.price.eur$Aspect, Prad = trans.price.eur$Prad, Sign = trans.price.eur$Sign), mean)
# round the difference to 5 decimals
eurusd.aggregated$x = round(eurusd.aggregated$x, digits=5)
eurusd.aggregated = eurusd.aggregated[order(eurusd.aggregated$Ptran, eurusd.aggregated$Prad),]

# mercury & mc
mercury.mc <- subset(trans.price.eur, Ptran == 'Mercury' & Prad == 'MC')
tapply(mercury.mc$val, mercury.mc$Aspect, mean)


# mercury & neptune
mercury.neptune <- subset(trans.price.eur, Ptran == 'Mercury' & Prad == 'Neptune')
tapply(mercury.neptune$val, mercury.neptune$Aspect, mean)


# mercury & venus
mercury.venus <- subset(trans.price.eur, Ptran == 'Mercury' & Prad == 'Venus')
tapply(mercury.venus$val, mercury.venus$Aspect, mean)

# analysis of venus & mars aspects
venus.mars <- subset(trans.price.eur, Ptran == 'Venus' & Prad == 'Mars')
tapply(venus.mars$val, venus.mars$Aspect, mean)

# analysis of Venus & Mercury aspects
venus.mercury = subset(trans.price.eur, Ptran=='Venus' & Prad=='Mercury')
tapply(venus.mercury$val, venus.mercury$Aspect, mean)
#         conj          oppo          quad           sex          trig
# -0.0033500000 -0.0003250000 -0.0001730769  0.0019593750 -0.0012050000

# analysis of Venus & Jupiter aspects
venus.jupiter = subset(trans.price.eur, Ptran=='Venus' & Prad=='Jupiter')
tapply(venus.jupiter$val, venus.jupiter$Aspect, mean)

# venus & neptune
venus.neptune = subset(trans.price.eur, Ptran=='Venus' & Prad=='Neptune')
tapply(venus.neptune$val, venus.neptune$Aspect, mean)

# venus & pluto
venus.pluto = subset(trans.price.eur, Ptran=='Venus' & Prad=='Pluto')
tapply(venus.pluto$val, venus.pluto$Aspect, mean)


# analysis of venus & venus aspects
venus.venus <- subset(trans.price.eur, Ptran == 'Venus' & Prad == 'Venus')
tapply(venus.venus$val, venus.venus$Aspect, mean)
# sextil - performs a negative effect but from pisces do less damage than from scorpio
#        conj         oppo         quad          sex         trig
# 0.000518750 -0.000318750 -0.000893750 -0.001541667 -0.000950000

# analysis of jupiter & mars aspects
jupiter.mars <- subset(trans.price.eur, Ptran == 'Jupiter' & Prad == 'Mars')
tapply(jupiter.mars$val, jupiter.mars$Aspect, mean)
#         conj          oppo          quad           sex          trig
# -0.0034500000  0.0109500000 -0.0010666667 -0.0005000000  0.0008166667

# analysis of sun & neptune aspects
sun.neptune <- subset(trans.price.eur, Ptran == 'Sun' & Prad == 'Neptune')
tapply(sun.neptune$val, sun.neptune$Aspect, mean)
#       conj         oppo         quad          sex         trig
# 0.000743750 -0.003466667 -0.000700000 -0.002375000  0.001317647

sun.pluto <- subset(trans.price.eur, Ptran == 'Sun' & Prad == 'Pluto')
tapply(sun.pluto$val, sun.pluto$Aspect, mean)

# sun & venus
sun.venus <- subset(trans.price.eur, Ptran == 'Sun' & Prad == 'Venus')
tapply(sun.venus$val, sun.venus$Aspect, mean)

# sun & mercury
sun.mercury <- subset(trans.price.eur, Ptran == 'Sun' & Prad == 'Mercury')
tapply(sun.mercury$val, sun.mercury$Aspect, mean)

# sun & saturn
sun.saturn <- subset(trans.price.eur, Ptran == 'Sun' & Prad == 'Saturn')
tapply(sun.saturn$val, sun.saturn$Aspect, mean)

# sun & sun
sun.sun <- subset(trans.price.eur, Ptran == 'Sun' & Prad == 'Sun')
tapply(sun.sun$val, sun.sun$Aspect, mean)

# neptune & saturn
neptune.saturn <- subset(trans.price.eur, Ptran == 'Neptune' & Prad == 'Saturn')
tapply(neptune.saturn$val, neptune.saturn$Aspect, mean)


# analysis of sun & neptune aspects
sun.neptune <- subset(trans.price.eur, Ptran == 'Sun' & Prad == 'Neptune')
# simplify the way to access the columns
with(sun.neptune, tapply(val, Aspect, mean))
attach(sun.neptune)
tapply(val, Aspect, mean)
detach(sun.neptune)
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
year = substr(as.POSIXct(nowDate), 1, 4)
quarter = quarters(nowDate)

moon.asc <- subset(trans.price.eur, Ptran == 'Moon' & Prad == 'Asc')
tapply(moon.asc$val, moon.asc$Aspect, mean)
# generate histogram faceted by Aspect type and with density layer
# ggplot(ds, aes(x=val)) + geom_histogram(binwidth=.002, colour="black", fill="white", aes(y=..density..)) + facet_grid(AS ~ .) + scale_x_continuous(limits=c(-.005,.005)) + geom_density(alpha=.2, fill="#FF6666")


# Some data with peaks and throughs 
plot(values, t="l")

# Calculate the moving average with a window of 10 points 
mov.avg <- ma(values, 1, 10, FALSE)

numSwaps <- 1000    
mov.avg.swp <- matrix(0, nrow=numSwaps, ncol=length(mov.avg))

# The swapping may take a while, so we display a progress bar 
prog <- txtProgressBar(0, numSwaps, style=3)

for (i in 1:numSwaps)
{
# Swap the data
val.swp <- sample(values)
# Calculate the moving average
mov.avg.swp[i,] <- ma(val.swp, 1, 10, FALSE)
setTxtProgressBar(prog, i)
}

# Now find the 1% and 5% quantiles for each column
limits.1 <- apply(mov.avg.swp, 2, quantile, 0.01, na.rm=T)
limits.5 <- apply(mov.avg.swp, 2, quantile, 0.05, na.rm=T)

# Plot the limits
points(limits.5, t="l", col="orange", lwd=2)
points(limits.1, t="l", col="red", lwd=2)

currency.planets = merge(usdcad, planets, by = "Date")

## create a synthetic sequence
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


    Date   High
2001-09-21 0.9274
2003-06-16 1.1928
2004-01-12 1.2895
2004-12-31 1.3659
2006-05-15 1.2970
2008-04-23 1.5995
2009-11-26 1.5139
2010-11-05 1.4245
2011-05-05 1.4897

    Date   High
2001-07-06 0.8475
2002-02-01 0.8638
2004-04-26 1.1882
2005-11-17 1.1760
2006-10-16 1.2535
2008-11-21 1.2635
2010-06-08 1.2006

ggplot(filld, aes(xmin = start, xmax = end, ymin = 4, ymax = 5, fill = label)) + geom_rect() + geom_segment(aes(x = 4, y = 0, xend = 4, yend = 5, colour = label), size = 2, show_guide = FALSE) + geom_text(aes(x = p, y = 4.5, label = label), colour = "white", size = 10) + coord_polar() + scale_y_continuous(limits = c(0, 5))
