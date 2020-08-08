library(grid)
source("./analysis.r")
setClassicAspectsSet()
span <- 22
dailyPlanets = buildPlanetsIndicators()
dailyPlanets2020 <- dailyPlanets[Date > as.Date('2017-01-01') & Date < as.Date('2021-01-01'), ]
chartPeriod <- c(as.Date("2019-01-10"), as.Date("2020-12-31"))
todayDate <- as.Date(Sys.Date())
currentDates <- c(todayDate, todayDate+1)
dateBreaks <- "3 days"
#getMySymbolsData("working")
security <- mainOpenSecurity("BTC-USD", 14, 28, "%Y-%m-%d", "2010-01-01")
datesHighs <- security$Date[peaks(security$Mid, span)]
datesLows <- security$Date[peaks(-security$Mid, span)]

drawSecurityPriceSerie <- function() {
  securityPeriod <- security[Date >= chartPeriod[1] & Date <= chartPeriod[2], ]
  p <- ggplot(data=securityPeriod) +
    geom_line(aes(x=Date, y=Mid), colour="blue", alpha=0.5) +
    geom_vline(xintercept = as.Date(datesHighs), linetype="dashed", color="forestgreen", size=0.6, alpha=0.7) +
    geom_vline(xintercept = as.Date(datesLows), linetype="dashed", color="red", size=0.6, alpha=0.7) +
    theme(axis.text.x = element_text(angle = 90, size = 8)) +
    scale_y_continuous(breaks=seq(0, 180, by=10)) +
    scale_x_date(date_breaks = dateBreaks, date_labels = "%Y-%m-%d", limits = chartPeriod)
}

drawFastIndicators <- function () {
  p <- ggplot(data=dailyPlanets2020) +
    geom_point(aes(x=Date, y=SUURLON), colour="blue", alpha=0.5) +
    geom_point(aes(x=Date, y=MOURLON), colour="black", alpha=0.6) +
    geom_point(aes(x=Date, y=VEURLON), colour="pink", alpha=0.5) +
    geom_point(aes(x=Date, y=MEURLON), colour="orange", alpha=0.5) +
    geom_point(aes(x=Date, y=MAURLON), colour="red", alpha=0.6) +
    geom_point(aes(x=Date, y=JUURLON), colour="palegreen3", alpha=0.6) +
    geom_point(aes(x=Date, y=SAURLON), colour="purple", alpha=0.6) +
    geom_point(aes(x=Date, y=NNURLON), colour="mediumaquamarine", alpha=0.6) +
    geom_point(aes(x=Date, y=SUMOLON), colour="steelblue", alpha=0.6) +
    geom_hline(yintercept = 180, linetype="dashed", color="magenta2", size=1, alpha=0.7) +
    geom_hline(yintercept = 150, linetype="dashed", color="salmon2", size=1, alpha=0.7) +
    geom_hline(yintercept = 135, linetype="dashed", color="magenta2", size=1, alpha=0.7) +
    geom_hline(yintercept = 120, linetype="dashed", color="salmon2", size=1, alpha=0.7) +
    geom_hline(yintercept = 90, linetype="dashed", color="magenta2", size=1, alpha=0.7) +
    geom_hline(yintercept = 60, linetype="dashed", color="salmon2", size=1, alpha=0.7) +
    geom_hline(yintercept = 45, linetype="dashed", color="magenta2", size=1, alpha=0.7) +
    geom_hline(yintercept = 30, linetype="dashed", color="salmon2", size=1, alpha=0.7) +
    geom_hline(yintercept = 0, linetype="dashed", color="blue", size=1, alpha=0.7) +
    geom_vline(xintercept = currentDates, linetype="dashed", color="maroon", size=0.6, alpha=0.7) +
    geom_vline(xintercept = as.Date(datesHighs), linetype="dashed", color="forestgreen", size=0.6, alpha=0.7) +
    geom_vline(xintercept = as.Date(datesLows), linetype="dashed", color="red", size=0.6, alpha=0.7) +
    scale_y_continuous(breaks=seq(0, 180, by=10)) +
    scale_x_date(date_breaks = dateBreaks, limits = chartPeriod) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
    #theme(axis.text.x = element_text(angle = 90, size = 8)) +
    #scale_x_date(date_breaks = dateBreaks, date_labels = "%Y-%m-%d", limits = chartPeriod)
}

venusIndicators <- function () {
  p <- ggplot(data=dailyPlanets2020) +
    geom_point(aes(x=Date, y=SUMELON), colour="blue", alpha=0.6) +
    geom_point(aes(x=Date, y=MOVELON), colour="black", alpha=0.6) +
    geom_point(aes(x=Date, y=MEVELON), colour="orange", alpha=0.6) +
    geom_point(aes(x=Date, y=VEMALON), colour="red", alpha=0.6) +
    geom_hline(yintercept = 180, linetype="dashed", color="magenta2", size=1, alpha=0.7) +
    geom_hline(yintercept = 150, linetype="dashed", color="salmon2", size=1, alpha=0.7) +
    geom_hline(yintercept = 135, linetype="dashed", color="magenta2", size=1, alpha=0.7) +
    geom_hline(yintercept = 120, linetype="dashed", color="salmon2", size=1, alpha=0.7) +
    geom_hline(yintercept = 90, linetype="dashed", color="magenta2", size=1, alpha=0.7) +
    geom_hline(yintercept = 60, linetype="dashed", color="salmon2", size=1, alpha=0.7) +
    geom_hline(yintercept = 45, linetype="dashed", color="magenta2", size=1, alpha=0.7) +
    geom_hline(yintercept = 30, linetype="dashed", color="salmon2", size=1, alpha=0.7) +
    geom_hline(yintercept = 0, linetype="dashed", color="blue", size=1, alpha=0.7) +
    geom_vline(xintercept = currentDates, linetype="dashed", color="maroon", size=0.6, alpha=0.7) +
    geom_vline(xintercept = as.Date(datesHighs), linetype="dashed", color="forestgreen", size=0.6, alpha=0.7) +
    geom_vline(xintercept = as.Date(datesLows), linetype="dashed", color="red", size=0.6, alpha=0.7) +
    theme(axis.text.x = element_text(angle = 90, size = 10)) +
    scale_y_continuous(breaks=seq(0, 180, by=10)) +
    scale_x_date(date_breaks = dateBreaks, date_labels = "%Y-%m-%d", limits = chartPeriod)
  print(p)
}

p1 <- drawFastIndicators()
p2 <- drawSecurityPriceSerie()

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
# drawSlowIndicators()
# securityPeaksValleys(security)