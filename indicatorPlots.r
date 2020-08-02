source("./analysis.r")
setModernAspectsSet()
dailyPlanets = buildPlanetsIndicators()
dailyPlanets2020 <- dailyPlanets[Date > as.Date('2017-01-01') & Date < as.Date('2021-01-01'), ]
chartPeriod <- c(as.Date("2018-01-10"), as.Date("2020-12-31"))
dateBreaks <- "7 days"

drawFastIndicators <- function () {
  p <- ggplot(data=dailyPlanets2020) +
    geom_point(aes(x=Date, y=SUURASP), colour="blue", alpha=0.5) +
    geom_point(aes(x=Date, y=MOURASP), colour="black", alpha=0.6) +
    geom_point(aes(x=Date, y=VEURASP), colour="pink", alpha=0.5) +
    #geom_point(aes(x=Date, y=MEURASP), colour="red", alpha=0.5) +
    geom_point(aes(x=Date, y=MAURASP), colour="red", alpha=0.6) +
    #geom_point(aes(x=Date, y=JUURASP), colour="purple", alpha=0.6) +
    theme(axis.text.x = element_text(angle = 90, size = 10)) +
    scale_y_continuous(breaks=seq(0, 180, by=10)) +
    scale_x_date(date_breaks = dateBreaks, date_labels = "%Y-%m-%d", limits = chartPeriod)
  print(p)
}

drawSlowIndicators <- function () {
  p <- ggplot(data=dailyPlanets2020) +
    geom_point(aes(x=Date, y=SUURASP), colour="blue", alpha=0.5) +
    geom_point(aes(x=Date, y=JUURASP), colour="purple", alpha=0.6) +
    geom_point(aes(x=Date, y=URNEASP), colour="black", alpha=0.6) +
    geom_point(aes(x=Date, y=SAURASP), colour="brown", alpha=0.6) +
    theme(axis.text.x = element_text(angle = 90, size = 10)) +
    scale_y_continuous(breaks=seq(0, 180, by=10)) +
    scale_x_date(date_breaks = dateBreaks, date_labels = "%Y-%m-%d", limits = chartPeriod)
  print(p)
}

drawFastIndicators()
#drawSlowIndicators()
