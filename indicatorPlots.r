library(grid)
source("./analysis.r")
setClassicAspectsSet()
span <- 22
dailyPlanets = buildPlanetsIndicators()
dailyPlanetsResearch <- dailyPlanets[Date > as.Date('2017-01-01') & Date < as.Date('2021-01-01'),]
chartPeriod <- c(as.Date("2018-01-10"), as.Date("2020-12-31"))
todayDate <- as.Date(Sys.Date())
currentDates <- c(todayDate, todayDate + 1)
dateBreaks <- "7 days"
#getMySymbolsData("working")
security <- mainOpenSecurity("XLM-USD", 14, 28, "%Y-%m-%d", "2010-01-01")
datesHighs <- security$Date[peaks(security$Mid, span)]
datesLows <- security$Date[peaks(-security$Mid, span)]

drawSecurityPriceSerie <- function() {
  securityPeriod <- security[Date >= chartPeriod[1] & Date <= chartPeriod[2],]
  p <- ggplot(data = securityPeriod) +
    geom_line(aes(x = Date, y = Mid), colour = "white", alpha = 0.7) +
    geom_vline(xintercept = as.Date(datesHighs), linetype = "dashed", color = "green", size = 0.6, alpha = 0.7) +
    geom_vline(xintercept = as.Date(datesLows), linetype = "dashed", color = "red", size = 0.6, alpha = 0.7) +
    scale_x_date(date_breaks = dateBreaks, date_labels = "%Y-%m-%d", limits = chartPeriod) +
    theme_black() +
    theme(panel.margin = unit(c(0, 0, 0, 0), "null")) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "null")) +
    theme(panel.grid = element_blank()) +
    theme(panel.border = element_blank()) +
    theme(plot.margin = unit(c(0, 0, 0, 0), "null")) +
    theme(panel.margin = unit(c(0, 0, 0, 0), "null")) +
    theme(axis.ticks = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(axis.line = element_blank()) +
    theme(legend.position = "none") +
    theme(axis.ticks.length = unit(0, "null")) +
    theme(axis.ticks.margin = unit(0, "null")) +
    theme(legend.margin = unit(0, "null")) +
    theme(axis.text.x = element_text(angle = 90, size = 8), axis.title.x = element_blank(), axis.title.y = element_blank())
}

aspectsTheme <- function() {
  # theme(axis.text.x = element_text(angle = 90, size = 8)) +
  list(
    theme_black(),
    theme(panel.margin = unit(c(0, 0, 0, 0), "null")),
    theme(plot.margin = unit(c(0, 0, 0, 0), "null")),
    theme(panel.grid = element_blank()),
    theme(panel.border = element_blank()),
    theme(plot.margin = unit(c(0, 0, 0, 0), "null")),
    theme(panel.margin = unit(c(0, 0, 0, 0), "null")),
    theme(axis.ticks = element_blank()),
    theme(axis.text = element_blank()),
    theme(axis.title = element_blank()),
    theme(axis.line = element_blank()),
    theme(legend.position = "none"),
    theme(axis.ticks.length = unit(0, "null")),
    theme(axis.ticks.margin = unit(0, "null")),
    theme(legend.margin = unit(0, "null")),
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()),
    theme(axis.title.y = element_blank()))
}

aspectsLines <- function() {
  list(geom_hline(yintercept = 165, linetype = "solid", color = "magenta2", size = 1, alpha = 0.7),
       geom_hline(yintercept = 150, linetype = "solid", color = "salmon2", size = 1, alpha = 0.7),
       geom_hline(yintercept = 135, linetype = "solid", color = "magenta2", size = 1, alpha = 0.7),
       geom_hline(yintercept = 120, linetype = "solid", color = "salmon2", size = 1, alpha = 0.7),
       geom_hline(yintercept = 100, linetype = "solid", color = "magenta2", size = 1, alpha = 0.7),
       geom_hline(yintercept = 80, linetype = "solid", color = "magenta2", size = 1, alpha = 0.7),
       geom_hline(yintercept = 60, linetype = "solid", color = "salmon2", size = 1, alpha = 0.7),
       geom_hline(yintercept = 45, linetype = "solid", color = "magenta2", size = 1, alpha = 0.7),
       geom_hline(yintercept = 30, linetype = "solid", color = "salmon2", size = 1, alpha = 0.7),
       geom_hline(yintercept = 15, linetype = "solid", color = "magenta2", size = 1, alpha = 0.7),
       geom_vline(xintercept = currentDates, linetype = "dashed", color = "white", size = 0.6, alpha = 0.7),
       geom_vline(xintercept = as.Date(datesHighs), linetype = "dashed", color = "green", size = 0.6, alpha = 0.7),
       geom_vline(xintercept = as.Date(datesLows), linetype = "dashed", color = "red", size = 0.6, alpha = 0.7),
       scale_y_continuous(breaks = seq(0, 180, by = 10)),
       scale_x_date(date_breaks = dateBreaks, limits = chartPeriod))
}

uranusIndicators <- function() {
  p <- ggplot(data = dailyPlanetsResearch) +
    geom_point(aes(x = Date, y = SUURLON, size = 1), colour = "yellow", alpha = 0.5, size = 1) +
    geom_point(aes(x = Date, y = MOURLON, size = 1), colour = "black", alpha = 0.6, size = 1) +
    geom_point(aes(x = Date, y = MEURLON, size = 1), colour = "orange", alpha = 0.5, size = 1) +
    geom_point(aes(x = Date, y = VEURLON, size = 1), colour = "pink", alpha = 0.7, size = 1) +
    geom_point(aes(x = Date, y = MAURLON, size = 1), colour = "red", alpha = 0.6, size = 1) +
    geom_point(aes(x = Date, y = JUURLON, size = 1), colour = "palegreen3", alpha = 0.6, size = 1) +
    geom_point(aes(x = Date, y = SAURLON, size = 1), colour = "gray", alpha = 0.6, size = 1) +
    geom_point(aes(x = Date, y = NNURLON, size = 1), colour = "mediumaquamarine", alpha = 0.6, size = 1) +
    aspectsLines() +
    aspectsTheme()
}

p1 <- uranusIndicators()
p2 <- drawSecurityPriceSerie()

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))
# drawSlowIndicators()
# securityPeaksValleys(security)