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
  planetsCombBase <<- as.character(lapply(planetsComb, function(x) paste(x[1], x[2], sep='')))
  planetsCombLon <<- as.character(lapply(planetsComb, function(x) paste(x[1], x[2], 'LON', sep='')))
  planetsCombAsp <<- as.character(lapply(planetsComb, function(x) paste(x[1], x[2], 'ASP', sep='')))
  planetsCombOrb <<- as.character(lapply(planetsComb, function(x) paste(x[1], x[2], 'ORB', sep='')))
  zodSignsCols <<- c('AR', 'TA', 'GE', 'CA', 'LE', 'VI', 'LI', 'SC', 'SA', 'CP', 'AC', 'PI')
  lenZodEnergyMi <<- sum(planetsBaseCols %ni% planetsMajors) * length(zodSignsCols)
  #lenZodEnergyMa <- (length(planetsLonCols) * length(zodSignsCols)) - lenZodEnergyMi
  # Remove eclipse cols due it do not have speed
  planetsSpCols <<- planetsSpCols[grep('^E', planetsSpCols, ignore.case=T, invert=T)]
  planetsDecCols <<- planetsDecCols[grep('^E', planetsDecCols, ignore.case=T, invert=T)]
}

setPlanetsSUMOMEVEMAJUNNSA <- function() {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'NN', 'SA')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsSUMOMEVEMACEJUNNSA <- function() {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'CE', 'JU', 'NN', 'SA')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsSUMOMEVEMAJUNNSNSA <- function() {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'NN', 'SN', 'SA')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsSUMOMEVEMAJUNNSNSAESEM <- function() {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'NN', 'SN', 'SA', 'ES', 'EM')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsSUMOMEVEMAJUNNSAESEM <- function() {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'NN', 'SA', 'ES', 'EM')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsSUMOMEVEMAJUNNSNSA2 <- function() {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'NN', 'SN', 'SA')
  planetsMajors <<- c('SA', 'UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsSUMOMEVEMAJUNNSAUR <- function() {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'NN', 'SA', 'NE')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsSUMOMEVEMAJUNNSAURNE <- function() {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'JU', 'NN', 'SA', 'UR', 'NE')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsMOMEVESUMAJUNNSAURNEPL <- function() {
  planetsBaseCols <<- c('MO', 'ME', 'VE', 'SU', 'MA', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsMOMEVESUMAJUSAURNEPL <- function() {
  planetsBaseCols <<- c('MO', 'ME', 'VE', 'SU', 'MA', 'JU', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsMOMEVESUMACEJUSAURNEPL <- function() {
  planetsBaseCols <<- c('MO', 'ME', 'VE', 'SU', 'MA', 'CE', 'JU', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsMEVESUMACEJUNNSAURNEPL <- function() {
  planetsBaseCols <<- c('ME', 'VE', 'SU', 'MA', 'CE', 'JU', 'NN', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsMEVESUMACEJUSAURNEPL <- function() {
  planetsBaseCols <<- c('ME', 'VE', 'SU', 'MA', 'CE', 'JU', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsMEVESUMAJUSAURNEPL <- function() {
  planetsBaseCols <<- c('ME', 'VE', 'SU', 'MA', 'JU', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

setPlanetsSUMOMEVEMACEJUNNSNSAURNEPL <- function() {
  planetsBaseCols <<- c('SU', 'MO', 'ME', 'VE', 'MA', 'CE', 'JU', 'NN', 'SN', 'SA', 'UR', 'NE', 'PL')
  planetsMajors <<- c('UR', 'NE', 'PL')
  buildPlanetsColsNames(planetsBaseCols)
}

# By default use the classical astrology planets
setPlanetsSUMOMEVEMAJUNNSA()
buildPlanetsColsNames(planetsBaseCols)
