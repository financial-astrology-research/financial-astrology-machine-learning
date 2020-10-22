setAll2AspectsSet <- function() {
  # Aspects and orbs
  aspects <<- c(0, 30, 36, 40, 45, 51, 60, 72, 80, 90, 103, 108, 120, 135, 144, 150, 154, 160, 180)
  deforbs <<- c(12, 2, 2, 2, 2, 2, 7, 2, 2, 7, 2, 2, 7, 2, 2, 2, 2, 2, 12)
  defpolarities <<- c(2, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 2, 1, 0, 0)
}

setAllAspectsSet <- function() {
  # Aspects and orbs
  aspects <<- c(0, 30, 36, 40, 45, 51, 60, 72, 80, 90, 103, 108, 120, 135, 144, 154, 160, 180)
  deforbs <<- c(12, 2, 2, 2, 2, 2, 5, 5, 2, 7, 2, 2, 7, 2, 5, 2, 2, 12)
  defpolarities <<- c(2, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0)
}

setModernAspectsSet <- function() {
  aspects <<- c(0, 30, 45, 52, 60, 72, 90, 103, 120, 135, 144, 150, 180)
  aspectsEnergy <<- c(1, 1, 1, 1, 1, 2, 2, 1, 1, 1, 1, 1, 1)
  deforbs <<- c(10, 3, 3, 3, 6, 3, 10, 3, 6, 3, 3, 6, 10)
  defpolarities <<- c(2, 1, 0, 0, 1, 0, 0, 1, 1, 0, 1, 2, 0)
}

setClassicAspectsSet <- function() {
  aspects <<- c(0, 30, 45, 60, 90, 120, 135, 150, 180)
  aspectsEnergy <<- c(1, 1, 1, 1, 2, 1, 1, 1, 1)
  deforbs <<- c(10, 4, 4, 6, 10, 6, 4, 6, 10)
  defpolarities <<- c(2, 1, 0, 1, 0, 1, 0, 1, 0)
}

setClassicAspectsSet2 <- function() {
  aspects <<- c(0, 30, 45, 60, 90, 120, 135, 150, 180)
  aspectsEnergy <<- c(0, 0, 1, 0, 2, 1, 0, 2, 0)
  deforbs <<- c(10, 4, 4, 6, 10, 6, 4, 6, 10)
  defpolarities <<- c(2, 1, 0, 1, 0, 1, 0, 1, 0)
}

setClassicAspectsSet3 <- function() {
  aspects <<- c(0, 30, 45, 60, 90, 120, 135, 150, 180)
  aspectsEnergy <<- c(0, -0.7, 1, -1, 1, 1.5, -0.7, 2, -1)
  deforbs <<- c(10, 4, 4, 6, 10, 6, 4, 6, 10)
  defpolarities <<- c(2, 1, 0, 1, 0, 1, 0, 1, 0)
}

setClassicAspectsSet4 <- function() {
  aspects <<- c(0, 30, 45, 60, 90, 120, 135, 150, 180)
  aspectsEnergy <<- c(0, -0.7, 1, -1, 1, 1.5, -0.7, 2, -1)
  deforbs <<- c(5, 5, 5, 5, 5, 5, 5, 5, 5)
  defpolarities <<- c(2, 1, 0, 1, 0, 1, 0, 1, 0)
}

setClassicAspectsSet5 <- function() {
  aspects <<- c(0, 30, 45, 60, 90, 120, 135, 150, 180)
  aspectsEnergy <<- c(-3, -0.7, 1, -1, 1, 1.5, -0.7, 2, -1)
  deforbs <<- c(12, 5, 7, 5, 10, 5, 7, 7, 12)
  defpolarities <<- c(2, 1, 0, 1, 0, 1, 0, 1, 0)
}

setClassicAspectsSet6 <- function() {
  aspects <<- c(0, 30, 45, 60, 90, 120, 135, 150, 180)
  aspectsEnergy <<- c(2, 2, 2, 2, 2, 2, 2, 2, 2)
  deforbs <<- c(12, 5, 7, 5, 10, 5, 7, 7, 12)
  defpolarities <<- c(2, 1, 0, 1, 0, 1, 0, 0, 0)
}

setClassicAspectsSet7 <- function() {
  aspects <<- c(0, 30, 45, 60, 90, 120, 150, 180)
  aspectsEnergy <<- c(2, 2, 2, 2, 2, 2, 2, 2)
  deforbs <<- c(12, 5, 7, 5, 10, 5, 7, 12)
  defpolarities <<- c(2, 1, 0, 1, 0, 1, 0, 0)
}

# Limit orbs to 2 degrees which is the max strength.
setClassicAspectsSet8 <- function() {
  aspects <<- c(0, 30, 45, 60, 90, 120, 135, 150, 180)
  aspectsEnergy <<- c(3, 3, 3, 3, 3, 3, 3, 3, 3)
  deforbs <<- c(3, 3, 3, 3, 3, 3, 3, 3, 3)
  defpolarities <<- c(2, 1, 0, 1, 0, 1, 0, 0, 0)
}

# Limit orbs to 2 degrees which is the max strength.
setModernMixAspectsSet1 <- function() {
  aspects <<- c(0, 30, 45, 51, 60, 72, 90, 103, 120, 135, 144, 150, 180)
  aspectsEnergy <<- c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
  deforbs <<- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
  defpolarities <<- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
}

setModernAspectsSet2 <- function() {
  # aspects <<- c(0, 30, 45, 52, 60, 72, 90, 103, 120, 135, 144, 150, 180)
  aspects <<- c(0, 30, 45, 51, 60, 72, 90, 103, 120, 135, 144, 150, 180)
  aspectsEnergy <<- c(-3, -0.7, 1, 1.5, -1, 1, 1, -1.5, 1.5, -0.7, 2, 2, -1)
  deforbs <<- c(12, 5, 4, 3, 5, 5, 10, 3, 5, 6, 2, 4, 12)
  defpolarities <<- c(2, 1, 0, 1, 0, 1, 0, 1, 0)
}

setModernAspectsSet3 <- function() {
  # aspects <<- c(0, 30, 45, 52, 60, 72, 90, 103, 120, 135, 144, 150, 180)
  # aspects <<- c(0, 30, 36, 40, 45, 51, 60, 72, 80, 90, 103, 108, 120, 135, 144, 150, 154, 160, 180)
  aspects <<- c(0, 30, 36, 40, 45, 51, 60, 72, 80, 90, 103, 120, 135, 144, 150, 154, 160, 180)
  aspectsEnergy <<- c(-3, -1, -1, -1, 1, 1.5, -1, -1, 1, 1, -1.5, 1.5, -0.7, 2, 2, 1, 1, -1)
  deforbs <<- c(12, 4, 2, 2, 3, 3, 5, 5, 2, 8, 3, 5, 6, 2, 2, 2, 2, 12)
  defpolarities <<- c(2, 1, 0, 1, 0, 1, 0, 1, 0)
}

# All aspects with second scale disabled by zero energy.
setModernAspectsSet4 <- function() {
  aspects <<- c(0, 30, 36, 40, 45, 51, 60, 72, 80, 90, 103, 120, 135, 144, 150, 154, 160, 180)
  aspectsEnergy <<- c(-3, -1, 0, 0, 1, 1.5, -1, 0, 1, 1, -1.5, 1.5, -0.7, 2, 2, 0, 0, -1)
  deforbs <<- c(12, 4, 2, 2, 3, 3, 5, 5, 2, 8, 3, 5, 6, 2, 2, 2, 2, 12)
  defpolarities <<- c(2, 1, 0, 1, 0, 1, 0, 1, 0)
}

setModernAspectsSet5 <- function() {
  # aspects <<- c(0, 30, 45, 52, 60, 72, 90, 103, 120, 135, 144, 150, 180)
  aspects <<- c(0, 30, 45, 51, 60, 72, 90, 103, 120, 135, 144, 150, 180)
  aspectsEnergy <<- c(-3, -0.7, 1, 1.5, -1, 1, 1, -1.5, 1.5, -0.7, 2, 2, -1)
  deforbs <<- c(3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3)
  defpolarities <<- c(2, 1, 0, 1, 0, 1, 0, 1, 0)
}

setMajorsAspectsSet <- function() {
  aspects <<- c(0, 45, 60, 90, 120, 135, 180)
  aspectsEnergy <<- c(1, 1, 1, 2, 1, 1, 1)
  deforbs <<- c(10, 4, 6, 10, 6, 4, 10)
  defpolarities <<- c(2, 0, 1, 0, 1, 0, 0)
}

#setModernAspectsSet()
setClassicAspectsSet()
