setAll2AspectsSet <- function() {
  # Aspects and orbs
  aspects <<-       c( 0 , 30 , 36 , 40 , 45 , 51 , 60 , 72 , 80 , 90 , 103 , 108 , 120 , 135 , 144 , 150 , 154 , 160 , 180)
  deforbs <<-       c(12 , 2  , 2  , 2  , 2  , 2  , 7  , 2  , 2  , 7  , 2   , 2   , 7   , 2   , 2   , 2   , 2   , 2   , 12)
  defpolarities <<- c( 2 , 1  , 0  , 1  , 0  , 0  , 1  , 0  , 1  , 0  , 1   , 0   , 1   , 0   , 1   , 2   , 1   , 0   , 0)
}

setAllAspectsSet <- function() {
  # Aspects and orbs
  aspects <<-       c( 0 , 30 , 36 , 40 , 45 , 51 , 60 , 72 , 80 , 90 , 103 , 108 , 120 , 135 , 144 , 154 , 160 , 180)
  deforbs <<-       c(12 , 2  , 2  , 2  , 2  , 2  , 5  , 5  , 2  , 7  , 2   , 2   , 7   , 2   , 5   , 2   , 2   , 12)
  defpolarities <<- c( 2 , 1  , 0  , 1  , 0  , 0  , 1  , 0  , 1  , 0  , 1   , 0   , 1   , 0   , 1   , 1   , 0   , 0)
}

setModernAspectsSet <- function() {
  aspects <<-       c( 0 , 30 , 45 , 52 , 60 , 72 , 90 , 103 , 120 , 135 , 144 , 150 , 180)
  deforbs <<-       c(10 , 3  , 3  , 3  , 6  , 3  , 10 , 3   , 6   , 3   , 3   , 6   , 10)
  defpolarities <<- c( 2 , 1  , 0  , 0  , 1  , 0  , 0  , 1   , 1   , 0   , 1   , 2   , 0)
}

setClassicAspectsSet <- function() {
  aspects <<-       c( 0 , 30 , 45 , 60 , 90 , 120 , 135 , 150 , 180)
  deforbs <<-       c(10 , 4  , 4  , 6  , 10 , 6   , 4   , 6   , 10)
  defpolarities <<- c( 2 , 1  , 0  , 1  , 0  , 1   , 0   , 1   , 0)
}

setModernAspectsSet()
