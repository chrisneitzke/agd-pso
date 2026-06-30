source("../config.R")

source("../benchmark/benchmark_interface.R")

source("../core/individual.R")
source("../core/population.R")

source("../algorithms/initialization.R")

Sphere <- createBenchmark(
  
  name = "Sphere",
  
  dimension = 5,
  
  lowerBound = rep(-100, 5),
  
  upperBound = rep(100, 5),
  
  knownOptimum = 0,
  
  evaluateFunction = NULL
  
)

population <- initializePopulation(Sphere)

###############################################################################
# Population size
###############################################################################

stopifnot(length(population$individuals) == POP_SIZE)

###############################################################################
# Individual IDs
###############################################################################

stopifnot(population$individuals[[1]]$id == 1)

stopifnot(population$individuals[[POP_SIZE]]$id == POP_SIZE)

###############################################################################
# Dimension
###############################################################################

stopifnot(length(population$individuals[[1]]$x) == 5)

###############################################################################
# Bounds
###############################################################################

stopifnot(all(population$individuals[[1]]$x >= -100))

stopifnot(all(population$individuals[[1]]$x <= 100))

###############################################################################
# Velocity
###############################################################################

stopifnot(all(population$individuals[[1]]$velocity == 0))

###############################################################################
# Personal Best
###############################################################################

stopifnot(all(population$individuals[[1]]$pbest ==
                population$individuals[[1]]$x))

cat("Initialization OK!\n")
