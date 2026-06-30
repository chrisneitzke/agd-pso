###############################################################################
#
# AGD-PSO Framework
#
# File:
#    test_gso.R
#
# Description:
#    Integration test for the GSO-inspired adaptive diversity module.
#
###############################################################################

###############################################################################
# Load framework
###############################################################################

source("../config.R")

source("../core/individual.R")
source("../core/population.R")

source("../benchmark/benchmark_interface.R")
source("../benchmark/benchmark_utils.R")

source("../algorithms/penalty.R")
source("../algorithms/initialization.R")
source("../algorithms/evaluation.R")
source("../algorithms/gso.R")

source("../benchmark/sphere.R")

###############################################################################
# Header
###############################################################################

cat("\n")
cat("=============================================\n")
cat("GSO MODULE TEST\n")
cat("=============================================\n\n")

###############################################################################
# Benchmark
###############################################################################

benchmark <- Sphere

stopifnot(inherits(benchmark, "Benchmark"))
stopifnot(benchmark$name == "Sphere")

###############################################################################
# Population
###############################################################################

set.seed(12345)

population <-
  initializePopulation(
    benchmark = benchmark,
    populationSize = POP_SIZE
  )

population <-
  evaluatePopulation(
    population = population,
    benchmark = benchmark,
    maxNFE = POP_SIZE
  )

###############################################################################
# Density
###############################################################################

population <-
  computePopulationDensity(
    population,
    benchmark
  )

###############################################################################
# Adaptive mutation
###############################################################################

population <-
  adaptMutationRate(
    population
  )

###############################################################################
# Results
###############################################################################

cat("Population Size..........:", length(population$individuals), "\n")
cat("Mean Density.............:", population$densityMean, "\n")
cat("Mutation Rate............:", population$mutationRate, "\n")
cat("\n")

###############################################################################
# Basic assertions
###############################################################################

stopifnot(length(population$individuals) == POP_SIZE)

stopifnot(!is.na(population$densityMean))
stopifnot(is.finite(population$densityMean))

stopifnot(population$densityMean >= 0)
stopifnot(population$densityMean <= POP_SIZE - 1)

###############################################################################
# Mutation rate
###############################################################################

tol <- 1e-12

stopifnot(is.finite(population$mutationRate))

stopifnot(population$mutationRate >=
            MUTATION_MIN - tol)

stopifnot(population$mutationRate <=
            MUTATION_MAX + tol)

###############################################################################
# Neighbor consistency
###############################################################################

for(ind in population$individuals){
  
  stopifnot(!is.null(ind$neighbors))
  
  stopifnot(is.numeric(ind$neighbors))
  
  stopifnot(ind$neighbors >= 0)
  
  stopifnot(ind$neighbors <= POP_SIZE - 1)
  
}

###############################################################################
# Distance function
###############################################################################

d <-
  computeDistance(
    population$individuals[[1]]$x,
    population$individuals[[2]]$x
  )

stopifnot(is.finite(d))
stopifnot(d >= 0)

###############################################################################
# Symmetry
###############################################################################

d12 <-
  computeDistance(
    population$individuals[[1]]$x,
    population$individuals[[2]]$x
  )

d21 <-
  computeDistance(
    population$individuals[[2]]$x,
    population$individuals[[1]]$x
  )

stopifnot(abs(d12 - d21) < 1e-12)

###############################################################################
# Success
###############################################################################

cat("=============================================\n")
cat("GSO MODULE TEST PASSED!\n")
cat("=============================================\n")