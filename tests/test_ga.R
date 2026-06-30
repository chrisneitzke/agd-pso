###############################################################################
#
# AGD-PSO Framework
#
# File:
#    test_ga.R
#
# Description:
#    Integration test for the canonical Genetic Algorithm.
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

source("../algorithms/ga.R")

source("../benchmark/sphere.R")

###############################################################################
# Header
###############################################################################

cat("\n")
cat("=============================================\n")
cat("GENETIC ALGORITHM TEST\n")
cat("=============================================\n\n")

###############################################################################
# Benchmark
###############################################################################

benchmark <- Sphere

stopifnot(inherits(benchmark, "Benchmark"))
stopifnot(benchmark$name == "Sphere")

###############################################################################
# Execution
###############################################################################

set.seed(12345)

result <-
  optimizeGA(
    benchmark = benchmark,
    populationSize = POP_SIZE,
    maxNFE = MAX_NFE
  )

###############################################################################
# Results
###############################################################################

cat("Benchmark.................:", benchmark$name, "\n")
cat("Dimension.................:", benchmark$dimension, "\n")
cat("Generations...............:", result$generation, "\n")
cat("NFE.......................:", result$nfe, "\n")
cat("Best Fitness..............:", result$bestFitness, "\n")
cat("Global Best Fitness.......:", result$globalBestFitness, "\n")
cat("Mutation Rate............:", result$mutationRate, "\n")
cat("History Length...........:", length(result$historyBest), "\n")
cat("\n")

###############################################################################
# Basic assertions
###############################################################################

stopifnot(!is.null(result))

stopifnot(length(result$individuals) == POP_SIZE)

stopifnot(result$nfe > 0)
stopifnot(result$nfe <= MAX_NFE)

stopifnot(result$generation > 0)

###############################################################################
# Fitness
###############################################################################

stopifnot(is.finite(result$bestFitness))
stopifnot(is.finite(result$globalBestFitness))

stopifnot(result$bestFitness >= benchmark$knownOptimum)

stopifnot(result$globalBestFitness <=
            result$bestFitness + 1e-12)

###############################################################################
# Mutation rate
###############################################################################

tol <- 1e-12

stopifnot(is.finite(result$mutationRate))

stopifnot(result$mutationRate >= MUTATION_MIN - tol)

stopifnot(result$mutationRate <= MUTATION_MAX + tol)

###############################################################################
# History
###############################################################################

stopifnot(length(result$historyBest) > 0)

stopifnot(length(result$historyBest) ==
            length(result$historyMean))

stopifnot(length(result$historyBest) ==
            length(result$historyViolation))

stopifnot(length(result$historyBest) ==
            length(result$historyDiversity))

###############################################################################
# Best solutions
###############################################################################

stopifnot(!is.null(result$best))
stopifnot(!is.null(result$globalBest))

stopifnot(length(result$best$x) ==
            benchmark$dimension)

stopifnot(length(result$globalBest$x) ==
            benchmark$dimension)

###############################################################################
# Population consistency
###############################################################################

for(ind in result$individuals){
  
  stopifnot(length(ind$x) ==
              benchmark$dimension)
  
  stopifnot(is.finite(ind$fitness))
  
}

###############################################################################
# Success
###############################################################################

cat("\n")
cat("=============================================\n")
cat("GENETIC ALGORITHM TEST PASSED!\n")
cat("=============================================\n")