###############################################################################
#
# AGD-PSO Framework
#
# File:
#    benchmark_interface.R
#
# Description:
#    Defines the standard interface for optimization benchmarks.
#
# Author:
#    Christiano Anderson Neitzke
#
# Version:
#    1.0
#
###############################################################################

###############################################################################
# Creates a benchmark object.
###############################################################################

createBenchmark <- function(name,
                            dimension,
                            lowerBound,
                            upperBound,
                            knownOptimum,
                            evaluateFunction,
                            isMinimization = TRUE){
  
  stopifnot(length(lowerBound) == dimension)
  stopifnot(length(upperBound) == dimension)
  
  benchmark <- list(
    
    name = name,
    
    dimension = dimension,
    
    lowerBound = lowerBound,
    
    upperBound = upperBound,
    
    knownOptimum = knownOptimum,
    
    isMinimization = isMinimization,
    
    evaluate = evaluateFunction
    
  )
  
  class(benchmark) <- "Benchmark"
  
  return(benchmark)
  
}

###############################################################################
# Prints benchmark information.
###############################################################################

printBenchmark <- function(benchmark){
  
  cat("\n")
  cat("=========================================\n")
  cat("Benchmark :", benchmark$name, "\n")
  cat("Dimension :", benchmark$dimension, "\n")
  cat("Objective :", ifelse(benchmark$isMinimization,
                            "Minimization",
                            "Maximization"), "\n")
  cat("Known optimum :", benchmark$knownOptimum, "\n")
  cat("=========================================\n")
  
}