###############################################################################
#
# AGD-PSO Framework
#
# File:
#    initialization.R
#
# Description:
#    Population initialization routines.
#
# Author:
#    Christiano Anderson Neitzke
#
# Version:
#    1.0
#
###############################################################################

###############################################################################
# Creates one random individual inside the benchmark search space.
###############################################################################

initializeIndividual <- function(benchmark,
                                 id = NA_integer_) {
  
  individual <- createIndividual(benchmark$dimension)
  
  ###########################################################################
  # Identification
  ###########################################################################
  
  individual$id <- id
  
  ###########################################################################
  # Random position
  ###########################################################################
  
  individual$x <- runif(
    n   = benchmark$dimension,
    min = benchmark$lowerBound,
    max = benchmark$upperBound
  )
  
  ###########################################################################
  # Initial velocity (used by PSO)
  ###########################################################################
  
  individual$velocity <- rep(0, benchmark$dimension)
  
  ###########################################################################
  # Personal best (will be updated after first evaluation)
  ###########################################################################
  
  individual$pbest <- individual$x
  
  return(individual)
}

###############################################################################
# Creates the initial population.
###############################################################################

initializePopulation <- function(benchmark,
                                 populationSize = POP_SIZE) {
  
  population <- createPopulation(
    popSize   = populationSize,
    dimension = benchmark$dimension
  )
  
  for (i in seq_len(populationSize)) {
    
    population$individuals[[i]] <-
      initializeIndividual(
        benchmark = benchmark,
        id = i
      )
    
  }
  
  return(population)
}