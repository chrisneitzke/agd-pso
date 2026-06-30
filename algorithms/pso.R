###############################################################################
#
# AGD-PSO Framework
#
# File:
#    pso.R
#
# Description:
#    Canonical Particle Swarm Optimization.
#
# Author:
#    Christiano Anderson Neitzke
#
# Version:
#    1.0
#
###############################################################################

###############################################################################
# Updates particle velocity.
###############################################################################

updateVelocity <- function(individual,
                           globalBest,
                           benchmark){
  
  r1 <- runif(benchmark$dimension)
  r2 <- runif(benchmark$dimension)
  
  cognitive <-
    COGNITIVE_FACTOR *
    r1 *
    (individual$pbest - individual$x)
  
  social <-
    SOCIAL_FACTOR *
    r2 *
    (globalBest$x - individual$x)
  
  individual$velocity <-
    INERTIA_WEIGHT *
    individual$velocity +
    cognitive +
    social
  
  return(individual)
  
}

###############################################################################
# Updates particle position.
###############################################################################

updatePosition <- function(individual,
                           benchmark){
  
  individual$x <-
    individual$x +
    individual$velocity
  
  individual$x <-
    repairBounds(
      individual$x,
      benchmark
    )
  
  return(individual)
  
}

###############################################################################
# Canonical Particle Swarm Optimization
###############################################################################

optimizePSO <- function(benchmark,
                        populationSize = POP_SIZE,
                        maxNFE = MAX_NFE){
  
  ###########################################################################
  # Initial population
  ###########################################################################
  
  population <-
    initializePopulation(
      benchmark = benchmark,
      populationSize = populationSize
    )
  
  ###########################################################################
  # Initial velocities
  ###########################################################################
  
  for(i in seq_along(population$individuals)){
    
    population$individuals[[i]]$velocity <-
      randomVelocity(benchmark)
    
  }
  
  ###########################################################################
  # Initial evaluation
  ###########################################################################
  
  population <-
    evaluatePopulation(
      population = population,
      benchmark = benchmark,
      maxNFE = maxNFE
    )
  
  ###########################################################################
  # Main loop
  ###########################################################################
  
  while(population$nfe < maxNFE){
    
    population$generation <- population$generation + 1
    
    #########################################################################
    # Move particles
    #########################################################################
    
    for(i in seq_along(population$individuals)){
      
      population$individuals[[i]] <-
        updateVelocity(
          individual = population$individuals[[i]],
          globalBest = population$globalBest,
          benchmark = benchmark
        )
      
      population$individuals[[i]] <-
        updatePosition(
          individual = population$individuals[[i]],
          benchmark = benchmark
        )
      
    }
    
    #########################################################################
    # Evaluate
    #########################################################################
    
    population <-
      evaluatePopulation(
        population = population,
        benchmark = benchmark,
        maxNFE = maxNFE
      )
    
  }
  
  return(population)
  
}