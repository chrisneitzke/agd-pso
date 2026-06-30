###############################################################################
#
# AGD-PSO Framework
#
# File:
#    gso.R
#
# Description:
#    GSO-inspired Adaptive Diversity Module.
#
#    This module does NOT implement the complete Glowworm Swarm Optimization
#    algorithm.
#
#    It estimates the population density and adaptively adjusts the mutation
#    rate used by the proposed AGD-PSO algorithm.
#
# Author:
#    Christiano Anderson Neitzke
#
# Version:
#    1.0
#
###############################################################################

###############################################################################
# Euclidean distance.
###############################################################################

computeDistance <- function(x1, x2){
  
  sqrt(sum((x1 - x2)^2))
  
}

###############################################################################
# Computes the number of neighbors of one individual.
###############################################################################

computeNeighbors <- function(index,
                             population,
                             benchmark){
  
  searchRange <-
    benchmark$upperBound -
    benchmark$lowerBound
  
  visionRadius <-
    VISION_RADIUS_FACTOR *
    sqrt(sum(searchRange^2))
  
  xi <- population$individuals[[index]]$x
  
  neighbors <- 0
  
  for(j in seq_along(population$individuals)){
    
    if(j == index){
      
      next
      
    }
    
    xj <- population$individuals[[j]]$x
    
    distance <-
      computeDistance(
        xi,
        xj
      )
    
    if(is.na(distance)){
      
      next
      
    }
    
    if(distance <= visionRadius){
      
      neighbors <- neighbors + 1
      
    }
    
  }
  
  return(neighbors)
  
}

###############################################################################
# Estimates the population density.
###############################################################################

computePopulationDensity <- function(population,
                                     benchmark){
  
  n <- length(population$individuals)
  
  totalNeighbors <- 0
  
  for(i in seq_len(n)){
    
    neighbors <-
      computeNeighbors(
        index = i,
        population = population,
        benchmark = benchmark
      )
    
    if(is.na(neighbors)){
      
      neighbors <- 0
      
    }
    
    population$individuals[[i]]$neighbors <- neighbors
    
    totalNeighbors <- totalNeighbors + neighbors
    
  }
  
  population$densityMean <-
    if(n > 0)
      totalNeighbors / n
  else
    0
  
  return(population)
  
}

###############################################################################
# Adapts the mutation rate according to the population density.
###############################################################################

adaptMutationRate <- function(population){
  
  populationSize <- length(population$individuals)
  
  if(is.na(population$densityMean)){
    
    population$densityMean <- 0
    
  }
  
  density <-
    population$densityMean /
    max(1, populationSize - 1)
  
  density <- max(0, density)
  density <- min(1, density)
  
  population$mutationRate <-
    max(
      MUTATION_MIN,
      min(
        MUTATION_MAX,
        MUTATION_MAX -
          density *
          (MUTATION_MAX - MUTATION_MIN)
      )
    )
  
  return(population)
  
}