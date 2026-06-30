###############################################################################
#
# AGD-PSO Framework
#
# File:
#    ga.R
#
# Description:
#    Canonical Genetic Algorithm.
#
# Author:
#    Christiano Anderson Neitzke
#
# Version:
#    1.0
#
###############################################################################

###############################################################################
# Tournament Selection (k = 2)
###############################################################################

tournamentSelection <- function(population){
  
  idx <- sample(
    seq_along(population$individuals),
    size = 2,
    replace = FALSE
  )
  
  if(population$individuals[[idx[1]]]$fitness <
     population$individuals[[idx[2]]]$fitness){
    
    winnerIndex <- idx[1]
    
  }else{
    
    winnerIndex <- idx[2]
    
  }
  
  return(
    
    list(
      
      individual =
        cloneIndividual(
          population$individuals[[winnerIndex]]
        ),
      
      index = winnerIndex
      
    )
    
  )
  
}

###############################################################################
# Arithmetic Crossover
###############################################################################

arithmeticCrossover <- function(parent1,
                                parent2,
                                benchmark){
  
  child1 <- createIndividual(benchmark$dimension)
  child2 <- createIndividual(benchmark$dimension)
  
  alpha <- runif(1)
  
  child1$x <-
    alpha * parent1$x +
    (1 - alpha) * parent2$x
  
  child2$x <-
    alpha * parent2$x +
    (1 - alpha) * parent1$x
  
  return(list(child1, child2))
  
}

###############################################################################
# Gaussian Mutation
###############################################################################

gaussianMutation <- function(individual,
                             benchmark,
                             mutationRate){
  
  for(j in seq_len(benchmark$dimension)){
    
    if(runif(1) < mutationRate){
      
      sigma <-
        0.10 *
        (benchmark$upperBound[j] -
           benchmark$lowerBound[j])
      
      individual$x[j] <-
        individual$x[j] +
        rnorm(1,
              mean = 0,
              sd = sigma)
      
    }
    
  }
  
  individual$x <-
    repairBounds(individual$x,
                 benchmark)
  
  return(individual)
  
}

###############################################################################
# Creates one offspring.
###############################################################################

createChild <- function(population,
                        benchmark,
                        mutationRate){
  
  parent1 <- tournamentSelection(population)
  parent2 <- tournamentSelection(population)
  
  if(runif(1) < CROSSOVER_RATE){
    
    children <-
      arithmeticCrossover(
        parent1$individual,
        parent2$individual,
        benchmark
      )
    
    child <- children[[sample(1:2, 1)]]
    
  }else{
    
    if(runif(1) < 0.5){
      
      child <- cloneIndividual(parent1$individual)
      
    }else{
      
      child <- cloneIndividual(parent2$individual)
      
    }
    
  }
  
  child <-
    gaussianMutation(
      child,
      benchmark,
      mutationRate
    )
  
  return(child)
  
}

###############################################################################
# Creates one offspring and returns parent indices.
#
# Used only by hybrid algorithms.
###############################################################################

createChildWithParents <- function(population,
                                   benchmark,
                                   mutationRate){
  
  parent1 <- tournamentSelection(population)
  parent2 <- tournamentSelection(population)
  
  if(runif(1) < CROSSOVER_RATE){
    
    children <-
      arithmeticCrossover(
        parent1$individual,
        parent2$individual,
        benchmark
      )
    
    child <- children[[sample(1:2, 1)]]
    
  }else{
    
    if(runif(1) < 0.5){
      
      child <- cloneIndividual(parent1$individual)
      
    }else{
      
      child <- cloneIndividual(parent2$individual)
      
    }
    
  }
  
  return(list(
      child = child,
      parent1Index = parent1$index,
      parent2Index = parent2$index
  ))
  
}

###############################################################################
# Creates the next generation.
###############################################################################

createNextGeneration <- function(population,
                                 benchmark){
  
  populationSize <- length(population$individuals)
  
  newPopulation <- createPopulation(
    
    popSize = populationSize,
    
    dimension = benchmark$dimension
    
  )
  
  ###########################################################################
  # Preserve evolution state
  ###########################################################################
  
  newPopulation$generation <- population$generation
  newPopulation$nfe <- population$nfe
  newPopulation$mutationRate <- population$mutationRate
  
  newPopulation$historyBest <- population$historyBest
  newPopulation$historyMean <- population$historyMean
  newPopulation$historyViolation <- population$historyViolation
  newPopulation$historyDiversity <- population$historyDiversity
  
  ###########################################################################
  # Elitism
  ###########################################################################
  
  newPopulation$individuals[[1]] <-
    cloneIndividual(population$best)
  
  newPopulation$individuals[[1]]$id <- 1
  
  ###########################################################################
  # Remaining individuals
  ###########################################################################
  
  if(populationSize > 1){
    
    for(i in 2:populationSize){
      
      child <- createChild(
        
        population = population,
        benchmark = benchmark,
        mutationRate = population$mutationRate
        
      )
      
      child$id <- i
      newPopulation$individuals[[i]] <- child
      
    }
    
  }
  
  return(newPopulation)
  
}

###############################################################################
# Canonical Genetic Algorithm
###############################################################################

optimizeGA <- function(benchmark,
                       populationSize = POP_SIZE, maxNFE = MAX_NFE){
  
  ###########################################################################
  # Initial population
  ###########################################################################
  
  population <- initializePopulation(
    benchmark = benchmark,
    populationSize = populationSize
  )
  
  population <- evaluatePopulation(
    population = population,
    benchmark = benchmark,
    maxNFE = maxNFE
  )
  
  ###########################################################################
  # Evolution
  ###########################################################################
  
  while(population$nfe < maxNFE){
    
    population$generation <- population$generation + 1
    
    population <-
      
      createNextGeneration(
        
        population,
        
        benchmark
        
      )
    
    population <-
      
      evaluatePopulation(
        
        population,
        
        benchmark,
        
        maxNFE = maxNFE
        
      )
    
  }
  
  return(population)
  
}

###############################################################################
# Returns the indices of the elite individuals.
###############################################################################

getEliteIndices <- function(population,
                            eliteSize){
  
  fitness <- sapply(
    population$individuals,
    function(ind) ind$fitness
  )
  
  order(fitness)[1:eliteSize]
  
}