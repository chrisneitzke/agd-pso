###############################################################################
#
# AGD-PSO Framework
#
# File:
#    agdpso.R
#
# Description:
#    Adaptive Genetic Algorithm with Dynamic mutation control (GSO-inspired)
#    and PSO-based elite intensification.
#
# Author:
#    Christiano Anderson Neitzke
#
# Version:
#    1.0
#
###############################################################################

###############################################################################
# Creates one offspring using GA operators and PSO intensification.
###############################################################################

createHybridChild <- function(population,
                              benchmark,
                              eliteMask){
  
  ###########################################################################
  # Create child and retrieve parents
  ###########################################################################
  
  offspring <-
    createChildWithParents(
      population = population,
      benchmark = benchmark,
      mutationRate = population$mutationRate
    )
  
  child <- offspring$child
  
  ###########################################################################
  # Elite intensification
  ###########################################################################
  
  eliteParent <-
    eliteMask[offspring$parent1Index] ||
    eliteMask[offspring$parent2Index]
  
  if(eliteParent){
    
    #########################################################################
    # The offspring is initialized as a PSO particle using the
    # best elite parent as reference.
    #########################################################################
    
    if(population$individuals[[offspring$parent1Index]]$fitness <
       population$individuals[[offspring$parent2Index]]$fitness){
      
      reference <-
        population$individuals[[offspring$parent1Index]]
      
    }else{
      
      reference <-
        population$individuals[[offspring$parent2Index]]
      
    }
    
    child$velocity <- as.numeric(reference$velocity)
    child$pbest <- child$x
    child$pbestFitness <- Inf
    
    child <-
      updateVelocity(
        individual = child,
        globalBest = population$globalBest,
        benchmark = benchmark
      )
    
    child <-
      updatePosition(
        individual = child,
        benchmark = benchmark
      )
    
  }
  
  ###########################################################################
  # Final mutation (adaptive)
  ###########################################################################
  
  child <-
    gaussianMutation(
      individual = child,
      benchmark = benchmark,
      mutationRate = population$mutationRate
    )
  
  ###########################################################################
  # Repair bounds
  ###########################################################################
  
  child$x <-
    repairBounds(
      child$x,
      benchmark
    )
  
  return(child)
  
}

###############################################################################
# Creates the next hybrid generation.
###############################################################################

createNextGenerationHybrid <- function(population,
                                       benchmark){
  
  populationSize <- length(population$individuals)
  
  eliteSize <-
    max(
      1,
      ceiling(
        ELITE_PERCENTAGE *
          populationSize
      )
    )
  
  eliteIndices <-
    getEliteIndices(
      population,
      eliteSize
    )
  
  eliteMask <- rep(FALSE, populationSize)
  eliteMask[eliteIndices] <- TRUE
  
  ###########################################################################
  # Create new population
  ###########################################################################
  
  newPopulation <-
    createPopulation(
      popSize = populationSize,
      dimension = benchmark$dimension
    )
  
  ###########################################################################
  # Preserve evolution state
  ###########################################################################
  
  newPopulation$generation <- population$generation
  newPopulation$nfe <- population$nfe
  newPopulation$mutationRate <- population$mutationRate
  newPopulation$densityMean <- population$densityMean
  newPopulation$historyBest <- population$historyBest
  newPopulation$historyMean <- population$historyMean
  newPopulation$historyViolation <- population$historyViolation
  newPopulation$historyDiversity <- population$historyDiversity
  newPopulation$historyMutationRate <- population$historyMutationRate
  
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
      
      child <-
        createHybridChild(
          population = population,
          benchmark = benchmark,
          eliteMask = eliteMask
        )
      
      child$id <- i
      
      newPopulation$individuals[[i]] <- child
      
    }
    
  }
  
  return(newPopulation)
  
}

###############################################################################
# Adaptive Genetic Algorithm with Dynamic mutation control and PSO
# intensification.
###############################################################################

optimizeAGDPSO <- function(benchmark,
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
  # Initial random velocities
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
    
    population$generation <-
      population$generation + 1
    
    #########################################################################
    # Adaptive mutation (GSO-inspired)
    #########################################################################
    
    population <-
      computePopulationDensity(
        population,
        benchmark
      )
    
    population <-
      adaptMutationRate(
        population
      )
    
    
    #cat(  population$generation,  population$densityMean,   population$mutationRate,"\n" )
    
    
    #########################################################################
    # Hybrid generation
    #########################################################################
    
    population <-
      createNextGenerationHybrid(
        population,
        benchmark
      )
    
    #########################################################################
    # Evaluation
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