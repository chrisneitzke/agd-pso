###############################################################################
#
# AGD-PSO Framework
#
# File:
#    evaluation.R
#
# Description:
#    Evaluation routines for individuals and populations.
#
# Author:
#    Christiano Anderson Neitzke
#
# Version:
#    1.0
#
###############################################################################

###############################################################################
# Evaluates one individual.
###############################################################################

evaluateIndividual <- function(individual,
                               benchmark,
                               currentNFE,
                               maxNFE){
  
  ###########################################################################
  # Benchmark evaluation
  ###########################################################################
  
  result <- benchmark$evaluate(individual$x)
  
  individual$objective   <- result$objective
  individual$constraints <- result$constraints
  individual$violation   <- result$violation
  individual$feasible <- result$feasible
  
  ###########################################################################
  # Penalized fitness
  ###########################################################################
  
  individual$fitness <- computeFitness(
    
    objective  = individual$objective,
    
    violation  = individual$violation,
    
    currentNFE = currentNFE,
    
    maxNFE     = maxNFE,
    
    penaltyMin = PENALTY_MIN,
    
    penaltyMax = PENALTY_MAX
    
  )
  
  ###########################################################################
  # Update Personal Best (PSO)
  ###########################################################################
  
  if(individual$fitness < individual$pbestFitness){
    
    individual$pbest <- individual$x
    
    individual$pbestFitness <- individual$fitness
    
  }
  
  return(individual)
  
}

###############################################################################
# Evaluates the entire population.
###############################################################################

evaluatePopulation <- function(population,
                               benchmark,
                               maxNFE = MAX_NFE){
  
  bestFitness <- Inf
  bestIndex   <- 1
  
  totalFitness <- 0
  totalViolation <- 0
  
  ###########################################################################
  # Evaluate every individual
  ###########################################################################
  
  evaluated <- 0
  
  for(i in seq_along(population$individuals)){
    
    if(population$nfe >= maxNFE){
      break
    }
    
    evaluated <- evaluated + 1
    
    population$individuals[[i]] <-
      
      evaluateIndividual(
        
        individual = population$individuals[[i]],
        
        benchmark = benchmark,
        
        currentNFE = population$nfe,
        
        maxNFE = maxNFE
        
      )
    
    #######################################################################
    # Number of Function Evaluations
    #######################################################################
    
    population$nfe <- population$nfe + 1
    
    #######################################################################
    # Running statistics
    #######################################################################
    
    totalFitness <-
      totalFitness +
      population$individuals[[i]]$fitness
    
    totalViolation <-
      totalViolation +
      population$individuals[[i]]$violation
    
    #######################################################################
    # Global best
    #######################################################################
    
    if(population$individuals[[i]]$fitness < bestFitness){
      
      bestFitness <- population$individuals[[i]]$fitness
      
      bestIndex <- i
      
    }
    
  }
  
  ###########################################################################
  # Store best solution
  ###########################################################################
  
  population$best <-
    cloneIndividual(
      population$individuals[[bestIndex]]
    )
  
  population$bestFitness <-
    population$best$fitness
  
  ###########################################################################
  # Update Global Best
  ###########################################################################
  
  population <- updateGlobalBest(population)
  
  ###########################################################################
  # Population statistics
  ###########################################################################
  
  population$historyBest <-
    c(population$historyBest,
      population$bestFitness)
  
  population$historyMean <-
    c(
      population$historyMean,
      totalFitness / evaluated
    )
  
  population$historyViolation <-
    c(
      population$historyViolation,
      totalViolation / evaluated
    )
  
  ###########################################################################
  # Diversity
  ###########################################################################
  
  X <- do.call(
    
    rbind,
    
    lapply(
      
      population$individuals,
      
      function(ind) ind$x
      
    )
    
  )
  
  diversity <- mean(
    
    apply(
      X,
      2,
      function(col){
        
        s <- sd(col)
        
        if(is.na(s)) 0 else s
        
      }
      
    )
    
  )
  
  population$historyDiversity <- c(population$historyDiversity, diversity)
  
  population$historyMutationRate <- c(population$historyMutationRate, population$mutationRate)

  return(population)
  
}

###############################################################################
# Updates the global best solution.
###############################################################################

updateGlobalBest <- function(population){
  
  if(population$bestFitness < population$globalBestFitness){
    
    population$globalBest <-
      cloneIndividual(population$best)
    
    population$globalBestFitness <-
      population$bestFitness
    
  }
  
  return(population)
  
}