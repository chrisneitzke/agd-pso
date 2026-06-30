###############################################################################
#
# AGD-PSO Framework
#
# File:
#    population.R
#
# Description:
#    Defines the Population data structure.
#
# Author:
#    Christiano Anderson Neitzke
#
# Version:
#    0.1
#
###############################################################################

###############################################################################
# Creates a new population.
###############################################################################

createPopulation <- function(popSize,
                             dimension){

    individuals <- vector("list", popSize)

    for(i in seq_len(popSize)){
        individuals[[i]] <- createIndividual(dimension)
    }

    population <- list(
      
      individuals = individuals,
      
      generation = 0,
      
      nfe = 0,
      
      best = NULL,
      
      bestFitness = Inf,
      
      globalBest = NULL,
      
      globalBestFitness = Inf,
      
      densityMean = NA,
      
      mutationRate = MUTATION_MIN,
      
      historyBest = numeric(0),
      
      historyMean = numeric(0),
      
      historyViolation = numeric(0),
      
      historyDiversity = numeric(0),
      
      historyMutationRate = numeric(0)
      
    )

    return(population)

}