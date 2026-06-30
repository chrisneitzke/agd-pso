###############################################################################
#
# AGD-PSO Framework
#
# File:
#    test_g10_constraints.R
#
# Description:
#    Inspects the best solution found for G10.
#
###############################################################################

###############################################################################
# Load Configuration
###############################################################################

source("../config.R")

###############################################################################
# Core
###############################################################################

source("../core/individual.R")
source("../core/population.R")

###############################################################################
# Benchmark
###############################################################################

source("../benchmark/benchmark_interface.R")
source("../benchmark/benchmark_utils.R")

source("../benchmark/g10.R")

###############################################################################
# Algorithms
###############################################################################

source("../algorithms/penalty.R")
source("../algorithms/initialization.R")
source("../algorithms/evaluation.R")

source("../algorithms/ga.R")
source("../algorithms/pso.R")
source("../algorithms/gso.R")
source("../algorithms/agdpso.R")

###############################################################################
# Execute AGD-PSO
###############################################################################

set.seed(2)

population <-
  
  optimizeAGDPSO(
    
    benchmark = G10,
    
    populationSize = POP_SIZE,
    
    maxNFE = MAX_NFE
    
  )

###############################################################################
# Best Individual
###############################################################################

best <- population$globalBest

cat("\n")
cat("=============================================\n")
cat("G10 CONSTRAINT INSPECTION\n")
cat("=============================================\n\n")

cat("Objective ..........:", best$objective, "\n")
cat("Fitness ............:", population$globalBestFitness, "\n")
cat("Violation ..........:", best$violation, "\n")
cat("Feasible ...........:", best$feasible, "\n\n")

cat("Decision Variables\n\n")

for(i in seq_along(best$position)){
  
  cat(
    
    sprintf(
      
      "x[%d] = %.12f\n",
      
      i,
      
      best$position[i]
      
    )
    
  )
  
}

cat("\n")
cat("Constraints\n\n")

for(i in seq_along(best$constraints)){
  
  status <-
    
    if(best$constraints[i] <= CONSTRAINT_TOLERANCE)
      
      "OK"
  
  else
    
    "VIOLATED"
  
  cat(
    
    sprintf(
      
      "g%d = % .12e   %s\n",
      
      i,
      
      best$constraints[i],
      
      status
      
    )
    
  )
  
}

cat("\n")
cat("=============================================\n")