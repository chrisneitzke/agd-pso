###############################################################################
#
# AGD-PSO Framework
#
# File:
#    benchmark_utils.R
#
# Description:
#    Utility functions for benchmark evaluation.
#
# Author:
#    Christiano Anderson Neitzke
#
# Version:
#    1.0
#
###############################################################################

###############################################################################
# Computes the total constraint violation.
#
# Constraints must be in the form:
#
# g(x) <= 0
#
###############################################################################

computeViolation <- function(constraints){
  
  return(sum(pmax(0, constraints)))
  
}

###############################################################################
# Returns TRUE if the solution is feasible.
###############################################################################

#isFeasible <- function(violation){
#  return(violation == 0)
#}

isFeasible <- function(
    violation,
    tolerance = CONSTRAINT_TOLERANCE
){
  
  violation <= tolerance
  
}

###############################################################################
# Generates a random solution inside the search space.
###############################################################################

randomSolution <- function(benchmark){
  
  runif(
    
    n = benchmark$dimension,
    
    min = benchmark$lowerBound,
    
    max = benchmark$upperBound
    
  )
  
}

###############################################################################
# Checks whether a solution is inside the search space.
###############################################################################

checkBounds <- function(x,
                        benchmark){
  
  lower <- all(x >= benchmark$lowerBound)
  
  upper <- all(x <= benchmark$upperBound)
  
  return(lower && upper)
  
}

repairBounds <- function(x, benchmark){
  
  pmin(
    
    pmax(x,
         benchmark$lowerBound),
    
    benchmark$upperBound
    
  )
  
}

randomVelocity <- function(benchmark){
  
  range <- benchmark$upperBound - benchmark$lowerBound
  
  runif(
    
    benchmark$dimension,
    
    min = -0.10 * range,
    
    max =  0.10 * range
    
  )
  
}