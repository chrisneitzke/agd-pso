###############################################################################
#
# AGD-PSO Framework
#
# File:
#    penalty.R
#
# Description:
#    Dynamic linear penalty function for constrained optimization.
#
# Author:
#    Christiano Anderson Neitzke
#
# Version:
#    0.1
#
###############################################################################

###############################################################################
# Computes the dynamic penalty coefficient.
###############################################################################

computePenaltyCoefficient <- function(currentNFE,
                                      maxNFE,
                                      penaltyMin,
                                      penaltyMax){

    lambda <- penaltyMin +
        (penaltyMax - penaltyMin) *
        (currentNFE / maxNFE)

    return(lambda)

}

###############################################################################
# Computes the penalized fitness.
###############################################################################

computeFitness <- function(objective,
                           violation,
                           currentNFE,
                           maxNFE,
                           penaltyMin,
                           penaltyMax){

    lambda <- computePenaltyCoefficient(
        currentNFE,
        maxNFE,
        penaltyMin,
        penaltyMax
    )

    fitness <- objective + lambda * violation

    return(fitness)

}