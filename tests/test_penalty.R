source("../algorithms/penalty.R")

fitness <- computeFitness(
    objective = 10,
    violation = 2,
    currentNFE = 50000,
    maxNFE = 100000,
    penaltyMin = 10,
    penaltyMax = 1000
)

print(fitness)