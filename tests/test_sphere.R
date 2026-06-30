
source("../config.R")
source("../benchmark/benchmark_interface.R")
source("../benchmark/benchmark_utils.R")

source("../benchmark/sphere.R")

source("../core/individual.R")
source("../core/population.R")

source("../algorithms/penalty.R")
source("../algorithms/initialization.R")
source("../algorithms/evaluation.R")

population <- initializePopulation(Sphere)

population <- evaluatePopulation(population, Sphere)

print(population$best$fitness)

print(population$nfe)