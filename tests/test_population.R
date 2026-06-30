source("../config.R")
source("../core/individual.R")
source("../core/population.R")

pop <- createPopulation(
  
  popSize = 100,
  
  dimension = 13
  
)

stopifnot(length(pop$individuals) == 100)

stopifnot(pop$generation == 0)

stopifnot(pop$bestFitness == Inf)

stopifnot(is.list(pop$individuals[[1]]))

stopifnot(length(pop$individuals[[1]]$x) == 13)

stopifnot(pop$nfe == 0)

stopifnot(is.null(pop$best))

stopifnot(is.na(pop$densityMean))

stopifnot(pop$mutationRate == MUTATION_MIN)

cat("Population OK!\n")