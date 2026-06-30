source("../benchmark/benchmark_interface.R")

Sphere <- createBenchmark(
  
  name = "Sphere",
  
  dimension = 5,
  
  lowerBound = rep(-100,5),
  
  upperBound = rep(100,5),
  
  knownOptimum = 0,
  
  evaluateFunction = NULL
  
)

printBenchmark(Sphere)


source("../benchmark/benchmark_utils.R")

computeViolation(c(-1,-5,3,-2,7))