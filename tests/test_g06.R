###############################################################################
#
# AGD-PSO Framework
#
# File:
#    test_g06.R
#
# Description:
#    Integration test for the CEC2006 G06 benchmark.
#
###############################################################################

###############################################################################
# Load framework
###############################################################################

source("../config.R")

source("../benchmark/benchmark_interface.R")
source("../benchmark/benchmark_utils.R")

source("../benchmark/g06.R")

###############################################################################
# Header
###############################################################################

cat("\n")
cat("=============================================\n")
cat("G06 BENCHMARK TEST\n")
cat("=============================================\n\n")

###############################################################################
# Benchmark
###############################################################################

benchmark <- G06

###############################################################################
# Basic Properties
###############################################################################

stopifnot(inherits(benchmark, "Benchmark"))

stopifnot(benchmark$name == "G06")

stopifnot(benchmark$dimension == 2)

stopifnot(length(benchmark$lowerBound) == 2)

stopifnot(length(benchmark$upperBound) == 2)

stopifnot(abs(benchmark$knownOptimum + 6961.813875) < 1e-6)

###############################################################################
# Test 1 - Lower Bound
###############################################################################

x <- benchmark$lowerBound

result <- benchmark$evaluate(x)

cat("Lower Bound Objective....:", result$objective, "\n")
cat("Lower Bound Violation....:", result$violation, "\n")
cat("Lower Bound Feasible.....:", result$feasible, "\n\n")

stopifnot(is.numeric(result$objective))
stopifnot(is.numeric(result$violation))
stopifnot(length(result$constraints) == 2)
stopifnot(is.logical(result$feasible))

###############################################################################
# Test 2 - Upper Bound
###############################################################################

x <- benchmark$upperBound

result <- benchmark$evaluate(x)

cat("Upper Bound Objective....:", result$objective, "\n")
cat("Upper Bound Violation....:", result$violation, "\n")
cat("Upper Bound Feasible.....:", result$feasible, "\n\n")

stopifnot(length(result$constraints) == 2)

###############################################################################
# Test 3 - Center of Search Space
###############################################################################

x <- (benchmark$lowerBound + benchmark$upperBound) / 2

result <- benchmark$evaluate(x)

cat("Center Objective.........:", result$objective, "\n")
cat("Center Violation.........:", result$violation, "\n")
cat("Center Feasible..........:", result$feasible, "\n\n")

stopifnot(length(result$constraints) == 2)

###############################################################################
# Test 4 - Random Solutions
###############################################################################

set.seed(12345)

for(i in 1:100){
  
  x <- randomSolution(benchmark)
  
  result <- benchmark$evaluate(x)
  
  stopifnot(length(result$constraints) == 2)
  
  stopifnot(is.numeric(result$objective))
  
  stopifnot(is.numeric(result$violation))
  
  stopifnot(result$violation >= 0)
  
  stopifnot(is.logical(result$feasible))
  
  stopifnot(result$feasible ==
              (result$violation == 0))
  
}

###############################################################################
# Test 5 - Bounds
###############################################################################

for(i in 1:100){
  
  x <- randomSolution(benchmark)
  
  stopifnot(checkBounds(x, benchmark))
  
}

###############################################################################
# Test 6 - Constraint Consistency
###############################################################################

x <- randomSolution(benchmark)

result <- benchmark$evaluate(x)

expectedViolation <-
  computeViolation(result$constraints)

stopifnot(abs(expectedViolation -
                result$violation) < 1e-12)

###############################################################################
# Test 7 - Constraint Signs
###############################################################################

x <- c(14, 10)

result <- benchmark$evaluate(x)

stopifnot(length(result$constraints) == 2)

###############################################################################
# Success
###############################################################################

cat("Benchmark Name..........:", benchmark$name, "\n")
cat("Dimension...............:", benchmark$dimension, "\n")
cat("Constraints.............:", length(result$constraints), "\n")
cat("Known Optimum...........:", benchmark$knownOptimum, "\n\n")

cat("=============================================\n")
cat("G06 BENCHMARK TEST PASSED!\n")
cat("=============================================\n")