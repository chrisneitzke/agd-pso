###############################################################################
#
# AGD-PSO Framework
#
# File:
#    test_g06_known_solution.R
#
###############################################################################

source("../config.R")

source("../benchmark/benchmark_interface.R")
source("../benchmark/benchmark_utils.R")

source("../benchmark/g06.R")

cat("\n")
cat("=============================================\n")
cat("G06 KNOWN SOLUTION TEST\n")
cat("=============================================\n\n")

###############################################################################
# Known optimum from CEC2006
###############################################################################

x <- c(
  
  14.095000,
  
  0.84296079
  
)

###############################################################################
# Evaluation
###############################################################################

result <- G06$evaluate(x)

cat("Objective ........:", result$objective, "\n\n")

cat("Constraints\n")

print(result$constraints)

cat("\n")

cat("Violation ........:", result$violation, "\n")

cat("Feasible .........:", result$feasible, "\n")

cat("\n")

cat("Known optimum ....:", G06$knownOptimum, "\n")

cat("\n")

###############################################################################
# Assertions
###############################################################################

stopifnot(abs(result$objective -
                G06$knownOptimum) < 1e-3)

stopifnot(result$violation <=
            CONSTRAINT_TOLERANCE)

stopifnot(result$feasible)

cat("\n")
cat("=============================================\n")
cat("KNOWN SOLUTION ACCEPTED\n")
cat("=============================================\n")