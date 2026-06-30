###############################################################################
#
# AGD-PSO Framework
#
# File:
#    analysis.R
#
###############################################################################

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(scales)

source("io.R")

source("plots_boxplot.R")
source("plots_convergence.R")
source("plots_diversity.R")
source("plots_violation.R")
source("plots_mutation.R")

###############################################################################
# Load experiment
###############################################################################

cat("\n")
cat("=========================================================\n")
cat("Loading experimental results\n")
cat("=========================================================\n")

experiment <- loadExperiment()

summaryData <- experiment$summary
historyData <- experiment$history

###############################################################################
# Diagnostics
###############################################################################

cat("\nSummary rows : ", nrow(summaryData), "\n")
cat("History rows : ", nrow(historyData), "\n\n")

cat("History columns:\n")
print(names(historyData))

cat("\nBenchmarks:\n")
print(unique(historyData$Benchmark))

cat("\nAlgorithms:\n")
print(unique(historyData$Algorithm))

###############################################################################
# Create output directory
###############################################################################

dir.create(
  "output",
  showWarnings = FALSE
)

dir.create(
  "output/figures",
  showWarnings = FALSE
)

###############################################################################
# Plots
###############################################################################

plotBoxplots(summaryData)

plotConvergence(historyData)

plotDiversity(historyData)

plotViolation(historyData)

plotMutation(historyData)

###############################################################################

cat("\n")
cat("=========================================================\n")
cat("Analysis finished successfully.\n")
cat("=========================================================\n")