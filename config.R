###############################################################################
# AGD-PSO
# Adaptive Genetic Algorithm with Density Feedback and PSO Refinement
#
# Configuration File
#
# This file centralizes every experimental parameter used by the framework.
# No numeric constants should appear elsewhere in the source code.
#
###############################################################################

###############################################################################
# General Configuration
###############################################################################

# Random seed (NULL = random)
SEED <- NULL

# Number of independent executions
RUNS <- 31

# Maximum Number of Function Evaluations (NFE)
MAX_NFE <- 100000

CONSTRAINT_TOLERANCE <- 1e-6

###############################################################################
# Population
###############################################################################

POP_SIZE <- 100

###############################################################################
# Genetic Algorithm
###############################################################################

# Tournament size
TOURNAMENT_SIZE <- 2

# Crossover
CROSSOVER_RATE <- 0.90

# Mutation (adaptive)
MUTATION_MIN <- 0.005
MUTATION_MAX <- 0.050

###############################################################################
# Particle Swarm Optimization
###############################################################################

# Percentage of elite refined by PSO
ELITE_PERCENTAGE <- 0.10

# Canonical PSO parameters (Clerc & Kennedy)
INERTIA_WEIGHT      <- 0.729
COGNITIVE_FACTOR    <- 1.49445
SOCIAL_FACTOR       <- 1.49445

###############################################################################
# Glowworm Swarm Optimization
###############################################################################

# Neighborhood radius factor
#
# Radius will be computed as:
#
# radius = GSO_RADIUS_FACTOR * sqrt(dimension) * domain_width
#
VISION_RADIUS_FACTOR <- 0.20

GSO_STEP_FACTOR <- 0.02

EPSILON <- 1e-12

###############################################################################
# Constraint Handling
###############################################################################

# Dynamic Linear Penalty
PENALTY_MIN <- 1000
PENALTY_MAX <- 50000

###############################################################################
# Output
###############################################################################

SAVE_GENERATION_RESULTS <- TRUE

SAVE_EXECUTION_LOG <- TRUE

SAVE_CONVERGENCE <- TRUE

SAVE_DENSITY <- TRUE

###############################################################################
# Graphics
###############################################################################

GENERATE_BOXPLOT <- TRUE

GENERATE_CONVERGENCE_PLOT <- TRUE

GENERATE_DENSITY_PLOT <- TRUE

###############################################################################
# Statistical Analysis
###############################################################################

SIGNIFICANCE_LEVEL <- 0.05