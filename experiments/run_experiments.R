###############################################################################
#
# AGD-PSO Framework
#
# File:
#    run_experiments.R
#
# Description:
#    Official experimental campaign.
#
# Author:
#    Christiano Anderson Neitzke
#
# Version:
#    1.0
#
###############################################################################

###############################################################################
# Load Configuration
###############################################################################

source("../config.R")

###############################################################################
# Core
###############################################################################

source("../core/individual.R")
source("../core/population.R")

###############################################################################
# Benchmark
###############################################################################

source("../benchmark/benchmark_interface.R")
source("../benchmark/benchmark_utils.R")

###############################################################################
# Algorithms
###############################################################################

source("../algorithms/penalty.R")
source("../algorithms/initialization.R")
source("../algorithms/evaluation.R")

source("../algorithms/ga.R")
source("../algorithms/pso.R")
source("../algorithms/gso.R")
source("../algorithms/agdpso.R")

###############################################################################
# Benchmarks
###############################################################################

source("../benchmark/g01.R")
source("../benchmark/g06.R")
source("../benchmark/g10.R")

###############################################################################
# Directories
###############################################################################

dirs <- c(
  
  "../results",
  
  "../results/summary",
  
  "../results/history"
  
)

for(dir in dirs){
  
  if(!dir.exists(dir)){
    
    dir.create(
      dir,
      recursive = TRUE
    )
    
  }
  
}

###############################################################################
# Benchmarks
###############################################################################

benchmarks <- list(
  
  G01,
  
  G06,
  
  G10
  
)

###############################################################################
# Algorithms
###############################################################################

algorithms <- list(
  
  GA = optimizeGA,
  
  PSO = optimizePSO,
  
  AGDPSO = optimizeAGDPSO
  
)

###############################################################################
# Campaign
###############################################################################

cat("\n")
cat("=========================================================\n")
cat("AGD-PSO EXPERIMENTAL CAMPAIGN\n")
cat("=========================================================\n\n")

campaignStart <- Sys.time()

###############################################################################
# Benchmark Loop
###############################################################################

for(benchmark in benchmarks){
  
  cat("\n")
  cat("=========================================================\n")
  cat("Benchmark:", benchmark$name, "\n")
  cat("=========================================================\n\n")
  
  ###########################################################################
  # Algorithm Loop
  ###########################################################################
  
  for(algorithmName in names(algorithms)){
    
    algorithm <- algorithms[[algorithmName]]
    
    cat("-----------------------------------------------\n")
    cat("Algorithm:", algorithmName, "\n")
    cat("-----------------------------------------------\n")
    
    #########################################################################
    # Summary
    #########################################################################
    
    summary <- data.frame(
      
      Benchmark = character(),
      
      Algorithm = character(),
      
      Run = integer(),
      
      Seed = integer(),
      
      Objective = numeric(),
      
      Fitness = numeric(),
      
      Violation = numeric(),
      
      Feasible = logical(),
      
      Success = integer(),
      
      NFE = integer(),
      
      Generations = integer(),
      
      Time = numeric(),
      
      stringsAsFactors = FALSE
      
    )
    
    #########################################################################
    # Independent Runs
    #########################################################################
    
    for(run in 1:RUNS){
      
      set.seed(run)
      
      cat(
        
        sprintf(
          
          "[%s][%s] Run %02d/%02d ... ",
          
          benchmark$name,
          
          algorithmName,
          
          run,
          
          RUNS
          
        )
        
      )
      
      startTime <- Sys.time()
      
      #######################################################################
      # Execute algorithm
      #######################################################################
      
      population <-
        
        algorithm(
          
          benchmark = benchmark,
          
          populationSize = POP_SIZE,
          
          maxNFE = MAX_NFE
          
        )
      
      #######################################################################
      # Execution time
      #######################################################################
      
      elapsed <-
        
        as.numeric(
          
          difftime(
            
            Sys.time(),
            
            startTime,
            
            units = "secs"
            
          )
          
        )
      
      #######################################################################
      # Success
      #######################################################################
      
      success <-
        
        as.integer(
          
          population$globalBest$feasible
          
        )
      
      #######################################################################
      # Append Summary
      #######################################################################
      
      summary <-
        
        rbind(
          
          summary,
          
          data.frame(
            
            Benchmark = benchmark$name,
            
            Algorithm = algorithmName,
            
            Run = run,
            
            Seed = run,
            
            Objective = population$globalBest$objective,
            
            Fitness = population$globalBestFitness,
            
            Violation = population$globalBest$violation,
            
            Feasible = population$globalBest$feasible,
            
            Success = success,
            
            NFE = population$nfe,
            
            Generations = population$generation,
            
            Time = elapsed,
            
            stringsAsFactors = FALSE
            
          )
          
        )
      
      #######################################################################
      # Save History
      #######################################################################
      
      history <- data.frame(
        
        Generation = seq_along(population$historyBest) - 1,
        
        NFE = seq_along(population$historyBest) * POP_SIZE,
        
        BestFitness = population$historyBest,
        
        MeanFitness = population$historyMean,
        
        MeanViolation = population$historyViolation,
        
        Diversity = population$historyDiversity,
        
        MutationRate = population$historyMutationRate
        
      )
      
      write.csv(
        
        history,
        
        file = sprintf(
          
          "../results/history/%s_%s_run%02d.csv",
          
          benchmark$name,
          
          algorithmName,
          
          run
          
        ),
        
        row.names = FALSE
        
      )
      
      #######################################################################
      # Progress
      #######################################################################
      
      cat(
        
        sprintf(
          
          "Fitness = %.10f   Violation = %.6f   Time = %.2fs\n",
          
          population$globalBestFitness,
          
          population$globalBest$violation,
          
          elapsed
          
        )
        
      )
      
    }
    
    #########################################################################
    # Save Summary
    #########################################################################
    
    write.csv(
      
      summary,
      
      file = sprintf(
        
        "../results/summary/%s_%s.csv",
        
        benchmark$name,
        
        algorithmName
        
      ),
      
      row.names = FALSE
      
    )
    
    #########################################################################
    # Summary Statistics
    #########################################################################
    
    cat("\n")
    
    cat(
      
      sprintf(
        
        "Average Fitness ......... %.10f\n",
        
        mean(summary$Fitness)
        
      )
      
    )
    
    cat(
      
      sprintf(
        
        "Best Fitness ............ %.10f\n",
        
        min(summary$Fitness)
        
      )
      
    )
    
    cat(
      
      sprintf(
        
        "Average Objective ....... %.10f\n",
        
        mean(summary$Objective)
        
      )
      
    )
    
    cat(
      
      sprintf(
        
        "Average Violation ....... %.10f\n",
        
        mean(summary$Violation)
        
      )
      
    )
    
    cat(
      
      sprintf(
        
        "Success Rate ............ %.2f %%\n",
        
        100 * mean(summary$Success)
        
      )
      
    )
    
    cat(
      
      sprintf(
        
        "Average Time ............ %.2f s\n",
        
        mean(summary$Time)
        
      )
      
    )
    
    cat("\n")
    
  }
  
}

###############################################################################
# Campaign Summary
###############################################################################

campaignTime <-
  
  as.numeric(
    
    difftime(
      
      Sys.time(),
      
      campaignStart,
      
      units = "mins"
      
    )
    
  )

cat("\n")
cat("=========================================================\n")
cat("EXPERIMENTAL CAMPAIGN FINISHED\n")
cat("=========================================================\n\n")

cat(
  
  sprintf(
    
    "Benchmarks .............. %d\n",
    
    length(benchmarks)
    
  )
  
)

cat(
  
  sprintf(
    
    "Algorithms .............. %d\n",
    
    length(algorithms)
    
  )
  
)

cat(
  
  sprintf(
    
    "Independent Runs ........ %d\n",
    
    RUNS
    
  )
  
)

cat(
  
  sprintf(
    
    "Total Executions ........ %d\n",
    
    length(benchmarks) *
      length(algorithms) *
      RUNS
    
  )
  
)

cat(
  
  sprintf(
    
    "Elapsed Time ............ %.2f minutes\n",
    
    campaignTime
    
  )
  
)

cat("\n")
cat("Results saved in:\n")
cat("   ../results/summary/\n")
cat("   ../results/history/\n")
cat("\n")
cat("=========================================================\n")