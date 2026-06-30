###############################################################################
#
# AGD-PSO Framework
#
# File:
#    plots_convergence.R
#
# Description:
#    Average convergence curves.
#
###############################################################################

plotConvergence <- function(historyData){
  
  cat("Generating convergence plots...\n")
  
  ###########################################################################
  # Benchmarks
  ###########################################################################
  
  benchmarks <-
    
    sort(
      
      unique(historyData$Benchmark)
      
    )
  
  ###########################################################################
  
  for(benchmark in benchmarks){
    
    #######################################################################
    # Aggregate Runs
    #######################################################################
    
    data <-
      
      historyData |>
      
      dplyr::filter(
        
        Benchmark == benchmark
        
      ) |>
      
      dplyr::group_by(
        
        Algorithm,
        NFE
        
      ) |>
      
      dplyr::summarise(
        
        MeanBest =
          
          mean(
            
            BestFitness,
            
            na.rm = TRUE
            
          ),
        
        SD =
          
          sd(
            
            BestFitness,
            
            na.rm = TRUE
            
          ),
        
        .groups = "drop"
        
      )
    
    #######################################################################
    # Plot
    #######################################################################
    
    p <-
      
      ggplot(
        
        data,
        
        aes(
          
          x = NFE,
          
          y = MeanBest,
          
          colour = Algorithm,
          
          fill = Algorithm
          
        )
        
      ) +
      
      geom_ribbon(
        
        aes(
          
          ymin = MeanBest - SD,
          
          ymax = MeanBest + SD
          
        ),
        
        alpha = 0.15,
        
        colour = NA
        
      ) +
      
      geom_line(
        
        linewidth = 1.1
        
      ) +
      
      labs(
        
        title =
          
          paste(
            
            benchmark,
            
            "- Mean Convergence"
            
          ),
        
        x =
          
          "Number of Function Evaluations (NFE)",
        
        y =
          
          "Best Fitness"
        
      ) +
      
      theme_bw(
        
        base_size = 12
        
      ) +
      
      theme(
        
        legend.position = "bottom",
        
        plot.title = element_text(
          
          face = "bold"
          
        )
        
      )
    
    #######################################################################
    # Save
    #######################################################################
    
    ggsave(
      
      filename =
        
        file.path(
          
          "output",
          
          "figures",
          
          paste0(
            
            "convergence_",
            
            benchmark,
            
            ".pdf"
            
          )
          
        ),
      
      plot = p,
      
      width = 8,
      
      height = 5
      
    )
    
  }
  
}