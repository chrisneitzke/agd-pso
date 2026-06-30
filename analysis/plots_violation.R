###############################################################################
#
# AGD-PSO Framework
#
# File:
#    plots_violation.R
#
# Description:
#    Average constraint violation curves.
#
###############################################################################

plotViolation <- function(historyData){
  
  cat("Generating violation plots...\n")
  
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
        
        MeanViolation =
          
          mean(
            
            MeanViolation,
            
            na.rm = TRUE
            
          ),
        
        SD =
          
          sd(
            
            MeanViolation,
            
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
          
          y = MeanViolation,
          
          colour = Algorithm,
          
          fill = Algorithm
          
        )
        
      ) +
      
      geom_ribbon(
        
        aes(
          
          ymin =
            
            pmax(
              
              MeanViolation - SD,
              
              0
              
            ),
          
          ymax =
            
            MeanViolation + SD
          
        ),
        
        alpha = 0.15,
        
        colour = NA
        
      ) +
      
      geom_line(
        
        linewidth = 1.1
        
      ) +
      
      scale_x_continuous(
        
        labels = scales::comma
        
      ) +
      
      labs(
        
        title =
          
          paste(
            
            benchmark,
            
            "- Constraint Violation"
            
          ),
        
        x =
          
          "Number of Function Evaluations (NFE)",
        
        y =
          
          "Mean Constraint Violation"
        
      ) +
      
      theme_bw(
        
        base_size = 12
        
      ) +
      
      theme(
        
        legend.position = "bottom",
        
        plot.title =
          
          element_text(
            
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
            
            "violation_",
            
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