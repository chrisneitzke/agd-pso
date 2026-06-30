###############################################################################
#
# AGD-PSO Framework
#
# File:
#    plots_diversity.R
#
# Description:
#    Average population diversity curves.
#
###############################################################################

plotDiversity <- function(historyData){
  
  cat("Generating diversity plots...\n")
  
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
        
        MeanDiversity =
          
          mean(
            
            Diversity,
            
            na.rm = TRUE
            
          ),
        
        SD =
          
          sd(
            
            Diversity,
            
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
          
          y = MeanDiversity,
          
          colour = Algorithm,
          
          fill = Algorithm
          
        )
        
      ) +
      
      geom_ribbon(
        
        aes(
          
          ymin =
            
            pmax(
              
              MeanDiversity - SD,
              
              0
              
            ),
          
          ymax =
            
            MeanDiversity + SD
          
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
            
            "- Population Diversity"
            
          ),
        
        x =
          
          "Number of Function Evaluations (NFE)",
        
        y =
          
          "Mean Diversity"
        
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
            
            "diversity_",
            
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