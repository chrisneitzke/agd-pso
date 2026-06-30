###############################################################################
#
# AGD-PSO Framework
#
# File:
#    plots_mutation.R
#
# Description:
#    Adaptive mutation rate evolution.
#
###############################################################################

plotMutation <- function(historyData){
  
  cat("Generating mutation plots...\n")
  
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
    # AGD-PSO only
    #######################################################################
    
    data <-
      
      historyData |>
      
      dplyr::filter(
        
        Benchmark == benchmark,
        
        Algorithm == "AGDPSO"
        
      ) |>
      
      dplyr::group_by(
        
        NFE
        
      ) |>
      
      dplyr::summarise(
        
        MeanMutation =
          
          mean(
            
            MutationRate,
            
            na.rm = TRUE
            
          ),
        
        SD =
          
          sd(
            
            MutationRate,
            
            na.rm = TRUE
            
          ),
        
        .groups = "drop"
        
      )
    
    #######################################################################
    # Skip if no data
    #######################################################################
    
    if(nrow(data) == 0){
      
      next
      
    }
    
    #######################################################################
    # Plot
    #######################################################################
    
    p <-
      
      ggplot(
        
        data,
        
        aes(
          
          x = NFE,
          
          y = MeanMutation
          
        )
        
      ) +
      
      geom_ribbon(
        
        aes(
          
          ymin =
            
            pmax(
              
              MeanMutation - SD,
              
              0
              
            ),
          
          ymax =
            
            MeanMutation + SD
          
        ),
        
        alpha = 0.15,
        
        fill = "steelblue"
        
      ) +
      
      geom_line(
        
        linewidth = 1.1,
        
        colour = "steelblue"
        
      ) +
      
      scale_x_continuous(
        
        labels = scales::comma
        
      ) +
      
      coord_cartesian(
        
        ylim = c(
          
          0,
          
          0.05
          
        )
        
      ) +
      
      labs(
        
        title =
          
          paste(
            
            benchmark,
            
            "- Adaptive Mutation Rate"
            
          ),
        
        x =
          
          "Number of Function Evaluations (NFE)",
        
        y =
          
          "Mutation Rate"
        
      ) +
      
      theme_bw(
        
        base_size = 12
        
      ) +
      
      theme(
        
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
            
            "mutation_",
            
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