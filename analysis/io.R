###############################################################################
#
# File:
#    io.R
#
###############################################################################

library(readr)
library(dplyr)
library(tools)

loadExperiment <- function(){
  
  ###########################################################################
  # Files
  ###########################################################################
  
  summaryFiles <-
    
    list.files(
      
      "../results/summary",
      
      pattern = "\\.csv$",
      
      full.names = TRUE
      
    )
  
  historyFiles <-
    
    list.files(
      
      "../results/history",
      
      pattern = "\\.csv$",
      
      full.names = TRUE
      
    )
  
  ###########################################################################
  # Summary
  ###########################################################################
  
  summaryData <-
    
    lapply(
      
      summaryFiles,
      
      read_csv,
      
      show_col_types = FALSE
      
    ) |>
    
    bind_rows()
  
  ###########################################################################
  # History
  ###########################################################################
  
  historyData <-
    
    lapply(
      
      historyFiles,
      
      function(file){
        
        data <-
          
          read_csv(
            
            file,
            
            show_col_types = FALSE
            
          )
        
        ################################################################
        
        name <-
          
          file_path_sans_ext(
            
            basename(file)
            
          )
        
        parts <-
          
          strsplit(
            
            name,
            
            "_"
            
          )[[1]]
        
        ################################################################
        
        data$Benchmark <- parts[1]
        
        data$Algorithm <- parts[2]
        
        data$Run <-
          
          as.integer(
            
            sub(
              
              "run",
              
              "",
              
              parts[3]
              
            )
            
          )
        
        ################################################################
        
        data
        
      }
      
    ) |>
    
    bind_rows()
  
  ###########################################################################
  
  list(
    
    summary = summaryData,
    
    history = historyData
    
  )
  
}