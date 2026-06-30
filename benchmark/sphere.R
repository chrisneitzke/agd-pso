###############################################################################
# Sphere benchmark (development only)
###############################################################################

Sphere <- createBenchmark(
  
  name = "Sphere",
  
  dimension = 5,
  
  lowerBound = rep(-100,5),
  
  upperBound = rep(100,5),
  
  knownOptimum = 0,
  
  evaluateFunction = function(x){
    
    obj <- sum(x^2)
    
    constraints <- numeric(0)
    
    violation <- 0
    
    list(
      
      objective = obj,
      
      constraints = constraints,
      
      violation = violation,
      
      feasible = TRUE
      
    )
    
  }
  
)