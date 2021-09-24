testCase <- function(systemData, iteration){
  
  for(i in 1: iteration){

    path <- explorePropagationPath(c("D1"), systemData);
    path <- evaluatePathRisk(path, systemData);
    
    browser();
    
  }
    
  
}

