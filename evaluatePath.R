evaluatePathRisk <- function(path, systemData){

  usedDPs   <-  path@dps
  dpList    <-  systemData@dpList
  requirementConflict <- length(grep("R", dpList[usedDPs, "type"])); #check how many requirement dps are included in the path
  solutionArea        <- calculateSolutionArea(path, systemData);
  requiredResource    <- evaluateRequiredResource(path, systemData);

  #input the result into the path instance
  path@requirementConflict   <- requirementConflict;
  path@solutionArea          <- solutionArea;
  path@requiredResource      <- requiredResource;

  return(path);
}



###calculate the solution area of a path 
calculateSolutionArea <- function(path, systemData){

  solutionArea <- colSums(path@constDPMatrix) / colSums(systemData@constDPMatrix);
  #remove constraints that are not involved the path
  solutionArea <- subset(solutionArea, solutionArea>0);
  solutionArea <- geoMeans(solutionArea);
  
  return(solutionArea)
}

###geometric mean functions for calculation of solution area 
geoMeans= function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}




###evaluate the required resources of a path 
evaluateRequiredResource <- function(path, systemData){
  
  usedDPs <- path@dps

  ##prepare the base data matrix for the calculation
  baseData <- merge(systemData@dpList[,], systemData@entityDPMatrix[,], 
                    by.x="row.names", by.y="row.names");
  
  row.names(baseData) <- baseData$Row.names;
  baseData <- baseData[,-1];
  
  firstIndex <- grep(rownames(head(systemData@entityList,1)), colnames(baseData))[1];
  lastIndex <- grep(rownames(tail(systemData@entityList,1)), colnames(baseData));
  
  baseData <- cbind(baseData[c("type", "importance", "costSens")],
                    baseData[firstIndex:lastIndex]) 
  
  ##calculate the required resource
  firstIndex <- grep(rownames(head(systemData@entityList,1)), colnames(baseData))[1];
  lastIndex <- grep(rownames(tail(systemData@entityList,1)), colnames(baseData));
  
  entityData <- systemData@entityList["criticality"];
  
  ##equation of evaluation
  baseData[,(firstIndex:lastIndex)] <- (baseData[,(firstIndex:lastIndex)] 
                                        * baseData[,"costSens"]);
  
  changeScale <- (colSums(baseData[usedDPs,(firstIndex:lastIndex)]) 
                  / colSums(baseData[,(firstIndex:lastIndex)]));
  changeScale[is.nan(changeScale)] <- 0
  
  requiredResource <-  entityData * changeScale;
  requiredResource <- sum(requiredResource);
  
  return(requiredResource);
  
}














