###explore a propagation path
explorePropagationPath <- function(startDPs, systemData){
  
  #define wrapper class of path
  propagationPath <- new("PropagationPath",
                         path = list(startDPs),
                         dps = startDPs,
                         constDPMatrix = systemData@constDPMatrix);
  #iterating the engineering decision
  check <- ncol(propagationPath@constDPMatrix);  
  while(check !=0){
    propagationPath <- engineeringDecision(propagationPath);
    check <- ncol(propagationPath@constDPMatrix); 
  }

  #write down the DPs and constraints in the propagation path
  #with elimination of duplication
  propagationPath@dps <- unique(propagationPath@dps); 
  propagationPath@constraints <- unique(propagationPath@constraints);

  #express the path as matrix format
  pathMatrix <- systemData@constDPMatrix;
  pathMatrix[propagationPath@dps, ] <- pathMatrix[propagationPath@dps, ] + 1; 
  pathMatrix[which(pathMatrix>0)] <- ifelse(pathMatrix[which(pathMatrix>0)]==1, 0, 1); 
  propagationPath@constDPMatrix <- pathMatrix;
   
  return(propagationPath);
}


##decide the direction of propagation on a constraint
engineeringDecision <- function(propagationPath){
  
  candidateDPs <- NULL;
  dps <- unlist(tail(propagationPath@path,1));
  constDPMatrix <- propagationPath@constDPMatrix;

  #delete constraints not connected to target DPs
  relatedConstraints <- constDPMatrix[dps,,drop=FALSE]; 
  relatedConstraints <- constDPMatrix[,which(colSums(relatedConstraints) > 0), drop=FALSE];

  usedConstraints <- colnames(relatedConstraints);
  constDPMatrix   <- constDPMatrix[, !colnames(constDPMatrix) %in% usedConstraints, drop=FALSE];

  #candidateDPs <- rownames(relatedConstraints[which(rowSums(relatedConstraints) > 0), ,drop=FALSE]);
  candidateDPs <- names(which(rowSums(relatedConstraints) > 0));
  candidateDPs <- setdiff(candidateDPs, propagationPath@dps);
  
  #finish the iteration if there is no candidate DP 
  if(length(candidateDPs) == 0){
    propagationPath@constDPMatrix <- matrix(ncol=0, nrow=0); #check -> 0
    return(propagationPath)};

  #select from the candiate DPs
  selectedDPs <- sample(candidateDPs, sample(length(candidateDPs),1));
  
  #write the decision on the path class
  propagationPath@path <- append(propagationPath@path, list(usedConstraints)); 
  propagationPath@path <- append(propagationPath@path, list(selectedDPs)); 
  propagationPath@dps  <- append(propagationPath@dps, selectedDPs); 
  propagationPath@constraints   <- append(propagationPath@constraints, usedConstraints); 
  propagationPath@constDPMatrix <- constDPMatrix;
  
  return(propagationPath);
}

