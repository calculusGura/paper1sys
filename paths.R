evaluateRisk <-function(systemData, paths){

  for (anEntity in rownames(systemData@entityList)){
    pathsOnAnEntity <- paths[,names(which(systemData@entityDPMatrix[,anEntity] == 1))];
    evaluateLikelihood(pathsOnAnEntity);
    evaluateImpact(systemData,anEntity, pathsOnAnEntity)
  }
}

evaluateLikelihood <- function(pathsOnAnEntity){
  likelihood <- length(which(rowSums(pathsOnAnEntity) > 0)) / nrow(pathsOnAnEntity)
  return(likelihood);
}

evaluateImpact  <-function(systemData, anEntity, pathsOnAnEntity){
  dpCriticality <- systemData@dpList[names(which(systemData@entityDPMatrix[,anEntity] ==1)),3];
  dpCriticality <- matrix(rep(dpCriticality,4), ncol=8, byrow=TRUE);
  pathCriticality <- pathsOnAnEntity * dpCriticality;
  impact <- sum(pathCriticality) / sum(dpCriticality);
  browser();
  return(impact);
}







path1 <- explorePropagationPath(c("D1"), systemData);
path2 <- explorePropagationPath(c("D1"), systemData);
path3 <- explorePropagationPath(c("D1"), systemData);
path4 <- explorePropagationPath(c("D1"), systemData);

path1 <- rowSums(path1@constDPMatrix);
path2 <- rowSums(path2@constDPMatrix);
path3 <- rowSums(path3@constDPMatrix);
path4 <- rowSums(path4@constDPMatrix);

path1[which(!path1==0)] <- 1
path2[which(!path2==0)] <- 1
path3[which(!path3==0)] <- 1
path4[which(!path4==0)] <- 1

paths <- rbind(path1, path2, path3, path4)

#firstIndex <- grep(rownames(head(productData@compList,1)), colnames(costData))[1];
evaluateRisk(systemData, paths)