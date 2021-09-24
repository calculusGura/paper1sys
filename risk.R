effectedEntities <- function(productData, path){
  
  usedDPs <- path@dps;
  entityDPMatrix <- productData@entityDPMatrix;
  
  effectedEntities <- colSums(productData@entityDPMatrix[usedDPs,])
                 

  return(effectedEntities)
  
}


test <- matrix(0, ncol=2, nrow=10)

E1 <- c(3,2,4,5,5,0,3,3,1,0)
E2 <- c(2,1,3,5,6,6,6,2,1,2)
test <- cbind(E1, E2)


effectedEntities(productData, path)

path1 <- exploringPropagationPath(c("D1", "D2"), productData);
path2 <- exploringPropagationPath(c("D1"), productData);
path3 <- exploringPropagationPath(c("D1"), productData);

paths <- path1@constDPMatrix + path2@constDPMatrix + path3@constDPMatrix 
comp2 <- merge(productData@dpList[,], productData@entityDPMatrix[,], 
               by.x="row.names", by.y="row.names");

