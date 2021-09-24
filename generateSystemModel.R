###importing csv data of Entity - DP relationships
generateEntityDPModel <- function(csvData){
  dataLength <- nrow(csvData); 
  ##define data slots 
  entityList <- list();
  dpList <- list();
  entityCodeList <- list();
  dpCodeList <- list();
  
  ##split CSV data & add them into the each list
  for(i in 1: dataLength){
    if(csvData[i,1] !=""){
      #name, type, criticality,status
      entityList <- c(entityList, c(as.character(csvData[i,1]), csvData[i,2], csvData[i,3],NA))};
      #name, importance, sensitivity, type
      dpList <- c(dpList, 
                c(as.character(csvData[i,4]), csvData[i,5], csvData[i,6],as.character(csvData[i,7]),NA));
  }  
  
  ##list of entity data -> data.frame
  entityList <- matrix(unlist(entityList), ncol=4, byrow=TRUE);
  colnames(entityList) <- c("name", "type", "criticality", "status");
  entityCodeList <- paste0("E",1:nrow(entityList));
  rownames(entityList) <- entityCodeList;
  #change data type for data.frame
  entityList <- data.frame(entityList);
  entityList$name <- as.character(entityList$name);
  entityList$criticality <- as.numeric(as.character(entityList$criticality));
  
  ##list of DP data -> data.frame
  dpList <- matrix(unlist(dpList), ncol=5, byrow=TRUE);
  colnames(dpList) <- c("name", "importance", "costSens", "type", "status");
  dpCodeList <- paste0("D",1:nrow(dpList)); #generate DP code
  rownames(dpList) <- dpCodeList; 
  #change data type for data.frame
  dpList <- data.frame(dpList);
  dpList$name <- as.character(dpList$name);
  dpList$importance <- as.numeric(as.character(dpList$importance));
  dpList$costSens <- as.numeric(as.character(dpList$costSens));
  dpList$type <- as.character(dpList$type);
  
  ##define entity-DP relationships on the matrix
  #initialize the matrix
  entityDPMatrix <- matrix(0,nrow = nrow(dpList), ncol = nrow(entityList));
  rownames(entityDPMatrix) <- dpCodeList;
  colnames(entityDPMatrix) <- entityCodeList;
  #add the relationship data into the matrix
  count <- 0;
  for(i in 1: dataLength){
    if(csvData[i,1] !=""){count <- count + 1;}
    entityDPMatrix[i, count] <- 1;
  }  

  ##return generated model
  systemData <- new("SystemData",
                    entityList = entityList, 
                    dpList = dpList, 
                    entityDPMatrix= entityDPMatrix);
  
  return(systemData);
}




###importing csv data of Constraint - DP relationships
generateConstDPModel <- function(csvData, systemData){
  
  dataLength <- nrow(csvData);
  constList <- list();
  
  for(i in csvData[which(csvData[,1] != ""),1]){
    constList <- c(constList, as.character(i));
  }

  ##constraint list
  constList <- matrix(unlist(constList), ncol=1, byrow=TRUE);
  colnames(constList) <- c("name");
  constCodeList <- paste0("Cst",1:nrow(constList)); #generate constraint code
  rownames(constList) <- constCodeList;
  ##list -> data.frame
  constList <- data.frame(constList);
  constList$name <- as.character(constList$name);
  
  ##define const-DP relationships on the matrix
  #initialize the matrix
  constDPMatrix <- matrix(0,nrow = nrow(systemData@dpList), ncol = nrow(constList));
  rownames(constDPMatrix) <- rownames(systemData@dpList);
  colnames(constDPMatrix) <- rownames(constList);
  #add the relationship data into the matrix
  count <- 0;
  for(i in 1: dataLength){
    if(csvData[i,1] !=""){count <- count + 1;}
    dpId <- which((systemData@dpList)[,1] == csvData[i,2]);
    constDPMatrix[dpId, count] <- 1;
  }

  systemData@constList <- constList;
  systemData@constDPMatrix <- constDPMatrix; 
  systemData@dpdpMatrix <- transformToDPDPmatrix(constDPMatrix); #matrix of dp-dp relationships
  
  return(systemData);
}


###Transform DP-constraint matrix to DP-DP matrix  
transformToDPDPmatrix <-function(dpConstMatrix){
  numberOfConst <- ncol(dpConstMatrix);
  numberOfDP <- nrow(dpConstMatrix);
  
  # generate 0-matrix with DP names 
  dpdpMatrix <- matrix(nrow = numberOfDP, ncol = numberOfDP, 0);
  rownames(dpdpMatrix) <- rownames(dpConstMatrix);
  colnames(dpdpMatrix) <- rownames(dpConstMatrix);
  
  for(i in 1:numberOfConst) {
    #Deduce DPs from each constraint
    relatedDPs <- which(dpConstMatrix[,i] ==1);
    #Put the constraint number to the cells of the related DPs
    dpdpMatrix[relatedDPs, relatedDPs] <- i; 
  }
  
  # put 0 on the diagonal cells
  diag(dpdpMatrix) <-0;
  return(dpdpMatrix);
}



