###define the shapes of the network node
library(igraph); 
source("./nodeShapes.R"); 


###execute the visualization of network
showSystemNetwork <- function(systemData){
  networkData <- calculateSystemNetwork(systemData);
  plot <- visualizeNetwork(systemData,networkData); 

}


###create the network data from the system information
calculateSystemNetwork <- function(systemData){
  ##igraph package required
  library(igraph)
  ##matrix DP X {constraint, compoent}
  constDPMatrix <- systemData@constDPMatrix;
  entityDPMatrix <- systemData@entityDPMatrix;
  systemMatrix <-deliveryData <- merge(constDPMatrix, entityDPMatrix, by.x="row.names", by.y="row.names");
  row.names(systemMatrix) <- systemMatrix[,1];
  systemMatrix <- systemMatrix[,-1];

  ##generate network graph from systemMatrix
  networkData <- graph_from_incidence_matrix(systemMatrix, weight=TRUE); #weight for propagation path
  
  ##define the type and label of nodes 
  vertexNames <- V(networkData)$name;
  # DP types (requirement DP:1, attribute DP:2)  
  dpData <- matrix(vertexNames);
  rownames(dpData) <- dpData[,1];
  dpData <- merge(dpData, systemData@dpList, by.x="row.names", by.y="row.names");
  dpTypes <- dpData[,6];
  dpTypes <- ifelse(dpTypes=="R", 1, 2); 
  dpNames <- dpData[,3];

  #constraint: 3
  constData <- matrix(vertexNames);
  rownames(constData) <- constData[,1];
  constData <- merge(constData, systemData@constList, by.x="row.names", by.y="row.names");
  constTypes <- as.numeric(paste(rep(3,nrow(systemData@constList))));
  constNames <- constData[,3];
  
  #component:4, function:5
  entityData <- matrix(vertexNames);
  rownames(entityData) <- entityData[,1];
  entityData <- merge(entityData, systemData@entityList, by.x="row.names", by.y="row.names");
  entityTypes <- entityData[,4];
  entityTypes <- ifelse(entityTypes=="C", 4, 5); 
  entityNames <- entityData[,3];
  
  V(networkData)$types <- c(dpTypes,constTypes,entityTypes);
  V(networkData)$labels <- c(dpNames,constNames,entityNames);
  #edge type sold:1, 5: 
  E(networkData)$lty <- ifelse(is.na(match(ends(networkData, E(networkData))[,2], row.names(systemData@entityList))),1,6);

  return(networkData)
}
  

###visualize the network of system information
visualizeNetwork <- function(systemData, networkData){
  
  ##define the nodes (color, shape, label)
  # myXX <- user defined shape
  V(networkData)$color <- c("palegreen3", "steelblue","slategray", "tomato", "slateblue")[V(networkData)$types];
  V(networkData)$shape <- c("mytriangle", "circle","myrectangle", "mydiamond","mydiamond")[V(networkData)$types];
  V(networkData)$label.cex=.6 #label size
  V(networkData)$label.font=2

  ##define the edges (color, thickness, label)
  E(networkData)$width <- E(networkData)$weight;#thickness of edges following their weight 
  E(networkData)$color <-  ifelse(E(networkData)$width==1, "gray50", "red"); #default color of edges

  ##visualize the graph
  set.seed(123) #fix the graph 
  networkGraph <- plot(networkData,
                 layout = layout_with_dh(
                   networkData,
                   weight.node.dist = 1), 
                 vertex.label.color="black", 
                 vertex.frame.color=0,
                 vertex.size=5);
  #show group  
  ##plot(network, mark.groups=list(c(1,4,5,8), c(15:17)),mark.col=c("#C5E5E7","#ECD89A"), mark.border=NA)

  return(networkGraph);  
}
