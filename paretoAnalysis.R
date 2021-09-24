#test case 
test <- matrix(0, ncol=3, nrow= 100);
test <- as.data.frame(test);
test[,1] <- paste0("P",1:nrow(test));

test[,2] <- runif(100, min=0, max=100);
test[,3] <- runif(100, min=0, max=100);
test[,4] <- runif(100, min=0, max=100);

colnames(test) <- c("name", "x", "y","z");


### find Pareto frontier
findParetoFrontier <- function(plotData){
  
  pa <- matrix(0, nrow= nrow(plotData));
  plotData <- cbind(plotData,pa);

  plotData = plotData[order(plotData$x,decreasing=TRUE),]
  plotData[which(!duplicated(cummax(plotData$y))),"pa"] <- 1
  
  plotData = plotData[order(plotData$y,decreasing=TRUE),]
  plotData[which(!duplicated(cummax(plotData$z))),"pa"] <- 1
  
  plotData = plotData[order(plotData$z,decreasing=TRUE),]
  plotData[which(!duplicated(cummax(plotData$x))),"pa"] <- 1
  

  return(plotData)
}
  
pareto<- findParetoFrontier(test)



figure <- plot_ly(pareto, x = ~x, y = ~y, z = ~z,
                  marker = list(size =5,  #plotSize
                                color = ~pa, 
                                showscale = TRUE));



figure <- add_markers(figure, color = ~pa, colors = c('gray', 'red'));




show3dScatterWithPareto <- function(data){
  figure <- plot_ly(data, x = ~data[,2], y = ~data[,3], z = ~data[,4])
  figure <- add_markers(figure, color = ~data[,5], colors = c('gray', 'red'));
  figure <- figure %>% layout(scene = list(xaxis = list(title = colnames(data)[2]),
                                           yaxis = list(title = colnames(data)[3]),
                                           zaxis = list(title = colnames(data)[4])));  
  
  return(figure)
}






