emptlyRiskMatirx <- function(data){
  elementList  <- colnames(data);
  plot_ly(x=elementList, y=elementList, type = "heatmap")   + geom_raster() 
  browser(); 
  
  elementList  <- as.data.frame(elementList);

  matrix <- ggplot(elementList, aes(elementList, elementList))+
    geom_raster() +
    theme_bw() +
    #labels on x-axis move to the top
    scale_x_discrete(position = "top", drop = FALSE) +
    #order labels on y-axis reversly
    scale_y_discrete(limits=rev, drop = FALSE) +
    #define guide line on the matrix 
    geom_vline(xintercept = seq(0.5, nrow(elementList)+0.5,1), lwd=0.5, colour="grey")+
    geom_hline(yintercept=seq(0.5, nrow(elementList)+0.5, 1), lwd=0.5, colour="grey") +
    theme(
      # remove x/y-axis title
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      # rotate the labels of x-axis 
      axis.text.x = element_text(angle = 70, hjust = 0),
      # Force the plot into a square aspect ratio
      aspect.ratio = 1,
      panel.grid.major = element_blank(),
      # Hide the legend (optional)
      legend.position = "none");
  return(matrix)  
}

