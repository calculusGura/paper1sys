###################test case
setwd("/home/buzzsongs/paper1sys"); ##the working directory for chrome OS
impact <- read.csv("impact.csv");
likelihood <- read.csv("likelihood.csv");

rownames(impact) <- impact[,1]
impact <- impact[,-1]
rownames(likelihood) <- likelihood[,1]
likelihood <- likelihood[,-1]


showChangeRiskMatrix <- function(impact, likelihood){

  entities <- nrow(impact)
  
  x1 <-NULL; x2 <-NULL;
  y1 <-NULL; y2 <-NULL;
  
  #define the origin x,y location of each entity
  x=seq(0.5, (entities - 0.5), 1);
  y=seq((entities - 0.5), 0.5, -1);
  
  
  for(i in 1:entities){
    x1 <- c(x1,rep(x[i], entities));
    x2 <- c(x2,x[i]+likelihood[,i]);
    
    y1 <- c(y1, y);
    y2 <- c(y2, y+impact[,i])
  }


  cells <- data.frame(x1,x2,y1,y2);
  risk <- (cells[,2] - cells[,1]) * (cells[,4] - cells[,3]);
  risk[which((risk>=0) & (risk<=0.3))] <- "green";
  risk[which((risk>0.3) & (risk<=0.55))] <- "yellow";
  risk[which((risk>0.55) & (risk<=1))] <- "red";
  cells <- cbind(cells, risk);
  
  diagCell <- as.matrix(impact);
  diagCell[,] <- 0;
  diagCell[seq(1,length(diagCell), (entities+1))] <- 1

  graph <- graph_from_adjacency_matrix(diagCell, mode="directed", diag=TRUE);
  node_list <- get.data.frame(graph, what = "vertices");
  edge_list <- get.data.frame(graph, what = "edges");
  all_nodes <- node_list$name;
  
  plot_data <- edge_list %>% mutate(
    to = factor(to, levels = rev(all_nodes)),
    from = factor(from, levels = all_nodes));
browser();
  ggplot(plot_data, aes(x = from, y = to), fill="white", alpha=0) +
    geom_raster() +
    theme_bw() +
    geom_rect(data = cells,
              aes(xmin = x1, xmax = x2, ymin = y2, ymax = y1),
              alpha = .7, inherit.aes = FALSE, fill=risk) +
    #labels on x-axis move to the top
    scale_x_discrete(position = "top", drop = FALSE) +
    #order labels on y-axis reversly
    scale_y_discrete(position = "left", drop = FALSE) +
    #define guide line on the matrix 
    geom_vline(xintercept = seq(0.5, entities+0.5,1), lwd=0.5, colour="grey")+
    geom_hline(yintercept=seq(0.5, entities+0.5, 1), lwd=0.5, colour="grey") +
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
}

showChangeRiskMatrix(impact, likelihood)







  