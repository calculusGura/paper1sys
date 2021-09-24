library(visNetwork)

nodes <- data.frame(id = 1:4)
edges <- data.frame(from = c(2,4,3,3), to = c(1,2,4,2))

visNetwork(nodes, edges, width = "100%") %>% 
  visNodes(shape = "square", 
           color = list(background = "lightblue", 
                        border = "darkblue",
                        highlight = "yellow"),
           shadow = list(enabled = TRUE, size = 10)) %>%
  visLayout(randomSeed = 12) # to have always the same network




nodes <- data.frame(id = 0:5,label=0:5)
edges <- data.frame( from=c(0,0,0,1,2,3,1,2,4),to=c(2,3,5,5,5,5,3,4,5))
visNetwork(nodes, edges)%>% visHierarchicalLayout(direction = "LR")  %>% visEdges(arrows = "to")
visNetwork::visIgraphLayout(visNetwork(nodes, edges) %>% visEdges(arrows = "to") %>%
                              visEvents(selectNode = "function(properties) {
      alert('selected nodes ' + this.body.data.nodes.get(properties.nodes[0]).id);}"))


visIgraph(igraph_network)


data <- toVisNetworkData(networkData)
visNetwork::visIgraphLayout(
  visNetwork(nodes = data$nodes[,c(1,3,5)], edges = data$edges) %>%
    visNodes(#shape = "square", 
           color = list(background = "lightblue", 
                        border = "darkblue",
                        highlight = "yellow"),
           shadow = list(enabled = TRUE, size = 10)) 
  )



library(htmlwidgets)
saveWidget(p, file=paste0(getwd(),"/networkInteractive1.html"))


nodes <- data.frame(id = 1:10, 
                    label = paste("Node", 1:10),                                 # add labels on nodes
                    group = c("GrA", "GrB"),                                     # add groups on nodes 
                    value = 1:10,                                                # size adding value
                    shape = c("square", "triangle", "box", "circle", "dot", "star",
                              "ellipse", "database", "text", "diamond"),                   # control shape of nodes
                    title = paste0("<p><b>", 1:10,"</b><br>Node !</p>"),         # tooltip (html or character)
                    color = c("darkred", "grey", "orange", "darkblue", "purple"),# color
                    shadow = c(FALSE, TRUE, FALSE, TRUE, TRUE))                  # shadow

head(nodes)

edges <- data.frame(from = sample(1:10, 8), to = sample(1:10, 8),
                    label = paste("Edge", 1:8),                                 # add labels on edges
                    length = c(100,500),                                        # length
                    arrows = c("to", "from", "middle", "middle;to"),            # arrows
                    dashes = c(TRUE, FALSE),                                    # dashes
                    title = paste("Edge", 1:8),                                 # tooltip (html or character)
                    smooth = c(FALSE, TRUE),                                    # smooth
                    shadow = c(FALSE, TRUE, FALSE, TRUE))                       # shadow
head(edges)



visNetwork::visIgraphLayout(
  visNetwork(nodes, edges, width = "100%"))



nodes <- data.frame(id = 1:5, group = c(rep("A", 2), rep("B", 3)))
edges <- data.frame(from = c(2,5,3,3), to = c(1,2,4,2))
visNetwork::visIgraphLayout(
  visNetwork(nodes, edges, width = "100%") %>% 
    #visNodes(shape = "square") %>%                        # square for all nodes
    #visEdges(arrows ="to") %>%                            # arrow "to" for all edges
    visGroups(groupname = "A", color = "darkblue", shape = "dot") %>%    # darkblue for group "A"
    visGroups(groupname = "B", color = "darkred", shape = "star")             # red for group "B"
  %>% visLegend() %>%  
    visOptions(nodesIdSelection = TRUE)%>% 
    visInteraction(navigationButtons = TRUE) 
)