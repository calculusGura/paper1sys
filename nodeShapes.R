### definition of node shapes 

##triangle vertex
mytriangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/150 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], fg=vertex.color, bg=vertex.color,
          stars = cbind(vertex.size, vertex.size, vertex.size),
          add=TRUE, inches=FALSE)
}

# clips as a circle
#add_shape("mytriangle", clip=shape_noclip, plot=mytriangle, parameters=list(vertex.norays=5))
add_shape("mytriangle", clip=shapes("circle")$clip,
          plot=mytriangle)




##diamond vertex
myDiamond <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], fg=vertex.color, bg=vertex.color,
          stars = cbind(vertex.size+0.1, vertex.size+0.03,vertex.size+0.1, vertex.size+0.03),
          xlim=c(vertex.size+1,2), ylim=c(vertex.size+2,1),
          add=TRUE, inches=FALSE)
}

# clips as a circle
add_shape("mydiamond", clip=shapes("circle")$clip,
          plot=myDiamond)



##rectangle vertex
myRectangle <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  
  symbols(x=coords[,1], y=coords[,2], fg=vertex.color, bg=vertex.color,
          rectangles = cbind(vertex.size+0.1, vertex.size+0.03),
          add=TRUE, inches=FALSE)
}

# clips as a circle
add_shape("myrectangle", clip=shapes("circle")$clip,
          plot=myRectangle)





## generic star vertex shape, with a parameter for number of rays
mystar <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
  vertex.size  <- 1/200 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  norays <- params("vertex", "norays")
  if (length(norays) != 1 && !is.null(v)) {
    norays <- norays[v]
  }
  
  mapply(coords[,1], coords[,2], vertex.color, vertex.size, norays,
         FUN=function(x, y, bg, size, nor) {
           symbols(x=x, y=y, bg=bg,
                   stars=matrix(c(size,size/2), nrow=1, ncol=nor*2),
                   add=TRUE, inches=FALSE)
         })
}
# no clipping, edges will be below the vertices anyway
add_shape("star", clip=shape_noclip,
          plot=mystar, parameters=list(vertex.norays=5))
