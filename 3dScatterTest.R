
install.packages("scatterplot3d")
library(scatterplot3d)
a<-scatterplot3d(pareto[,1:3], 
                 color = "#999999",
                 pch = 19, 
                 main="3D Scatter Plot",
                 xlab = "RC",
                 ylab = "RR",
                 zlab = "SA");

a$points3d(pareto1[,1:3], col = "red", pch = 19)



library(plotly)

mtcars$am[which(mtcars$am == 0)] <- 'Automatic'
mtcars$am[which(mtcars$am == 1)] <- 'Manual'
mtcars$am <- as.factor(mtcars$am)

fig <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec, color = ~am, colors = c('#BF382A', '#0C4B8E'))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weight'),
                                   yaxis = list(title = 'Gross horsepower'),
                                   zaxis = list(title = '1/4 mile time')))


fig <- plot_ly(mtcars, x = ~wt, y = ~hp, z = ~qsec,
               marker = list(color = ~mpg, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Weight'),
                                   yaxis = list(title = 'Gross horsepower'),
                                   zaxis = list(title = '1/4 mile time')),
                      annotations = list(
                        x = 1.13,
                        y = 1.05,
                        text = 'Miles/(US) gallon',
                        xref = 'paper',
                        yref = 'paper',
                        showarrow = FALSE
                      ))
fig


show3dScatterWithPareto <- function(data){
  figure <- plot_ly(data, x = ~data[,2], y = ~data[,3], z = ~data[,4])
  figure <- add_markers(figure, color = ~data[,5], colors = c('gray', 'red'));
  figure <- figure %>% layout(scene = list(xaxis = list(title = colnames(data)[2]),
                                        yaxis = list(title = colnames(data)[3]),
                                        zaxis = list(title = colnames(data)[4])));  
  
  return(figure)
}
show3dScatterWithPareto(pareto1)
