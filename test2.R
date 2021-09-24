#test case 
test <- matrix(0, ncol=4, nrow= 100);
test <- as.data.frame(test);
test[,1] <- paste0("P",1:nrow(test));

test[,2] <- runif(100, min=0, max=100);
test[,3] <- runif(100, min=0, max=100);

colnames(test) <- c("name", "x", "y","pa");
#-----------------------------------------

pareto = test[order(test$x,decreasing=TRUE),]
front = pareto[which(!duplicated(cummin(pareto$y))),]
pareto[which(!duplicated(cummax(pareto$y))),"pa"] <- 1


p <- ggplot(data=pareto, aes(x=x, y=y)) + 
  geom_point(data=pareto, aes(colour=pa), size=3, alpha=0.8)
ggplotly(p)


#-----------------------------------------

pareto.2 <- logical(length(x))
x.sort <- sort(x)
y.sort <- y[order(x)]
y.min <- max(y)
for(i in 1:length(x.sort)) {
  if(pareto.2[i] <- y.sort[i] <= y.min) y.min <- y.sort[i]
}    

#-----------------------------------------


p <- ggplot(data=pareto, aes(x=x, y=y)) + 
  geom_point(data=pareto, aes(colour=pa), size=3, alpha=0.8)
ggplotly(p)

aes(colour=quality), size=3, alpha=0.8) +
  geom_point(data=test2, aes(colour=quality), size=3, alpha=0.8) +
  scale_colour_gradient(low = "red", high = "blue") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA)) 
ggplotly(p)







#test case 
test <- matrix(0, ncol=5, nrow= 100);
test <- as.data.frame(test);
test[,1] <- paste0("P",1:nrow(test));

test[,2] <- runif(100, min=0, max=100);
test[,3] <- runif(100, min=0, max=100);
#test[,4] <- runif(100, min=0, max=100);


colnames(test) <- c("name", "x", "y","z","pa");
#-----------------------------------------


pareto = test[order(test$x,decreasing=TRUE),]
pareto[which(!duplicated(cummax(pareto$y))),"pa"] <- 1

pareto = test[order(test$y,decreasing=TRUE),]
#pareto[which(!duplicated(cummax(pareto$z))),"pa"] <- 1

pareto = test[order(test$z,decreasing=TRUE),]
#pareto[which(!duplicated(cummax(pareto$x))),"pa"] <- 1


plot_ly(pareto, x = ~x, y = ~y, z = ~z,
        marker = list(size =2,  #plotSize
                      color = ~pa, 
                      showscale = TRUE))

#-----------------------------------------

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



