
#test case 
test <- matrix(0, ncol=4, nrow= 100);
test <- as.data.frame(test);
test[,1] <- paste0("P",1:nrow(test));

test[,2] <- runif(100, min=0, max=100);
test[,3] <- runif(100, min=0, max=100);
test[,4] <- runif(100, min=0, max=100);
colnames(test) <- c("name", "quality", "cost", "delivery")



#test
p <- ggplot(data=test, aes(x=cost, y=delivery)) + 
  geom_point(aes(colour=quality), size=3, alpha=0.8) +
  geom_point(data=test, aes(colour=quality), size=3, alpha=0.8) +
    scale_colour_gradient(low = "red", high = "blue") +
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA)) 
ggplotly(p)




