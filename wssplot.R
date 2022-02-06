wssplot <- function(data, nc=15, seed=4320)
{
   require(ggplot2)
   x <- 1:nc
   wss <- rep(length(1:nc))
   for (i in 1:nc)
   {
      set.seed(seed)
      wss[i] <- sum(kmeans(data, centers=i, nstart=25)$withinss)
   }
   wssdata <- data.frame(x,wss)
   ggplot(wssdata, aes(x=x, y=wss)) + 
      geom_line() +
      geom_point() +
      labs(x="k", y="WSS") +
      scale_x_continuous(breaks=seq(1:nc), minor_breaks = NULL)
}