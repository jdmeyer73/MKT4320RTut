kcenters <- function(kobject) {
   require(ggplot2)
   centers <- data.frame(kobject$centers)
   len <- length(centers)
   centers$Cluster <- row.names(centers)
   cenlong <- reshape(centers, direction="long",
                      v.names="value",
                      varying=1:len,
                      times=names(centers)[1:len],
                      timevar="Variable")
   plot <- ggplot(aes(x=Variable, y=value, fill=Cluster), data=cenlong) +
      geom_col(position="dodge") +
      theme(legend.position="bottom") +
      labs(x="Segmentation Variable", y="Cluster Center", fill="Cluster")
   return=list("table"=centers, "plot"=plot)
}