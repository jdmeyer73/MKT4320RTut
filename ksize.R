ksize <- function(data, centers, nstart=25, seed=4320) {
   sortfreq <- function(t) {
      for (tnum in 1:length(t)) {
         t[[tnum]]$Freq <- sort(t[[tnum]]$Freq, decreasing=TRUE)
      }
      return(t)
   }
   if (missing(centers)) {
      error <- "No centers selected"
      return(error)
   } else {
      kc_table <- lapply(centers, 
                         function(i) {
                            set.seed(seed)
                            data.frame(table(kmeans(data,
                                                    centers=i, 
                                                    nstart=25)$cluster))})
      kc_table <- sortfreq(kc_table)
      k_count <- suppressWarnings(Reduce(function(d1, d2) merge(d1, d2, 
                                               by="Var1", all=TRUE), 
                        kc_table))
      if (length(centers)>1) {
         cnc <- sapply(centers, function(i) paste0("k_", centers, "_Count"))[,1]
      } else {
         cnc <- paste0("k_", centers, "_Count")
      }
      colnames(k_count) <- c("Num_Clusters", cnc)
      kp_table <- lapply(centers, 
                         function(i) {
                            set.seed(seed)
                            data.frame(round(100*prop.table(table(kmeans(data, 
                                                                         centers=i, 
                                                                         nstart=25)$cluster)),2))})
      kp_table <- sortfreq(kp_table)
      k_perc <- suppressWarnings(Reduce(function(d1, d2) merge(d1, d2, 
                                              by="Var1", all=TRUE), 
                       kp_table))
      if (length(centers)>1) {
         cnp <- sapply(centers, function(i) paste0("k_", centers, "_Percent"))[,1]
      } else {
         cnp <- paste0("k_", centers, "_Percent")
      }
      colnames(k_perc) <- c("Num_Clusters", cnp)
      return(list("kcount"=k_count, "kperc"=k_perc))
   }
}