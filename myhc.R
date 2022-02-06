myhc <- function(data, 
                   dist=c("euc", "euc2", "max", "abs", "bin"), 
                   method=c("ward", "single", "complete", "average"),
                   cuts,
                   clustop=c("N","Y")) {
   require(dendextend)
   require(dplyr)
   sortfreq <- function(t) {
      for (tnum in 1:length(t)) {
         t[[tnum]]$Freq <- sort(t[[tnum]]$Freq, decreasing=TRUE)
      }
      return(t)
   }
   #
   # Create lookup for distance method and distance title
   #
   distdf <- data.frame(inp=c("euc", "euc2", "max", "abs", "bin"), 
                        outp=c("euclidean", "euclidean", "maximum", 
                               "manhattan", "binary"), 
                        dtitle=c("Euclidean", "Euclidean^2", "Maximum", 
                                 "Absolute", "Binary"))
   outdist <- distdf[,2:3]
   rownames(outdist) <- distdf[,1]
   dtype <- outdist[dist,1]
   dtitle <- outdist[dist,2]
   #
   # Create lookup for linkage method and linkage title
   #
   linkdf <- data.frame(inp=c("ward", "single", "complete", "average"), 
                        outp=c("ward.D", "single", "complete", "average"), 
                        ltitle=c("Ward's D", "Single", "Complete", "Average"))
   outlink <- linkdf[,2:3]
   rownames(outlink) <- linkdf[,1]
   ltype <- outlink[method,1]
   ltitle <- outlink[method,2]
   #
   # Create power for distance (needed for euc2)
   #
   if(dist=="euc2") {
      pw <- 2
   } else {
      pw <- 1
   }
   #
   # Create dissimilarity matrix
   #
   diss <- dist(data, method=dtype)^pw
   #
   # Create base hclust and dendrogram object
   #
   hc <- hclust(diss, method=ltype)
   hcd <- as.dendrogram(hc)
   #
   # IF CUT IS MISSING, CREATE ONLY DENDROGRAM (AND MAYBE clustop INDICES)
   #
   if(missing(cuts)) {
      dend <- as.dendrogram(hc)
      sub <- "All Branches"
      plot(dend, ylab="Similarity Measure",
           main=paste(dtitle,"Distance /",ltitle,"Linkage"),
           sub=sub)
      if (clustop=="N") {
         return("No Cuts")
      } else {
         require(NbClust)
         duda <- NbClust(data, diss=diss, distance=NULL, min.nc=1, max.nc=10, method=ltype, index="duda")$All.index
         pseudot2 <- NbClust(data, diss=diss, distance=NULL, min.nc=1, max.nc=10, method=ltype, index="pseudot2")$All.index
         out <- data.frame("Num.Clusters"=1:10,"Duda/Hart"=duda, "pseudo-t^2"=pseudot2)
         return(list("stop"=out))
      }   
   } else {
      hcd_h <- heights_per_k.dendrogram(hcd)
   #
   # IF CUT IS NOT MISSING, CREATE TABLES AND DENDROGRAM
   #
      #
      # Create tables
      #
      kc_table <- lapply(cuts, 
                         function(i) data.frame(table(cutree(hcd, k=i))))
      kc_table <- sortfreq(kc_table)
      k_count <- suppressWarnings(Reduce(function(d1, d2) merge(d1, d2, 
                                               by="Var1", all=TRUE), 
                        kc_table))
      if (length(cuts)>1) {
         cnc <- sapply(cuts, function(i) paste0("k_", cuts, 
                                                "_Count"))[,1]
      } else {
         cnc <- paste0("k_", cuts, "_Count")
      }
      colnames(k_count) <- c("Cluster", cnc)
      k_count
      
      kp_table <- lapply(cuts, 
                         function(i) data.frame(round(100*prop.table(table(cutree(hcd, k=i))),2)))
      kp_table <- sortfreq(kp_table)
      k_perc <- suppressWarnings(Reduce(function(d1, d2) merge(d1, d2, 
                                              by="Var1", all=TRUE), 
                       kp_table))
      if (length(cuts)>1) {
         cnp <- sapply(cuts, function(i) paste0("k_", cuts, "_Percent"))[,1]
      } else {
         cnp <- paste0("k_", cuts, "_Percent")
      }
      colnames(k_perc) <- c("Cluster", cnp)
      k_perc
      #
      # Create dendrogram with bars
      #
      cuts_m <- max(cuts)
      the_bars <- sapply(cuts, 
                         function(i) cutree(hcd, k=i, 
                                            order_clusters_as_data = FALSE))
      if (length(cuts)>1) {
         cn <- sapply(cuts, function(i) paste0("k_",cuts))[,1]
      } else {
         cn <- paste0("k_", cuts)
      }
      colnames(the_bars) <- cn
      hcd %>% 
         set("branches_k_color", k=cuts_m) %>%
         set("branches_lwd", 4) %>%
         set("labels_colors","white") %>%
         plot(ylim=c(hcd_h[cuts_m], hcd_h[1]), 
              ylab="Similarity Measure",
              main=paste(dtitle,"Distance /",ltitle,"Linkage"))
      colored_bars(colors=the_bars, dend=hcd, sort_by_labels_order = FALSE)
      if (clustop=="N") {
         results <- list("kcount"=k_count, "kperc"=k_perc, "hc"=hc)
         return(results)
      } else {
         require(NbClust)
         duda <- NbClust(data, diss=diss, distance=NULL, min.nc=1, max.nc=10, method=ltype, index="duda")$All.index
         pseudot2 <- NbClust(data, diss=diss, distance=NULL, min.nc=1, max.nc=10, method=ltype, index="pseudot2")$All.index
         out <- data.frame("Num.Clusters"=1:10,"Duda/Hart"=duda, "pseudo-t^2"=pseudot2)
         results <- list("kcount"=k_count, "kperc"=k_perc, "hc"=hc, "stop"=out)
         return(results)
      }   
      
   }
 }