pcaex <- function(data, group, pref, comp=NULL) {
   require(dplyr)
   if(missing(group)) {
      if(missing(pref)) {
         pcadata <- data
      } else {
         prefcnum <- grep(pref,colnames(data))
         pcadata <- data[-prefcnum]
      }
   } else {
      grpcnum <- grep(group, colnames(data))
      if(missing(pref)) {
         pcadata <- data %>% 
            group_by(data[grpcnum]) %>% 
            summarise_all(mean) %>%
            select(-1)
      } else {
         prefcnum <- grep(pref,colnames(data))
         pcadata <- data[-prefcnum] %>% 
            group_by(data[grpcnum]) %>% 
            summarise_all(mean) %>%
            select(-1)
      }
   }
   if(missing(comp)) {
      require(ggplot2)
      pcaobj <- prcomp(pcadata,scale=TRUE)
      eigtable <- data.frame(Component=integer(),
                             Eigenvalue=double(),
                             Difference=double(),
                             Proporation=double(),
                             Cumulative=double())
      len <- length(pcaobj$sdev)
      for (i in 1:len) {
         eigtable[i,1] <- i
         eigtable[i,2] <- pcaobj$sdev[i]^2
         if(i!=len) {
            eigtable[i,3] <-  pcaobj$sdev[i]^2 - pcaobj$sdev[i+1]^2
         } else {
            eigtable[i,3] <- NA
         }
         eigtable[i,4] <- (pcaobj$sdev[i]^2)/len
         if(i==1) {
            eigtable[i,5] <- (pcaobj$sdev[i]^2)/len
         } else {
            eigtable[i,5] <- (pcaobj$sdev[i]^2)/len + eigtable[i-1,5]
         }
      }
      eigtable <- cbind("Component"=eigtable[,1], round(eigtable[,2:5],4))
      scree <- ggplot(aes(x=Component, y=Eigenvalue), data=eigtable) +
         geom_point() +
         geom_line() +
         scale_x_continuous(breaks=1:len, minor_breaks = NULL)
      return(list("table"=eigtable, "plot"=scree))
   } else {
      pcaobj <- prcomp(pcadata,scale=TRUE, rank=comp)
      eigtable <- data.frame(Component=integer(),
                             Eigenvalue=double(),
                             Difference=double(),
                             Proporation=double(),
                             Cumulative=double())
      len <- length(pcaobj$sdev)
      for (i in 1:len) {
         eigtable[i,1] <- i
         eigtable[i,2] <- pcaobj$sdev[i]^2
         if(i!=len) {
            eigtable[i,3] <-  pcaobj$sdev[i]^2 - pcaobj$sdev[i+1]^2
         } else {
            eigtable[i,3] <- NA
         }
         eigtable[i,4] <- (pcaobj$sdev[i]^2)/len
         if(i==1) {
            eigtable[i,5] <- (pcaobj$sdev[i]^2)/len
         } else {
            eigtable[i,5] <- (pcaobj$sdev[i]^2)/len + eigtable[i-1,5]
         }
      }
      unex <- 1-(pcaobj$rotation^2)%*%(matrix(pcaobj$sdev[1:comp]^2))
      unrotated <- data.frame(round(pcaobj$rotation,4),"Unexplained"=round(unex,4))
      cnames <- colnames(unrotated)
      rvarm <- varimax(pcaobj$rotation, normalize=FALSE)
      rot <- pcaobj$rotation%*%rvarm$rotmat
      rotated <- data.frame(round(rot,4), round(unex,4))
      names(rotated) <- cnames
      eigtable <- cbind("Component"=eigtable[,1], round(eigtable[,2:5],4))
      return(list("table"=eigtable, "unrotated"=unrotated, 
                  "rotated"=rotated, "pcaobj"=pcaobj))   
   }
}

