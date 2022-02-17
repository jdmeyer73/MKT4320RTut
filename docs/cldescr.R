cldescr <- function(data, var, vtype=c("F", "C"), cvar) {
   if(vtype=="F") {
      var <- grep(var, colnames(data))
      cvar <- grep(cvar, colnames(data))
      fdata <- data.frame(model.matrix(~data[,var]-1, data=data))
      len <- length(fdata)
      names(fdata) <- substring(names(fdata), 12)
      cnames <- colnames(fdata)
      fdata <- cbind(fdata,Cluster=data[,cvar])
      if(len==2) {
         means <- aggregate(.~Cluster, fdata, 
                            FUN=function(x) round(mean(x), digits=4))[,1:2]
         fdata <- fdata[,-2]
         aov <- aov(fdata[[1]]~Cluster, data=fdata)
         p <- summary(aov)[[1]][["Pr(>F)"]][1]
         aovp <- data.frame(Variable=colnames(fdata)[1], p.value=round(p,4))
         if(p<.1) {
            tukey <- round(TukeyHSD(aov)$Cluster[,c(1,4)],4)
         } else {
            tukey <- NULL
         }
      } else {
         aovp <- data.frame(Variable=character(), p.value=numeric())
         tukey <- list()
         means <- aggregate(.~Cluster, fdata, 
                            FUN=function(x) round(mean(x), digits=4))
         for (i in 1:len) {
            name <- cnames[i]
            aov <- aov(fdata[[i]]~Cluster, data=fdata)
            p <- summary(aov)[[1]][["Pr(>F)"]][1]
            aovp[i,] <- c(colnames(fdata)[[i]],round(p,4))
            if(p<.1) {
               tukey[[name]] <- round(TukeyHSD(aov)$Cluster[,c(1,4)],4)
            } else {
               tukey[[name]] <- NULL
            }
         }
      }
   } else {
      len <- length(var)
      cnames <- var
      if(len==1) {
         cnamesfull <- c(cnames,"Cluster")
         vari <- which(colnames(data)==var)
         cvar <- grep(cvar, colnames(data))
         fdata <- data.frame(data[,vari],Cluster=data[,cvar])
         names(fdata) <- cnamesfull
         means <- aggregate(.~Cluster, fdata, 
                            FUN=function(x) round(mean(x), digits=4))
         aov <- aov(fdata[[1]]~Cluster, data=fdata)
         p <- summary(aov)[[1]][["Pr(>F)"]][1]
         aovp <- data.frame(Variable=colnames(fdata)[1], p.value=round(p,4))
         if(p<.1) {
            tukey <- round(TukeyHSD(aov)$Cluster[,c(1,4)],4)
         } else {
            tukey <- NULL
         }
      } else {
         aovp <- data.frame(Variable=character(), p.value=numeric())
         cnamesfull <- c(cnames, "Cluster")
         tukey <- list()
         vari <- which(colnames(data)==var[1])
         cvar <- grep(cvar, colnames(data))
         fdata <- data.frame(Cluster=data[,cvar], data[,vari])
         for (i in 2:len) {
            vari <- which(colnames(data)==var[i])
            fdata <- data.frame(fdata, data[, vari])
         }
         lenfdata <- length(fdata)
         fdata <- fdata[,c(2:lenfdata,1)]
         names(fdata) <- cnamesfull
         means <- aggregate(.~Cluster, fdata, 
                            FUN=function(x) round(mean(x), digits=4))
         for (i in 1:len) {
            name <- cnamesfull[i]
            aov <- aov(fdata[[i]]~Cluster, data=fdata)
            p <- summary(aov)[[1]][["Pr(>F)"]][1]
            aovp[i,] <- c(colnames(fdata)[[i]],round(p,4))
            if(p<.1) {
               tukey[[name]] <- round(TukeyHSD(aov)$Cluster[,c(1,4)],4)
            } else {
               tukey[[name]] <- NULL
            }
         }
      }
   }
   return=list("means"=means, "aovp"=aovp, "tukey"=tukey)
}




