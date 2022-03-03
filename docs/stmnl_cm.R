stmnl_cm <- function(model, data) {
   pred <- predict(model, data, type="class")
   true <- with(data, eval(model$call$formula[[2]]))
   cm <- data.frame(unclass(addmargins(table(pred, true))))
   len <- dim(cm)[2]
   pcc <- 0
   acc <- 0
   cnames <- colnames(cm)
   rnames <- colnames(cm)
   for (i in 1:(len-1)) {
      cnames[i] <- paste0("T.",cnames[i])
      rnames[i] <- paste0("P.",rnames[i])
      acc <- acc + (cm[i,i]/cm[len,len])
      pcc <- pcc + (cm[len,i]/cm[len,len])^2
   }
   cnames[len] <- "Total"
   rnames[len] <- "Total"
   colnames(cm) <- cnames
   #rownames(cm) <- rnames
   rownames(cm) <- 1:len
   cm <- cbind("Level"=rnames,cm)
   out <- paste(paste0(format(round(acc,4), nsmall=4)," = Hit Ratio"),
                paste0(format(round(pcc,4), nsmall=4)," = PCC"), 
                "\n",sep="\n")
   cat(out)
   cm
}

