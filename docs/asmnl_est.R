asmnl_est <- function(formula, data, id, alt, choice, testdata) {
   require(mlogit)
   require(broom)
   mdata <- dfidx(data, idx=c(id,alt), choice=choice)
   mod <- mlogit(formula=formula, data=mdata)
   sum <- summary(mod)
   modout <- as.data.frame(tidy(mod))
   modout$estimate <- exp(modout$estimate)
   modout[,2:5] <- round(modout[,2:5],4)
   ll <- paste0("Log-Likelihood: ", format(round(mod$logLik,4),nsmall=4))
   mcf <- paste0("McFadden R^2: ", format(round(sum$mfR2[1],4),nsmall=4))
   lrt <- paste0("Likelihood ratio test: chisq = ",
                 format(round(sum$lratio$statistic[1],4),nsmall=4),
                 ifelse(sum$lratio$p.value<0.00005,
                        " (p.value < .0001)",
                        paste0(" (p.value = ", 
                               format(round(sum$lratio$p.value,4),nsmall=4),
                               ")")))
   cat(paste("\n","---------","Model Fit","---------","\n",sep="\n"))
   assessout <- paste(ll,mcf,lrt,"\n",sep="\n")
   cat(assessout)           
   cat(paste("---------------------","OR Estimation Results",
             "---------------------","\n",sep="\n"))
   print(modout, row.names=FALSE)
   ftrain <- data.frame(mod$probabilities)
   cnames <- colnames(ftrain)
   len <- dim(mod$probabilities)[2]
   chidlen <- nrow(testdata)/len
   ftrain$choice <- colnames(ftrain)[max.col(ftrain, ties.method="first")]
   acttrain <-  mod$model$idx[,2][mod$model[,1]==TRUE]
   cmtrain <- data.frame(unclass(addmargins(table(ftrain$choice, acttrain))))
   lencmt <- dim(cmtrain)[2]
   pcc <- 0
   acc <- 0
   c.names <- colnames(cmtrain)
   r.names <- colnames(cmtrain)
   for (i in 1:(lencmt-1)) {
      c.names[i] <- paste0("T.",c.names[i])
      r.names[i] <- paste0("P.",r.names[i])
      acc <- acc + (cmtrain[i,i]/cmtrain[lencmt,lencmt])
      pcc <- pcc + (cmtrain[lencmt,i]/cmtrain[lencmt,lencmt])^2
   }
   c.names[lencmt] <- "Total"
   r.names[lencmt] <- "Total"
   colnames(cmtrain) <- c.names
   rownames(cmtrain) <- r.names
   outcmtrain <- paste0("\n",
                        "---------------------------------------","\n",
                        "Classification Matrix for Training Data","\n",
                        "---------------------------------------","\n",
                        paste0(format(round(acc,4), nsmall=4)," = Hit Ratio","\n"),
                        paste0(format(round(pcc,4), nsmall=4)," = PCC","\n"),
                        "\n")
   cat(outcmtrain)
   print(cmtrain)
   
   tdata <- dfidx(testdata, idx=c(id,alt), choice=choice)
   tmod <- mlogit(formula=formula, data=tdata)
   X <- model.matrix(tmod)
   chid <- idx(tmod,1)
   for (i in 1:chidlen) {
      b <- i*len-(len-1)
      e <- i*len
      chid[b:e] <- i
   }
   eXb <- as.numeric(exp(X %*% coef(mod)))
   SeXb <- tapply(eXb, chid, sum)
   P <- eXb/SeXb[chid]
   ftest <- data.frame(matrix(P, ncol=len, byrow=TRUE))
   colnames(ftest) <- cnames
   ftest$choice <- colnames(ftest)[max.col(ftest, ties.method="first")]
   acttest <-  tmod$model$idx[,2][tmod$model[,1]==TRUE]
   cmtest <- data.frame(unclass(addmargins(table(ftest$choice, acttest))))
   pcc <- 0
   acc <- 0
   for (i in 1:(lencmt-1)) {
      acc <- acc + (cmtest[i,i]/cmtest[lencmt,lencmt])
      pcc <- pcc + (cmtest[lencmt,i]/cmtest[lencmt,lencmt])^2
   }
   colnames(cmtest) <- c.names
   rownames(cmtest) <- r.names
   outcmtest <- paste0("\n",
                       "--------------------------------------","\n",
                       "Classification Matrix for Holdout Data","\n",
                       "--------------------------------------","\n",
                       paste0(format(round(acc,4), nsmall=4)," = Hit Ratio","\n"),
                       paste0(format(round(pcc,4), nsmall=4)," = PCC","\n"),
                       "\n")
   cat(outcmtest)
   print(cmtest)
   return(mod)
}