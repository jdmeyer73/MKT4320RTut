capred <- function(formula, data, prof1, prof2) {
   if (length(prof1)!=length(prof2)) {
      stop("Profile lengths do not match")
   }
   newout <- data
   len <- length(prof1)
   utp1 <- NULL
   utp2 <- NULL
   prof1name <- NULL
   prof2name <- NULL
   for (i in 1:len) {
      cnum1 <- grep(prof1[i], colnames(data))
      cnum2 <- grep(prof2[i], colnames(data))
      utp1 <- append(utp1,cnum1)
      utp2 <- append(utp2,cnum2)
      prof1name <- append(prof1name, colnames(data[cnum1]))
      prof2name <- append(prof2name, colnames(data[cnum2]))
   }
   newout$prof1 <- rowSums(data[utp1])
   newout$prof2 <- rowSums(data[utp2])
   mu1 <- mean(newout$prof1)
   mu2 <- mean(newout$prof2)
   sd1 <- sd(newout$prof1)
   sd2 <- sd(newout$prof2)
   n <- nrow(newout)
   err1 <- qt(0.975, df=n-1)*sd1/sqrt(n)
   err2 <- qt(0.975, df=n-1)*sd2/sqrt(n)
   ci1 <- paste0("(",round(mu1-err1,3),",",round(mu1+err1,3),")")
   ci2 <- paste0("(",round(mu2-err2,3),",",round(mu2+err2,3),")")
   cat(paste0("\n","Profile 1 = ",paste0(prof1name, collapse="  /  "),"\n"))
   cat(paste0("Mean Utility = ",round(mu1,3),"\n"))
   cat(paste0("95% CI = ", ci1,"\n"))
   cat(paste0("\n","Profile 2 = ",paste0(prof2name, collapse="  /  "),"\n"))
   cat(paste0("Mean Utility = ",round(mu2,3),"\n"))
   cat(paste0("95% CI = ", ci2,"\n"))
}