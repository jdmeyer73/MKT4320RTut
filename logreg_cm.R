logreg_cm <- function(MOD, DATA, POSITIVE, CUTOFF=0.5) {
   require(caret)
   dvprob <- predict(MOD, DATA, type="response")
   dvnum <- ifelse(dvprob<CUTOFF, 1, 2)
   dvfac <- factor(dvnum)
   levels(dvfac) <- levels(MOD$model[,1])
   true <- DATA[[toString(formula(MOD)[[2]])]]
   cm <- confusionMatrix(dvfac, true, POSITIVE)
   print(cm)
   numpos <- table(DATA[[toString(formula(MOD)[[2]])]])[POSITIVE]
   numobs <- nrow(DATA)
   pcc <- paste0("PCC = ", 
                 sprintf("%.2f%%",
                         ((numpos/numobs)^2 + (1-(numpos/numobs))^2)*100))
   cat(pcc)
}