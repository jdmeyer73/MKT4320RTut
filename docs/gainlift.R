gainlift <- function(MOD,TRAIN,TEST,POSITIVE) {
   require(ggplot2)
   require(dplyr)
   require(tidyr)
   
   pr.t <- cbind(TRAIN, 
                 pred.t=predict(MOD, 
                                TRAIN, 
                                type="response")) %>%
      mutate(bg=(21-ntile(pred.t,20))/20,
             pos=ifelse(TRAIN[[toString(formula(MOD)[[2]])]]==POSITIVE,1,0)) %>%
      group_by(bg) %>%
      summarise(pos=sum(pos),
                obs=n()) %>%
      mutate(percpos=cumsum(pos)/max(cumsum(pos)),
             perobs=cumsum(obs)/max(cumsum(obs)), 
             lift=cumsum(pos)/(cumsum(obs)*(max(cumsum(pos))/max(cumsum(obs)))),
             sample="Training") %>%
      select(bg, percpos, lift, sample)
   
   
   pr.h <- cbind(TEST, 
                 pred.h=predict(MOD, 
                                TEST, 
                                type="response")) %>%
      mutate(bg=(21-ntile(pred.h,20))/20,
             pos=ifelse(TEST[[toString(formula(MOD)[[2]])]]==POSITIVE,1,0)) %>%
      group_by(bg) %>%
      summarise(pos=sum(pos),
                obs=n()) %>%
      mutate(percpos=cumsum(pos)/max(cumsum(pos)),
             perobs=cumsum(obs)/max(cumsum(obs)), 
             lift=cumsum(pos)/(cumsum(obs)*(max(cumsum(pos))/max(cumsum(obs)))),
             sample="Holdout") %>%
      select(bg, percpos, lift, sample)
   
   base <- cbind.data.frame(bg=seq(0.05,1,.05), percpos=seq(0.05,1,.05), lift=1, sample="Baseline")
   
   pr <- rbind(pr.t,pr.h, base)
   
   gainplot <- ggplot(aes(x=bg, y=percpos, color=sample), data=pr) +
      geom_point(size=1.5) + 
      geom_line(size=1) +
      scale_x_continuous("Proportion Customers Contacted",
                         limits=c(.05,1), breaks=seq(.1,1,.1)) +
      scale_y_continuous("Proportion Customers Positive",
                         limits=c(.05,1), breaks=seq(.1,1,.1)) +
      scale_color_manual("",
                         breaks=c("Training", "Holdout", "Baseline"),
                         values=c("forestgreen", "navy", "red4")) +
      theme(legend.position="bottom")
   
   liftplot <- ggplot(aes(x=bg, y=lift, color=sample), data=pr) +
      geom_point(size=1.5) + 
      geom_line(size=1) +
      scale_x_continuous("Proportion Customers Contacted",
                         limits=c(.05,1), breaks=seq(.1,1,.1)) +
      scale_y_continuous("Lift") +
      scale_color_manual("",
                         breaks=c("Training", "Holdout", "Baseline"),
                         values=c("forestgreen", "navy", "red4")) +
      theme(legend.position="bottom")
   
   lifttable <- pr %>% 
      select(-percpos) %>% 
      filter(sample!="Baseline") %>% 
      spread(sample, lift) %>%
      rename("% Sample"=bg)
   
   gaintable <- pr %>%
      select(-lift) %>%
      filter(sample!="Baseline") %>%
      spread(sample, percpos) %>%
      rename("% Sample"=bg)
   
   results <- list("gainplot"=gainplot, "liftplot"=liftplot,
                   "gaintable"=gaintable, "lifttable"=lifttable)
   return(results)
}