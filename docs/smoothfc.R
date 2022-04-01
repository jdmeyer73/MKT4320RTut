smoothfc <- function(data, tvar, obs, datetype=c("ym", "yq", "yw"), h) {
   require(fpp3)
   require(slider)
   fc <- h-1
   if (datetype=="ym") {
      tsdata <- data %>% 
         mutate(Date=yearmonth(.data[[tvar]]),
                Measure=.data[[obs]]) %>% 
         as_tsibble(index=Date)
   } else if (datetype=="yq") {
      tsdata <- data %>%
         mutate(Date=yearquarter(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else if (datetype=="yw") {
      tsdata <- data %>%
         mutate(Date=yearweek(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else {
      stop("Time variable not in correct format")
   }
   train <- tsdata[1:(nrow(tsdata)-h), ]
   forecast <- tsdata[(nrow(tsdata)-fc):nrow(tsdata), ]
   # Moving Average
   madata <- tsdata %>% mutate(Measure2=Measure)
   madata$Measure2[(nrow(madata)-fc):nrow(madata)] <- NA
   madata <- madata %>% 
      mutate(MA=slide_dbl(Measure2, .f=~mean(.x, na.rm=T), .before=h, .after=-1, .complete=TRUE))
   madatafc <- madata[(nrow(tsdata)-fc):nrow(tsdata), ] %>% 
      mutate(".model"="Mov.Ave",".mean"=MA, Measure=MA) %>% 
      select(".model", ".mean", Measure, Date)
   maresid <- madata[(nrow(madata)-fc):nrow(tsdata), ] %>%
      mutate(resid=Measure-MA)
   ma.acc <- maresid %>%
      as_tibble() %>% 
      summarise(MAE = sum(abs(resid))/nrow(maresid),
                RMSE = sqrt((sum(resid^2))/nrow(maresid)),
                MAPE = sum(abs(resid/Measure)*100)/nrow(maresid)) %>%
      mutate(Model="Mov.Ave") %>% 
      relocate(Model) %>%
      mutate_at(2:4, round, 3) %>% 
      data.frame()
   train_fit <- train %>% 
      fabletools::model("Exp.Smooth"=ETS(Measure ~ error("A")+trend("N")+season("N")),
            "H-W.Add"=ETS(Measure ~ error("A")+trend("A")+season("A")),
            "H-W.Mult"=ETS(Measure ~ error("M")+trend("A")+season("M")))
   train_fc <- train_fit %>% forecast(h=h)
   acc <- accuracy(train_fc, forecast) %>%
      select(.model, RMSE, MAE, MAPE) %>%
      rename(Model=1) %>% 
      mutate_at(2:4, round, 3) %>% 
      data.frame()
   acc <- rbind(acc,ma.acc)
   train_fc <- suppressWarnings(bind_rows(train_fc,madatafc))
   plot <- train_fc %>% autoplot(train, level=NULL, size=1) +
      autolayer(forecast, .data[[obs]]) +
      labs(y=obs) +
      guides(color=guide_legend(title="Model")) +
      theme(legend.position = "bottom")
   fcresid <- train_fc %>% 
      mutate(actual=rep(forecast$Measure,4),
             resid=actual-.mean)
   fcresplot <- fcresid %>% ggplot(aes(x=Date, y=resid, color=.model)) +
      geom_line(size=1) +
      geom_hline(yintercept = 0, color="black", linetype="dashed", size=2) +
      guides(color=guide_legend(title="Model")) +
      theme(legend.position = "bottom")
   
   results <- list("plot"=plot, "acc"=acc, "fcresid"=fcresid, "fcresplot"=fcresplot)
   return(results)
}