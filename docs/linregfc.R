linregfc <- function(data, tvar, obs, datetype=c("ym", "yq", "yw"), h) {
   require(fpp3)
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
   train_fit <- train %>% 
      fabletools::model('Lin.Reg.Trend'=TSLM(Measure ~ trend()),
            'Lin.Reg.Seas.Trend'=TSLM(Measure ~ trend()+season()))
   train_fc <- train_fit %>% forecast(h=h)
   plot <- train_fc %>% autoplot(train, level=NULL, size=1) + 
      autolayer(forecast, .data[[obs]]) +
      labs(y=obs) +
      guides(color=guide_legend(title="Model")) +
      theme(legend.position = "bottom")
   acc <- accuracy(train_fc, forecast) %>% 
      select(.model, RMSE, MAE, MAPE) %>%
      rename(Model=1) %>% 
      mutate_at(2:4, round, 3) %>% 
      data.frame()
   fcresid <- train_fc %>% 
      mutate(actual=rep(forecast$Measure,2),
             resid=actual-.mean)
   fcresplot <- fcresid %>% ggplot(aes(x=Date, y=resid, color=.model)) +
      geom_line(size=1) +
      geom_hline(yintercept = 0, color="black", linetype="dashed", size=2) +
      guides(color=guide_legend(title="Model")) +
      theme(legend.position = "bottom")
   
   results <- list("plot"=plot, "acc"=acc, "fcresid"=fcresid, "fcresplot"=fcresplot)
   return(results)
}
