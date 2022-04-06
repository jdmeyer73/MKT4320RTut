tswh.noise <- function(data, tvar, obs, datetype=c("ym", "yq", "yw"), h=0, arima, sarima) {
   require(fpp3)
   require(ggfortify)
   p <- arima[1]
   d <- arima[2]
   q <- arima[3]
   P <- sarima[1]
   D <- sarima[2]
   Q <- sarima[3]
   
   if (datetype=="ym") {
      slag <- 24
      tsdata <- data %>% 
         mutate(Date=yearmonth(.data[[tvar]]),
                Measure=.data[[obs]]) %>% 
         as_tsibble(index=Date)
   } else if (datetype=="yq") {
      slag <- 12
      tsdata <- data %>%
         mutate(Date=yearquarter(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else if (datetype=="yw") {
      slag <- 26
      tsdata <- data %>%
         mutate(Date=yearweek(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else {
      stop("Time variable not in correct format")
   }
   tsdata <- tsdata[1:(nrow(tsdata)-h), ]
   ts <- tsdata %>%
      model(model=ARIMA(Measure~pdq(p,d,q)+PDQ(P,D,Q))) %>% residuals()
   bt <- Box.test(ts$.resid, lag=slag, type="Ljung-Box", fitdf=(p+q+P+Q))
   ggcpgram(ts$.resid)+
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            plot.caption=element_text(hjust=0, size=14)) +
      labs(caption=paste0("Ljung-Box test: p=",round(bt$p.value,4)))
   
}