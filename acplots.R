acplots <- function(data, tvar, obs, datetype=c("ym", "yq", "yw"), h, lags=25, d=0, D=c(0,1)) {
   require(fpp3)
   if (datetype=="ym") {
      slag <- 12
      tsdata <- data %>% 
         mutate(Date=yearmonth(.data[[tvar]]),
                Measure=.data[[obs]]) %>% 
         as_tsibble(index=Date)
   } else if (datetype=="yq") {
      slag <- 4
      tsdata <- data %>%
         mutate(Date=yearquarter(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else if (datetype=="yw") {
      slag <- 52
      tsdata <- data %>%
         mutate(Date=yearweek(.data[[tvar]]),
                Measure=.data[[obs]]) %>%
         as_tsibble(index=Date)
   } else {
      stop("Time variable not in correct format")
   }
   pdata <- tsdata[1:(nrow(tsdata)-h), ]
   ci <- 1.96/sqrt(nrow(pdata))
   if (D==0) {
      if (d==0) {
         dlabel <- paste0("")
         acfdata <- pdata %>%
            ACF(Measure, lag_max=lags)
         pacfdata <- pdata %>%
            PACF(Measure, lag_max=lags)
      } else {
         dlabel <- paste0(" (d=",d,")")
         acfdata <- pdata %>%
            ACF(difference(Measure, differences=d), lag_max=lags)
         pacfdata <- pdata %>%
            PACF(difference(Measure, differences=d), lag_max=lags)
      }
   } else if (D==1) {
      if (d==0) {
         dlabel <- paste0(" (D=1)")
         acfdata <- pdata %>%
            ACF(difference(Measure, lag=slag), lag_max=lags)
         pacfdata <- pdata %>%
            PACF(difference(Measure, lag=slag), lag_max=lags)
      } else {
         dlabel <- paste0(" (d=",d,", D=1)")
         acfdata <- pdata %>%
            ACF(difference(difference(Measure, lag=slag), differences=d), 
                lag_max=lags)
         pacfdata <- pdata %>%
            PACF(difference(difference(Measure, lag=slag), differences=d), 
                lag_max=lags)
      }
   }
   aylabel <- paste0("Autocorrelations of ", obs, dlabel)
   pylabel <- paste0("Partial Autocorrelations of ", obs, dlabel)
   acf <- acfdata %>%
      ggplot(aes(x=lag, y=acf)) +
      geom_segment(aes(xend=lag, yend=0), size=1, color="blue") +
      geom_ribbon(aes(ymin=-ci, ymax=ci), alpha=.05, fill="red") +
      geom_point(aes(x=lag, y=acf), color="blue", size=2) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      labs(x="Lag", y=aylabel)
   pacf <- pacfdata %>%
      ggplot(aes(x=lag, y=pacf)) +
      geom_segment(aes(xend=lag, yend=0), size=1, color="blue") +
      geom_ribbon(aes(ymin=-ci, ymax=ci), alpha=.05, fill="red") +
      geom_point(aes(x=lag, y=pacf), color="blue", size=2) +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      labs(x="Lag", y=pylabel)
   results <- list("acf"=acf, "pacf"=pacf)
   return(results)
}