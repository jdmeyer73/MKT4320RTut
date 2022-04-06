tsplot <- function(data, tvar, obs, datetype=c("ym", "yq", "yw"), h) {
   require(fpp3)
   fc <- h-1
   if (datetype=="ym") {
      tsdata <- data %>% 
         mutate(Month=yearmonth(.data[[tvar]]),
                Measure=.data[[obs]]) %>% 
         as_tsibble(index=Month)
   } else if (datetype=="yq") {
      tsdata <- data %>%
         mutate(Quarter=yearquarter(.data[[tvar]])) %>%
         as_tsibble(index=Quarter)
   } else if (datetype=="yw") {
      tsdata <- data %>%
         mutate(Week=yearweek(.data[[tvar]])) %>%
         as_tsibble(index=Week)
   } else {
      stop("Time variable not in correct format")
   }
   if (h>0) {
      train <- tsdata[1:(nrow(tsdata)-h), ]
      forecast <- tsdata[(nrow(tsdata)-fc):nrow(tsdata), ]
      autoplot(train, .data[[obs]], size=1) +
         autolayer(forecast, .data[[obs]], color="red", size=1) +
         labs(y=obs) +
         theme_bw() +
         theme(panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank())
   } else {
      autoplot(tsdata, .data[[obs]], size=1) +
         labs(y=obs) +
         theme_bw() +
         theme(panel.grid.major.x = element_blank(),
               panel.grid.minor.x = element_blank())
   }
}