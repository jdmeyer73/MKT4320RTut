fccompare <- function(results, models) {
   require(dplyr)
   require(purrr)
   resids <- list()
   acc <- list()
   for (i in 1:length(results)) {
      resids[[i]] <- results[[i]][3]
      acc[[i]] <- results[[i]][2]
   }
   acc <- map(acc, ~data.frame(.)) %>% bind_rows()
   colnames(acc) <- c("Model", "RMSE", "MAE", "MAPE")
   resids <- map(resids, ~data.frame(.)) %>% bind_rows()
   colnames(resids) <- c("Model", "Date", "Measure", "Forecast", "Actual", "Resid")
   resids <- subset(resids, Model %in% models) %>% as_tibble()
   acc <- subset(acc, Model %in% models)
   fcresplot <- resids %>% ggplot(aes(x=Date, y=Resid, color=Model)) +
      geom_line(size=1) +
      geom_hline(yintercept = 0, color="black", linetype="dashed", size=2) +
      guides(color=guide_legend(title="Model")) +
      theme(legend.position = "bottom")
   results <- list("acc"=acc, "fcresplot"=fcresplot)
   return(results)
}