library(parallel)
library(boot)
#################################################################################################
# Warning: be aware of the hardware requirements for parallel computing. 
# If you are unsure or not familar with parallel computing, comment out parallel and ncpus. 
bootstrap.analysis.fun <- function(mod){
  boot_clm.fun <- function(data, indices) {
    d <- data[data$timetrend %in% indices, ]
    model <- clm(mod, data = d )
    return(coef(model))
  }
#
set.seed(4567)  
boot.res <- boot(
  data = df1,
  statistic = boot_clm.fun, 
  R = 2000,                        
  strata = df1$timetrend, 
  parallel = "multicore",  # !!WARNING!! USE "snow" instead of "multicore" on Windows (multicore for Unix/Linux)
  ncpus = 6                #  Adjust the cores such that it is suitable to your device.
  )
  
bootstrap_coefs <- as.data.frame(boot.res$t)
return(bootstrap_coefs)
}
#
mainanalysis.list <- as.list(c(mod1, mod2, mod3, mod4, mod5))
boot.main <- lapply(mainanalysis.list, bootstrap.analysis.fun)
####
results_df <- lapply(boot.main, function(boot_result) {
  fourth_column <- boot_result[, 4]  # lninc
  mean_value <- mean(fourth_column)
  quantile_5 <- quantile(fourth_column, 0.05)
  quantile_95 <- quantile(fourth_column, 0.95)
  c(mean_value, quantile_5, quantile_95, exp(mean_value), exp(quantile_5), exp(quantile_95))
})

# Convert the list of results into a data frame
results.df <- do.call(rbind, results_df) %>% as.data.frame(.) %>%
  setNames(c("log-Odds", "logCI.low", "logCI.high", "OddsRatio", "CI.low", "CI.high"))

print(results.df)

