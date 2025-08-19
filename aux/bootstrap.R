library(parallel)
library(boot)
#################################################################################################
# Warning: be aware of the hardware requirements for parallel computing. 
# If you are unsure or not familar with parallel computing, comment out parallel and ncpus. 
## bootstrap ci with boot.ci
bootstrap.analysis.fun <- function(mod){
  boot_clm.fun <- function(data, indices) {
    d <- data[indices, ]  # Resample based on row indices
    model <- clm(mod, data = d)
    return(coef(model))  # Return all coefficients
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
  ci <- boot.ci(boot.res, type = c("basic", "perc", "bca"), index = 4)
  return(ci)
  }
#
mainanalysis.list <- as.list(c(mod1, mod2, mod3, mod4, mod5))
boot.main <- lapply(mainanalysis.list, bootstrap.analysis.fun)

###
extract_ci <- function(boot.ci.result, model_name) {
  ci_list <- list(
    model = model_name,
    basic_low = if (!is.null(boot.ci.result$basic)) boot.ci.result$basic[4] else NA,
    basic_high = if (!is.null(boot.ci.result$basic)) boot.ci.result$basic[5] else NA,
    perc_low = if (!is.null(boot.ci.result$perc)) boot.ci.result$perc[4] else NA,
    perc_high = if (!is.null(boot.ci.result$perc)) boot.ci.result$perc[5] else NA,
    bca_low = if (!is.null(boot.ci.result$bca)) boot.ci.result$bca[4] else NA,
    bca_high = if (!is.null(boot.ci.result$bca)) boot.ci.result$bca[5] else NA
  )
  return(as.data.frame(ci_list))
}
model_names <- paste0("Model_", seq_along(boot.main))

ci_df <- do.call(rbind, lapply(seq_along(boot.main), function(i) {
  extract_ci(boot.main[[i]], model_names[i])
})) %>%  mutate(across(-model, exp)) %>%
  mutate(Odds.Ratio = exp(unlist(lapply(1:5, 
                                        FUN = function(i){boot.main[[i]][["t0"]]} ) )),
         .after = "model")
