## bootstrapping fixed-effect models
library(boot)
## base fe regression:
modelFE1 <- clm(formula = mod7,
                data = df3, 
                Hess = TRUE)
##
df3 <- df1 %>% mutate(state.factor = as.factor(statenumber))
mod7 <- as.factor(index) ~ lninc + lnincfed + timetrend + state.factor
bootstrap_clm <- function(data, indices) {
  data_resampled <- data[indices, ]
  model <- clm(formula = mod7, data = data_resampled)
  return(coef(model))
}

set.seed(4567)
bootstrap_results <- boot(data = df3, 
                          statistic = bootstrap_clm, 
                          R = 1000,
                          parallel = "multicore",  # !!WARNING!! USE "snow" instead of "multicore" on Windows (multicore for Unix/Linux)
                          ncpus = 6)

boot_ci <- lapply(1:length(coef(modelFE1)), function(i) {
  boot.ci(bootstrap_results, index = i, type = c("basic", "perc", "bca"), conf = 0.95)
})

# Print the bootstrap confidence intervals
boot_ci[[4]]  # log-CI for lninc.
boot_ci[[4]]$percent[4:5]
exp(boot_ci[[4]]$percent)[4:5]

boot_ci[[4]]$bca[4:5]
exp(boot_ci[[4]]$bca)[4:5]

exp(boot_ci[[4]]$basic)[4:5]
