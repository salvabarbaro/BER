library(ggplot2)
## Add information on state names
Data_Analysis <- read_dta("~/Documents/Research/Yardstick/DATA/Data_Analysis.dta") %>%
  dplyr::select(., c("administrative_area", "FKM21", "econ_strength"))  # "abbreviation"
df0 <- data %>%
  right_join(Data_Analysis %>% distinct(.), 
             by = c("FKM21", "econ_strength"))  %>%
  select(., c("statenumber", "administrative_area")) %>% distinct()

rm(Data_Analysis)
#statecontr.df <- statecontr01.df %>% left_join(x = ., y = df0, by = "statenumber") %>%
#  mutate(NUTS_ID = c(""))
#rm(df0, statecontr01.df, statecontr)

## Random Effect to assess each state's contribution

model.re <- clmm(formula = as.factor(index) ~ lninc + lnincfed + timetrend + 
                   att_t_fed + + FKM21 + econ_strength + (1 | statenumber),
                 data = df1)
summary(model.re)
broom::tidy(model.re, exponentiate = T, conf.int = T)
ranef(model.re)
confint(model.re)

ranef.states <- ranef(model.re) %>% as.data.frame(.) %>% 
  mutate(statenumber = as.numeric(row.names(.))) %>%
  left_join(x = ., y = df0, by = "statenumber") %>%
  mutate(abbreviation = c("BW", "BY", "BE", "BR", "HB", "HH", "HE", "MV", 
                          "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH")) %>%
  rename(Intercept = X.Intercept.)

re.pic.base <- ggplot(ranef.states, aes(x = abbreviation, y = Intercept)) +
  geom_point(col = "purple", size = 3, shape = 5) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "forestgreen") +
  labs(title = "Random Effects for Each State",
       x = "State",
       y = "Random Effect (Deviation from Overall Intercept)") +
  theme_gray(base_size = 22)
ggsave("~/Documents/Research/Yardstick/BER/ReplicationFiles/RandomEffectBase.pdf", plot = re.pic.base)





###############################################################################
###############################################################################
### Bootstrap approach to get CIs
library(boot)

bootstrap_random_effects <- function(data, indices) {
  # Resample the data
  data_resampled <- data[indices, ]
  
  # Refit the model on the resampled data
  model_resampled <- clmm(formula = as.factor(index) ~ lninc + lnincfed + timetrend + 
                            att_t_fed + FKM21 + econ_strength + (1 | statenumber),
                          data = data_resampled)
  # Extract random effects for statenumber
  random_effects_resampled <- ranef(model_resampled)$statenumber
  # Convert the random effects to a numeric vector
  random_effects_numeric <- as.numeric(unlist(random_effects_resampled))
  return(random_effects_numeric)
}

# Run the bootstrap with 1000 iterations
set.seed(4567)  # For reproducibility
bootstrap_results <- boot(data = df1, 
                          statistic = bootstrap_random_effects, 
                          R = 1000,
                          parallel = "multicore",  # !!WARNING!! USE "snow" instead of "multicore" on Windows (multicore for Unix/Linux)
                          ncpus = 6 )

boot_cis <- lapply(1:16, function(i) {
  boot.ci(bootstrap_results, index = i, type = "perc", conf = 0.95)
})

# Create a data frame for the random effects and confidence intervals
ranef_df <- as.data.frame(ranef(model.re)$statenumber) %>% 
  mutate(statenumber = as.numeric(row.names(.))) %>%
  left_join(x = ., y = df0, by = "statenumber") %>%
  mutate(abbreviation = c("BW", "BY", "BE", "BR", "HB", "HH", "HE", "MV", 
                          "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH")) %>%
  rename(Intercept = '(Intercept)' ) %>%
  mutate(lower = sapply(boot_cis, function(ci) ci$percent[4]),
         upper = sapply(boot_cis, function(ci) ci$percent[5]))

re.pic.ci <- ggplot(ranef_df, aes(x = abbreviation, y = Intercept)) +
  geom_point(col = "purple", size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, col = "forestgreen") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Bootstrapped Random Effects for Each State",
       x = "State",
       y = "Random Effect (Dev. from overall interc.)") +
  theme_gray(base_size = 22)
ggsave("~/Documents/Research/Yardstick/BER/ReplicationFiles/RandomEffectCI.pdf", plot = re.pic.ci)

#######################################################################################################
#  Is the random-effects model better suited than the fixed-effect model?
#
###
model.fe <- clm(formula = as.factor(index) ~ lninc + lnincfed + timetrend + 
                  att_t_fed +  factor(statenumber),
                data = df1)

AIC(model.fe)
AIC(model.re)
#
BIC(model.fe)
BIC(model.re)
#
# Likelihood ratio test between the random effects and fixed effects model
#anova(model.re, model.fe)
#anova(model.fe, model.re)  ## Models are not nested! LRT not appropriate.
