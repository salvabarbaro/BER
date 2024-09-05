### The same as MainAnalyses, but instead of running the ordinal regression with
# # MASS::polr, we exclusively use ordinal::clm.
setwd("~/Documents/Research/Yardstick/BER/ReplicationFiles/")
library(dplyr)
library(lmtest)
library(sandwich)
library(broom)
library(ordinal)  # For fixed effects ordered logistic regression
library(haven)
#library(texreg)
#library(ggplot2)
##########################################################################
# Load data
data <- read_dta("Main_data_set_replication.dta")
# Main Analysis
# Model Specification
mod1 <- as.factor(index) ~ lninc + lnincfed
mod2 <- as.factor(index) ~ lninc + lnincfed + timetrend
mod3 <- as.factor(index) ~ lninc + lnincfed + east + berlin + timetrend
mod4 <- as.factor(index) ~ lninc + lnincfed + att_t_fed + 
  FKM21 + econ_strength + east + 
  berlin + timetrend
mod5 <- as.factor(index) ~ lninc + lnincfed + lnvac + lnvacfed + prevac + 
  att_t_fed + FKM21 + econ_strength + east + 
  berlin + timetrend

## Data set for the main analyses (we consider waves 2 and 3)
df1 <-   data %>% filter(., wave %in% 2:3) %>%
  mutate(timetrend = as.numeric(date - min(date)))
#
clm.fun <- function(mod, data){
  model <- clm(formula = mod, data = data, link = 'logit')
  return(model)
}
###########################################################
## Main Analysis (Table 1)
mainanalysis.list <- as.list(c(mod1, mod2, mod3, mod4, mod5))
#res.main <- lapply(X= mainanalysis.list, FUN = clm.fun) 
res.main <- lapply(X= mainanalysis.list, FUN = clm.fun, data = df1) 
## Robustness Checks (R*)
# R1:  Model excluding NRW & Bavaria [Table 2, (1)]
dfRC1 <- df1 %>% 
  filter(., !statenumber %in% c(3, 11))
NONWBY <- lapply(X= mainanalysis.list, FUN = clm.fun, data = dfRC1) 
# R2: Percent difference: state- vs federal tier incidence / vaccination rate
df2 <- data %>%  filter(., wave %in% 2:3) %>%
  mutate(timetrend = as.numeric(date - min(date))) %>%
  mutate(inc.dev = (exp(lninc)- exp(lnincfed))/ exp(lnincfed),
         vac.dev = (exp(lnvac)- exp(lnvacfed))/ exp(lnvacfed))

mod6 <- as.factor(index) ~ inc.dev +vac.dev +  att_t_fed  +
  east + berlin + timetrend
modelR2 <- df2 %>% clm(formula = mod6, data = .)
summary(modelR2)
broom::tidy(modelR2, exponentiate = TRUE, conf.int = TRUE)
# Robustness check (R3) [Table 2, (3) and (4)]: Fixed-effects ordered logistic regression
df3 <- df1 %>% mutate(state.factor = as.factor(statenumber))
## mod2 + FE
mod7 <- as.factor(index) ~ lninc + lnincfed + timetrend + (1 | state.factor)
modelFE1 <- clmm(formula = mod7,
                 data = df3, threshold = 'equidistant', Hess = TRUE)
broom::tidy(modelFE1, exponentiate = T, conf.int = T)
#
mod8 <- as.factor(index) ~ lninc + lnincfed + lnvac + 
  lnvacfed + timetrend + (1 | state.factor)
modelFE2 <- clmm(formula = mod8,
                 data = df3, threshold = 'equidistant', Hess = TRUE)
broom::tidy(modelFE2, exponentiate = T, conf.int = T)

### Calculate clustered SE
clustered_se <- vcovCL(res.main[[1]], cluster = df1$timetrend)
summary(res.main[[1]], vcov = clustered_se)



