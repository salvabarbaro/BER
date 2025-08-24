# Packages
library(haven)        # for import of *.dta
library(dplyr)
#library(ordinal)
library(MASS)         # polr()
library(brant)        # brant test for proportional odds
library(broom)        # tidy() for coefficients
library(lmtest)       # coeftest()
library(sandwich)     # vcovCL()
library(readr)        # write_csv()
library(modelsummary)
library(tidyverse)

setwd("~/Documents/Research/Yardstick/BER/github/BER/")
# 1) Load data (Stata .dta)
df <- haven::read_dta("Main_data_set_replication.dta") #%>%
#  mutate(data_id = 1:nrow(.))
idtbl <- read.csv("IDtable.csv", header = T) %>%
#  dplyr::select(., c("statenumber", "Land", "stmt.id", "data.id")) %>%
  dplyr::rename(., stmt_id = stmt.id) %>%
  dplyr::rename(., data_id = data.id) %>%
  dplyr::mutate(date = as.Date(date))
df2 <- df %>% left_join(x = ., 
                       y = idtbl,
                       by = c("date", "statenumber", "index", "wave")) %>%
  distinct(.)  # resolves the warnings
df <- df2
rm(df2)

# 2) Restrict to waves 2 or 3 (Stata: if (wave == 2 | wave == 3))
dat <- df %>% filter(wave %in% c(2, 3)) %>%
  mutate(date.fct = as.factor(date),
         week.fct = as.factor(weeknr)) %>%
  mutate(timetrend = as.numeric(date - min(date))) %>%
  mutate(weektrend = as.numeric(weeknr - min(weeknr)))


# Brant test (PO/parallel lines)
m_full <- polr(as.factor(index) ~ lninc + lnincfed + att_t_fed + att_t_fed + FKM21 + econ_strength, 
               method = "logistic", data = dat)
po_test <- brant::brant(m_full)
rm(m_full, po_test)
# The five initial models (main = mod5)
mod1 <- as.factor(index) ~ lninc + lnincfed
mod2 <- as.factor(index) ~ lninc + lnincfed + timetrend      
mod3 <- as.factor(index) ~ lninc + lnincfed + week.fct
mod4 <- as.factor(index) ~ lninc + lnincfed + att_t_fed + 
  FKM21 + econ_strength + week.fct
mod5 <- as.factor(index) ~ lninc + lnincfed + lnvac + 
  lnvacfed  + att_t_fed + FKM21 + 
  econ_strength + week.fct

## all-in-one
polr.fun <- function(mod){polr(formula = mod, data = dat, Hess = T, model= T)}
res.main <- lapply(list(mod1, mod2, mod3, mod4, mod5), FUN = polr.fun)
modelsummary(res.main, exponentiate = T, statistic = "conf.int", coef_omit = "2|3|week", vocov = ~date)
##############################?#

# Alternative to all-in-one: 
# step-by-step
p1 <- polr(formula = mod1, data = dat, Hess = T, model = T)
modelsummary(p1, exponentiate = T, statistic = "conf.int", coef_omit = "2|3", vcov = ~date)
p2 <- polr(formula = mod2, data = dat, Hess = T, model = T)
modelsummary(p2, exponentiate = T, statistic = "conf.int", coef_omit = "2|3", vcov = ~date)
p3 <- polr(formula = mod3, data = dat, Hess = T, model = T)
modelsummary(p3, exponentiate = T, statistic = "conf.int", coef_omit = "2|3", vcov = ~date)
p4 <- polr(formula = mod4, data = dat, Hess = T, model = T)
modelsummary(p4, exponentiate = T, statistic = "conf.int", coef_omit = "2|3", vcov = ~date)
p5 <- polr(formula = mod5, data = dat, Hess = T, model = T)
modelsummary(p5, exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3|week", 
             vcov = ~date)
modelsummary(list(p1, p2, p3, p4, p5), exponentiate = T,
             statistic = "conf.int", coef_omit = "2|3|week")
##############################################################
## Robustness check with AI-generated values
ai.data <- read_dta("ai.dta")
#dat$ai_index <- ai.data$ai_index
dat.ai <- dat %>% 
  left_join(x = ., 
            y = ai.data %>% dplyr::select(., c("data_id", "stmt_id", "ai_index")),
            by = c("stmt_id", "data_id")) %>%
  distinct(.)
##########################################################
mod1.ai <- as.factor(ai_index) ~ lninc + lnincfed
mod2.ai <- as.factor(ai_index) ~ lninc + lnincfed + timetrend      
mod3.ai <- as.factor(ai_index) ~ lninc + lnincfed + week.fct
mod4.ai <- as.factor(ai_index) ~ lninc + lnincfed + att_t_fed + 
  FKM21 + econ_strength + week.fct
mod5.ai <- as.factor(ai_index) ~ lninc + lnincfed + lnvac + 
  lnvacfed  + att_t_fed + FKM21 + 
  econ_strength + week.fct

polr.fun <- function(mod){polr(formula = mod, data = dat.ai, Hess = T, model= T)}
res.ai <- lapply(list(mod1.ai, mod2.ai, mod3.ai, mod4.ai, mod5.ai), FUN = polr.fun)
modelsummary(res.ai, exponentiate = T, statistic = "conf.int", coef_omit = "2|3|week")

p1.ai <- polr(formula = mod1.ai, data = dat.ai, Hess = T, model = T)
modelsummary(p1.ai, exponentiate = T, statistic = "conf.int", coef_omit = "2|3", vcov = ~date)
## significant
p2.ai <- polr(formula = mod2.ai, data = dat.ai, Hess = T, model = T)
modelsummary(p2.ai, exponentiate = T, statistic = "conf.int", coef_omit = "2|3", vcov = ~date)
## mod2: significant
p3.ai <- polr(formula = mod3.ai, data = dat.ai, Hess = T, model = T)
modelsummary(p3.ai, exponentiate = T, statistic = "conf.int", coef_omit = "2|3", vcov = ~date)
## mod3: no longer significant 
p4.ai <- polr(formula = mod4.ai, data = dat.ai, Hess = T, model = T)
modelsummary(p4.ai, exponentiate = T, statistic = "conf.int", coef_omit = "2|3", vcov = ~date)
## still significant
p5.ai <- polr(formula = mod5.ai, data = dat.ai, Hess = T, model = T)
modelsummary(p5.ai, exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3", 
             vcov = ~date)
## still significant
mainmodlist <- list(p1, p2, p3, p4, p5) 
names(mainmodlist) <- c("(1): minimal", "(2): timetrend", "(3): Week-FE", "(4): covariates", "(5): Main model")
modelplot(mainmodlist, 
             exponentiate = T,
             coef_map = c('lninc' = 'ln(state-level inc. rate)'),
          vcov = ~date
             ) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  theme_minimal(base_size = 18) +
  scale_colour_manual(values =c("red", "blue", 
                                "yellow", "brown", 
                                "purple")) +
  xlim(0.5, 3.5) 


### Compare human coded models and AI-generated PPI values
mainai.list <- list(p1, p1.ai, p2, p2.ai, p3, p3.ai, p4, p4.ai, p5, p5.ai)
lt <- setNames(rep(c("solid", "dashed"), length.out = length(mainai.list)),
               names(mainai.list))

modelplot(
  mainai.list,
  exponentiate = TRUE,
  coef_map = c('lninc' = 'ln(state-level inc. rate)'),
  vcov = ~ date
  ) +
  aes(linetype = model) +                                # map linetype to model
  geom_vline(xintercept = 1, linetype = "dotdash", linewidth = 1.5) +
  theme_gray(base_size = 22) +
  scale_colour_manual(
    values = c("red","red","blue","blue","yellow","yellow","brown","brown","purple","purple"),
    breaks = names(mainai.list)
  ) +
  scale_linetype_manual(values = lt, breaks = names(mainai.list)) +
  xlim(0.5, 3.5) + coord_flip() +
  labs(x = "Odds and (exp.) 95% CIs")
##
ggsave("~/Documents/Research/Yardstick/BER/git/6842e459f8d02615df55d365/Presentation/mainAIcomp.pdf",
       width = 16, height = 8)
#######################################################################################
### Robustness Checks
## 1. Remove BY, NW; restrict on mod5
dat.rc1 <- dat %>% filter(., !Land %in% c("BY", "NW"))
rc1 <- polr(formula = mod5, data = dat.rc1, Hess = T, model = T)
modelsummary(rc1, exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3|week", 
             vcov = ~date)
## 2. Consider the week of the MPK's only
dat.rc2 <- dat %>% filter(., weekMPK == 1)
rc2 <- polr(formula = as.factor(index) ~ lninc + lnincfed + timetrend + 
              att_t_fed + FKM21 + econ_strength + lnvac + lnvacfed, 
            data = dat.rc2,
            Hess = T, model = T)
modelsummary(rc2, exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3|week", 
             vcov = ~date)
### up to three day until a MPK
dat.rc3 <- dat %>% filter(., dayssinceMPK < 4)
rc3 <- polr(formula = as.factor(index) ~ lninc + lnincfed + timetrend + 
              att_t_fed + FKM21 + econ_strength + lnvac + lnvacfed, 
            data = dat.rc3,
            Hess = T, model = T)
modelsummary(rc3, exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3|week", 
             vcov = ~date)
#### relative deviations instead of logs
dat.rc4 <- dat %>%
  mutate(inc.dev = (exp(lninc)- exp(lnincfed))/ exp(lnincfed),
         vac.dev = (exp(lnvac)- exp(lnvacfed))/ exp(lnvacfed))
mod.rc4 <- as.factor(index) ~ inc.dev + vac.dev + att_t_fed + 
  FKM21 + econ_strength + week.fct
  # index incdev vacdev prevac att_t_fed FKM21 econ_strength i.weeknr
rc4 <- polr(formula = mod.rc4, data = dat.rc4, Hess = T, model = T)
modelsummary(rc4, exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3|week",
             vcov = ~date
             )
#######################################################################
dat.rc5 <- dat %>% mutate(states.fct = as.factor(statenumber))
rc5A <- polr(formula = as.factor(index) ~ lninc + lnincfed + states.fct,
             data = dat.rc5, Hess = T, model = T)
rc5B <- polr(formula = as.factor(index) ~ lninc + lnincfed + lnvac + lnvacfed + 
               prevac + timetrend + states.fct,
             data = dat.rc5, Hess = T, model = T)
# index lninc lnincfed lnvac lnvacfed prevac date
modelsummary(list(rc5A, rc5B), exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3|week|fct",
             vcov = ~date
)
########################################################################
dat.rc6 <- dat %>% 
  mutate(east.fct = as.factor(east), 
         berlin.fct = as.factor(berlin)) #%>% 
#  filter(., !Land %in% c("BY", "NW"))
rc6 <- polr(formula = as.factor(index) ~ lninc + lnincfed + lnvac + lnvacfed + #prevac + 
              att_t_fed + FKM21 + econ_strength + east.fct + berlin.fct + week.fct ,
            data = dat.rc6, Hess = T, model = T)
modelsummary(rc6, exponentiate = T, coef_omit = "2|3|week", statistic = "conf.int",
             vcov = ~date)
# lninc lnincfed lnvac lnvacfed prevac att_t_fed FKM21 econ_strength east berlin i.weeknr

overview.list <- list(p5, p5.ai,   # main and main.ai 
                      rc1,         # BY & NW
                      rc2,         # Week of PMC
                      rc3,         # PMC + 3days
       #               rc4,         # relative dev.
                      rc5A, rc5B, # FE
                      rc6         # Dummy East+Berlin
                      )
names(overview.list) <- c("Main", "Main.AI", "BY.NW", "PMC.Week", "PMC+3days", "FE1", "FE2", "East.Bln")

modelplot(overview.list, 
          exponentiate = T,
          coef_map = c('lninc' = 'ln(state-level inc. rate)'),
          vcov = ~date
) +
  geom_vline(xintercept = 1, linetype = "dotdash", linewidth = 1.5) +
  theme_gray(base_size = 22) +
  coord_flip()+
  labs(x = "Odds and (exp.) 95% CIs")
#  scale_colour_manual(values =c("red", rep("purple", 7))) # +
#  xlim(0.5, 5)
ggsave("~/Documents/Research/Yardstick/BER/git/6842e459f8d02615df55d365/Presentation/robstnessOverview.pdf", 
       width = 16, height = 8)          
