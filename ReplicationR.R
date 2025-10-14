# Packages
library(haven)        # for import of *.dta
library(dplyr)
#library(ordinal)     # Alternative to MASS 
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
df <- haven::read_dta("Main_data_set_replication.dta") %>%
       mutate(incdev = (exp(lninc)- exp(lnincfed))/ exp(lnincfed),
              vacdev = (exp(lnvac)- exp(lnvacfed))/ exp(lnvacfed))


idtbl <- read.csv("IDtable.csv", header = T) %>%
#  dplyr::select(., c("statenumber", "Land", "stmt.id", "data.id")) %>%
  dplyr::rename(., stmt_id = stmt.id) %>%
  dplyr::rename(., data_id = data.id) %>%
  dplyr::mutate(date = as.Date(date))
df2 <- df %>% left_join(x = ., 
                       y = idtbl,
                       by = c("date", "statenumber", "index", "wave"),
                      relationship = "many-to-many") %>%
  distinct(.)  # resolves the warnings~
df <- df2
rm(df2)

# 2) Restrict to waves 2 or 3 (Stata: if (wave == 2 | wave == 3))
dat <- df %>% filter(wave %in% c(2, 3)) %>%
  mutate(date.fct = as.factor(date),
         week.fct = as.factor(weeknr)) %>%
  mutate(timetrend = as.numeric(date - min(date))) %>%
  mutate(weektrend = as.numeric(weeknr - min(weeknr)))


# Brant test (PO/parallel lines)
## omodel logit index lninc lnincfed lnvac lnvacfed prevac att_t_fed FKM21 econ_strength east berlin date if (wave == 2 | wave == 3)

m_full <- polr(as.factor(index) ~ lninc + lnincfed + 
  lnvac + lnvacfed + prevac + ## new
  att_t_fed + att_t_fed + FKM21 + econ_strength +
  east + berlin, 
  method = "logistic", data = dat)
po_test <- brant::brant(m_full)
## H_0 : Parallel Regression Assumption holds
## We rejet H_0 is a p-value is below 0.05.
## Overall: p-value = 0.15, thus PO-Assumption holds!
rm(m_full, po_test)
#############################################
####  TABLE 2 (Main regression models)
####  (main = mod5)
mod1 <- as.factor(index) ~ lninc + lnincfed
mod2 <- as.factor(index) ~ lninc + lnincfed + timetrend      
mod3 <- as.factor(index) ~ lninc + lnincfed + week.fct
mod4 <- as.factor(index) ~ lninc + lnincfed + att_t_fed + 
  FKM21 + econ_strength + week.fct
mod5 <- as.factor(index) ~ lninc + lnincfed + lnvac + 
  lnvacfed + att_t_fed + FKM21 + 
  econ_strength + week.fct

polr.fun <- function(mod){polr(formula = mod, data = dat, Hess = T, model= T)}
res.main <- lapply(list(mod1, mod2, mod3, mod4, mod5), FUN = polr.fun)
modelsummary(res.main, exponentiate = T, statistic = "conf.int", coef_omit = "2|3|week", vcov = ~date)
##############################?#
## Lines 71 - 89: the same as above, but model by model
## Main model 1
#p1 <- polr(formula = mod1, data = dat, Hess = T, model = T)
#modelsummary(p1, exponentiate = T, statistic = "conf.int", coef_omit = "2|3", vcov = ~date)
## Main model 2
#p2 <- polr(formula = mod2, data = dat, Hess = T, model = T)
#modelsummary(p2, exponentiate = T, statistic = "conf.int", coef_omit = "2|3", vcov = ~date)
## Main model 3
#p3 <- polr(formula = mod3, data = dat, Hess = T, model = T)
#modelsummary(p3, exponentiate = T, statistic = "conf.int", coef_omit = "2|3|week", vcov = ~date)
## Main model 4
#p4 <- polr(formula = mod4, data = dat, Hess = T, model = T)
#modelsummary(p4, exponentiate = T, statistic = "conf.int", coef_omit = "2|3|week", vcov = ~date)
## Main model 5
#p5 <- polr(formula = mod5, data = dat, Hess = T, model = T)
#modelsummary(p5, exponentiate = T, 
#             statistic = "conf.int", 
#             coef_omit = "2|3|week", 
#             vcov = ~date)
### now table 2
#modelsummary(list(p1, p2, p3, p4, p5), exponentiate = T,
#             statistic = "conf.int", coef_omit = "2|3|week")

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
mod4.ai <- as.factor(ai_index) ~ lninc + lnincfed +
  att_t_fed + FKM21 + econ_strength + week.fct
mod5.ai <- as.factor(ai_index) ~ lninc + lnincfed + lnvac + 
  lnvacfed  + att_t_fed + FKM21 +  econ_strength + week.fct

#polr.fun <- function(mod){polr(formula = mod, data = dat.ai, Hess = T, model= T)}
#res.ai <- lapply(list(mod1.ai, mod2.ai, mod3.ai, mod4.ai, mod5.ai), FUN = polr.fun)
#modelsummary(res.ai[[1]], exponentiate = T, statistic = "conf.int", coef_omit = "2|3|week", vcov = ~data)

p1.ai <- polr(formula = mod1.ai, data = dat.ai, Hess = T, model = T)
modelsummary(p1.ai, exponentiate = T, statistic = "conf.int", coef_omit = "2|3", vcov = ~date)
#
p2.ai <- polr(formula = mod2.ai, data = dat.ai, Hess = T, model = T)
modelsummary(p2.ai, exponentiate = T, statistic = "conf.int", coef_omit = "2|3", vcov = ~date)
## mod2: significant
p3.ai <- polr(formula = mod3.ai, data = dat.ai, Hess = T, model = T)
modelsummary(p3.ai, exponentiate = T, statistic = "conf.int", coef_omit = "2|3|week", vcov = ~date)
## mod3: no longer significant 
p4.ai <- polr(formula = mod4.ai, data = dat.ai, Hess = T, model = T)
modelsummary(p4.ai, exponentiate = T, statistic = "conf.int", coef_omit = "2|3|week", vcov = ~date)
## still significant
p5.ai <- polr(formula = mod5.ai, data = dat.ai, Hess = T, model = T)
modelsummary(p5.ai, exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3|week", 
             vcov = ~date)

#mainmodlist <- list(p1, p2, p3, p4, p5) 
#names(mainmodlist) <- c("(1): minimal", "(2): timetrend", "(3): Week-FE", "(4): covariates", "(5): Main model")
#modelplot(mainmodlist, 
#             exponentiate = T,
#             coef_map = c('lninc' = 'ln(state-level inc. rate)'),
#          vcov = ~date
#             ) +
#  geom_vline(xintercept = 1, linetype = "dashed") +
#  theme_minimal(base_size = 18) +
#  scale_colour_manual(values =c("red", "blue", 
#                                "yellow", "brown", 
#                                "purple")) +
#  xlim(0.5, 3.5) 


### Compare human coded models and AI-generated PPI values
#mainai.list <- list(p1, p1.ai, p2, p2.ai, p3, p3.ai, p4, p4.ai, p5, p5.ai)
#lt <- setNames(rep(c("solid", "dashed"), length.out = length(mainai.list)),
 #              names(mainai.list))

#modelplot(
#  mainai.list,
#  exponentiate = TRUE,
#  coef_map = c('lninc' = 'ln(state-level inc. rate)'),
#  vcov = ~ date
#  ) +
#  aes(linetype = model) +                                # map linetype to model
#  geom_vline(xintercept = 1, linetype = "dotdash", linewidth = 1.5) +
#  theme_gray(base_size = 22) +
#  scale_colour_manual(
#    values = c("red","red","blue","blue","yellow","yellow","brown","brown","purple","purple"),
#    breaks = names(mainai.list)
#  ) +
#  scale_linetype_manual(values = lt, breaks = names(mainai.list)) +
#  xlim(0.5, 3.5) + coord_flip() +
#  labs(x = "Odds and (exp.) 95% CIs")
##
#ggsave("~/Documents/Research/Yardstick/BER/git/6842e459f8d02615df55d365/Presentation/mainAIcomp.pdf",
#       width = 16, height = 8)
#######################################################################################
### Robustness Checks
### TABLE 3
#### relative deviations instead of logs (Table 3, Model (1))
mod.rc4 <- as.factor(index) ~ incdev + vacdev + att_t_fed +
  FKM21 + econ_strength + week.fct

rc4 <- polr(formula = mod.rc4, data = dat, Hess = T, model = T)
modelsummary(rc4, exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3|week",
             vcov = ~date
             )

modelsummary(rc4, exponentiate = T, 
             statistic = "p.value", 
             coef_omit = "2|3|week",
             vcov = ~date, stars=T
             )

## RC2:Remove BY, NW; restrict on mod5  (Table 3, Model (2))
dat.rc1 <- dat %>% filter(., !Land %in% c("BY", "NW"))
rc1 <- polr(formula = mod5, data = dat.rc1, Hess = T, model = T)
modelsummary(rc1, exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3|week", 
             vcov = ~date)
#RC3: Dummies for Eastern States and Berlin (Table 3, Model (3)). Note that this dummy-model relies on model (2) [without BY+NW]; though the lninc-odds ratio remains similar when not restricting the data set. 
dat.rc6 <- dat %>% 
  mutate(east.fct = as.factor(east), 
         berlin.fct = as.factor(berlin)) %>% 
  filter(., !Land %in% c("BY", "NW"))
rc6 <- polr(formula = as.factor(index) ~ lninc + 
  lnincfed + lnvac + lnvacfed + #prevac + 
  att_t_fed + FKM21 + econ_strength + east.fct + 
  berlin.fct + week.fct,
  data = dat.rc6, Hess = T, model = T)
modelsummary(rc6, exponentiate = T, 
  coef_omit = "2|3|week", statistic = "conf.int", 
  vcov = ~date)
# RC4, RC5
### Fixed Effects models (Table 3, Models (4) und (5))
### Note: we were not able to perfectly match the feologit function from stata, hence the values are approximates. We report in the paper the Stata output. 
dat.rc5 <- dat %>% 
  mutate(states.fct = as.factor(statenumber))
rc5A <- polr(formula = as.factor(index) ~ lninc + lnincfed + timetrend + states.fct,
             data = dat.rc5, Hess = T, model = T)
rc5B <- polr(formula = as.factor(index) ~ lninc + lnincfed + lnvac + lnvacfed +  prevac + timetrend + factor(Land),
             data = dat.rc5, Hess = T, model = T)
#
modelsummary(list(rc5A, rc5B), exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3|week|fct|factor",
             conf_level = 0.95
#             vcov = ~date
)

## 2. Consider the week of the MPK's only  (Table 5, Model (1))
dat.rc2 <- dat %>% filter(., weekMPK == 1)
rc2 <- polr(formula = as.factor(index) ~ lninc + lnincfed + 
  timetrend + att_t_fed + FKM21 + econ_strength + lnvac + lnvacfed, 
  data = dat.rc2, Hess = T, model = T)
modelsummary(rc2, exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3|week", 
             vcov = ~date)
### up to three day until a MPK  (Table 5, Model (2))
dat.rc3 <- dat %>% filter(., dayssinceMPK < 4)
rc3 <- polr(formula = as.factor(index) ~ lninc + lnincfed + timetrend + 
              att_t_fed + FKM21 + econ_strength + lnvac + lnvacfed, 
            data = dat.rc3,
            Hess = T, model = T)
modelsummary(rc3, exponentiate = T, 
             statistic = "conf.int", 
             coef_omit = "2|3|week", 
             vcov = ~date)




#overview.list <- list(p5, p5.ai,   # main and main.ai 
#                      rc1,         # BY & NW
#                      rc2,         # Week of PMC
#                      rc3,         # PMC + 3days
#       #               rc4,         # relative dev.
#                      rc5A, rc5B, # FE
#                      rc6         # Dummy East+Berlin
#                      )
#names(overview.list) <- c("Main", "Main.AI", "BY.NW", "PMC.Week", "PMC+3days", "FE1", "FE2", "East.Bln")

#modelplot(overview.list, 
#          exponentiate = T,
#          coef_map = c('lninc' = 'ln(state-level inc. rate)#'),
#          vcov = ~date
#) +
#  geom_vline(xintercept = 1, linetype = "dotdash", #linewidth = 1.5) +
#  theme_gray(base_size = 22) +
#  coord_flip()+
#  labs(x = "Odds and (exp.) 95% CIs")
#  scale_colour_manual(values =c("red", rep("purple", 7))) # +
#  xlim(0.5, 5)
#ggsave("~/Documents/Research/Yardstick/BER/git/6842e459f8d02615df55d365/Presentation/robstnessOverview.pdf", 
#       width = 16, height = 8)          


## alternative plot
#mods <- names(overview.list)

# Get the plotting data (already exponentiated if exponentiate=TRUE)
#df <- modelplot(
#  overview.list,
#  exponentiate = TRUE,
#  coef_map = c('lninc' = 'ln(state-level inc. rate)'),
#  vcov = ~ date,
#  draw = FALSE
#) #%>% filter(., is.na(model)==F)

# Keep only the coefficient you plotted
#df <- subset(df, term == 'ln(state-level inc. rate)')

# Put models on the axis in your specified order
#df$model <- factor(df$model, levels = mods)
#model_order <- c("FE2", "FE1", "East.Bln", "PMC+3days", "PMC.Week", "BY.NW", "Main.AI",  "Main")


#df$model <- factor(df$model, levels = model_order)

#ggplot(df %>% filter(., is.na(model)==F), 
#       aes(x = estimate, y = model, colour = model)) +
#  geom_vline(xintercept = 1, linetype = "dotdash", linewidth = 1.5) +
#  geom_point(size = 3, col = "#C1002B") +
#  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15, col = "#C1002B") +
#  theme_gray(base_size = 22) +
#  guides(colour = "none") +                 # drop legend; axis shows model names
#  labs(y = NULL, x = "Odds and (exp.) 95% CIs")
#ggsave("~/Documents/Research/Yardstick/BER/git/6842e459f8d02615df55d365/Presentation/robstnessOverview.pdf", 
#       width = 16, height = 8)   
