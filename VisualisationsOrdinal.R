## Data Visualization and Result Diplays
library(ggpubr)
library(ggplot2)
library(texreg)
library(stargazer)
# 1. Table 1:
# 1.1: Table with all data
res.main.df <- res.main %>%
    lapply(., tidy, conf.int = TRUE, exp = TRUE) %>%
    bind_rows(., .id = "model") %>%
    filter(., coef.type == "location") 
# 1.2.1 Plot with exponentiated values
vis.fun <- function(m){
  ggplot(data = res.main.df %>% 
           filter(., model %in% m, 
                  coef.type == "location", 
                  !term %in% c("timetrend")) , 
         aes(x =estimate, term, 
             xmin = conf.low, xmax = 3, 
             height = 0)) +
    geom_pointrange(position = position_dodge(width = 0.5)) +
    geom_vline(xintercept = 1, lty = 4, 
               col = "black", 
               size = 2) +
    geom_errorbarh(aes(color = ifelse(term == "lninc", "red", "forestgreen")), size = 2, alpha = 0.5) +
    geom_point(aes(color = ifelse(term == "lninc", "red", "forestgreen")), 
               size = 4) +
    scale_color_identity() + 
    theme_dark(base_size = 22) +
    #   theme(text      = element_text(size=22, family="LM Roman 12")) +
    labs(x = "Coef. (exp)", y = "", title = paste0("Model ",m))
}
plot.list <- lapply(1:5, vis.fun)
all.plots <- ggarrange(plotlist = plot.list, ncol = 2, nrow = 3, common.legend = T)
all.plots
#

# 1.2.2 Visualisation without exp-values:
coef_namesShort <- c(
  "lninc" = "State Incidence Rate (ln)",
  "lnincfed" = "Federal Incidence Rate (ln)",
  "timetrend" = "timetrend",
  "east" = "East Region",
  "berlin" = "Berlin",
  "lnvac" = "State Vaccination Rate (ln)",
  "lnvacfed" = "Fed Vaccination (ln)"
)

plotreg(l = res.main, 
        omit.coef = "1|2|2|3|3|4|scale(date)|date|berlin",
        ci.level = 0.99,
        custom.coef.map = coef_namesShort,
        signif.light = "purple",
        signif.medium = "purple",
        signif.dark = "red",
        insignif.light = "forestgreen",
        insignif.medium = "forestgreen",
        insignif.dark = "forestgreen",
        theme = theme_dark(base_size = 22))
##########################################################################################
## Robustness Checks
# ## R1: Excluding Bavaria and NRW (MASS)
# NONWBY_results.df <- NONWBY %>%
#   lapply(., tidy, conf.int = TRUE, exp = TRUE) %>%
#   bind_rows(., .id = "model") %>%
#   filter(., coef.type == "coefficient", term == "lninc") %>% 
#   dplyr::select("model", "estimate")  %>%
#   mutate(main.estimate = c(1.730, 1.904, 2.003, 1.983, 1.985)) %>%
#   dplyr::select("model", "estimate", "conf.low", "conf.high") %>%
#   setNames(c("Model", "RC.Estimate", "RC.CIlow", "RC.CIhigh")) 
# Comparison with the main analysis above
MAIN_results.df <- res.main.df %>%
  filter(., term == "lninc") %>% 
  dplyr::select("model", "estimate", "conf.low", "conf.high") %>%
  setNames(c("Model", "Main.Estimate", "Main.CIlow", "Main.CIhigh")) 
## R1: Excluding Bavaria and NRW (ordinal)
NONWBY_results.df <- NONWBY %>%
  lapply(., tidy, conf.int = TRUE, exp = TRUE) %>%
  bind_rows(., .id = "model") %>%
  filter(., coef.type == "location", term == "lninc") %>% 
  dplyr::select("model", "estimate", "conf.low", "conf.high") %>%
  setNames(c("Model", "RC.Estimate", "RC.CIlow", "RC.CIhigh")) 
## Compare with results in the main analyses
# bind together
R1.df <- NONWBY_results.df %>% 
  left_join(x = ., y = MAIN_results.df, by = "Model") #%>%
#tidyr::pivot_longer(data =., cols = -c(Model) ) %>%
#  mutate(
#    Analysis = case_when(
#      stringr::str_starts(name, "Main") ~ "Main",
#      stringr::str_starts(name, "RC") ~ "RC",
#      TRUE ~ NA_character_  ))
## Compare results
broom::tidy(NONWBY[[1]], exponentiate = TRUE, conf.int = T) # 1.55 vs. 1.730
broom::tidy(NONWBY[[2]], exponentiate = TRUE, conf.int = T) # 1.80 vs. 1.904
broom::tidy(NONWBY[[3]], exponentiate = TRUE, conf.int = T) # 1.88 vs. 2.003
broom::tidy(NONWBY[[4]], exponentiate = TRUE, conf.int = T) # 2.17 vs. 1.983
broom::tidy(NONWBY[[5]], exponentiate = TRUE, conf.int = T) # 2.14 vs. 1.985
################################################################################  
ggplot(data = R1.df, aes(y = as.numeric(Model))) +
  geom_point(aes(x = Main.Estimate), col = "blue", size = 6, shape = 4) +
  geom_point(aes(x = RC.Estimate),   col = "red", size = 6, shape = 4) +
  geom_errorbarh(aes(xmin = Main.CIlow, xmax = Main.CIhigh, y = as.numeric(Model) - 0.025), col = "blue", height = 0.2, alpha = 0.5) +
  geom_errorbarh(aes(xmin = RC.CIlow, xmax = RC.CIhigh, y = as.numeric(Model) + 0.025), col = "red", height = 0.2, alpha = 0.5) +
  theme_gray(base_size = 22) +
  xlim(0.5, 3.5) +
  geom_vline(xintercept = 1, linetype = "dashed", col = "black")+
  labs(x = "Exp(Estimates)", y = "Model Specification",
       subtitle = "Blue = Main Analyses, Red = NW & BY omitted")

### show results for main analyses without NW & BY
plotreg(l = NONWBY, 
        omit.coef = "1|2|2|3|3|4|scale(date)|date|berlin",
        ci.level = 0.95,
        custom.coef.map = coef_namesShort,
        signif.light = "purple",
        signif.medium = "purple",
        signif.dark = "red",
        insignif.light = "forestgreen",
        insignif.medium = "forestgreen",
        insignif.dark = "forestgreen",
        theme = theme_dark(base_size = 22))

##################################################################
### Generate Tables with exponentiated values (odds)
## here: main analyses.

tr <- extract(res.main[[1]])
texreg(res.main[[1]], 
       override.coef = exp(tr@coef), 
       override.ci.low = exp(tr@coef - 1.96 * tr@se ),
       override.ci.up  = exp(tr@coef + 1.96 * tr@se ),
ci.force = TRUE)

coef_names <- list(
  "lninc" = "State Incidence Rate (ln)",
  "lnincfed" = "Federal Incidence Rate (ln)",
  "timetrend" = "Date",
  "east" = "East Region",
  "att_t_fed" = "General stance",
  "econ_strength" = "Economic Strength",
  "berlin" = "Berlin",
  "lnvac" = "State Vaccination Rate (ln)",
  "lnvacfed" = "Fed Vaccination (ln)",
  "prevac" = "Pre-vaccination"
)
## as.function
texreg(res.main, 
       override.coef = lapply(res.main, function(model) exp(extract(model)@coef)),
       override.ci.low = lapply(res.main, function(model) exp(extract(model)@coef - 1.96 * extract(model)@se)),
       override.ci.up = lapply(res.main, function(model) exp(extract(model)@coef + 1.96 * extract(model)@se)),
       custom.coef.map = coef_names,
       ci.force = TRUE,  ci.test = 1,
       single.row = FALSE,
       use.packages = FALSE,
       omit.coef = "1|2|2|3|3|4|scale(date)|date|berlin",
       file = "~/Documents/Research/Yardstick/BER/rtaball.tex",
       booktabs = TRUE)  



