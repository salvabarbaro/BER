## Brant test : proportional odds assumption
## We use data set df1 from MainAnalysisOrdinal.R
## By mainanalysis.list, we apply the list of models, refer to MainAnalysisOrdinal.R
library(brant)
library(MASS)  # The Brant-Test applies to polr-objects from the MASS package
## 1. Brant test for the main model (5):
MASS::polr(formula = mod5, data = df1) %>% brant(.) %>% head(1)

## Supplementary: Brant test for all five model spezifications.
brant.list <- lapply(X = mainanalysis.list, 
                     FUN =function(mod){
                       MASS::polr(formula = mod, data = df1) %>% 
                         brant(.)})

brant.list.fun <- function(val){
  omnibus <- brant.list[[val]] %>% as.data.frame(.) %>% head(1)
  return(omnibus)
}
brant.df <- lapply(1:5, brant.list.fun) %>% do.call(rbind, .) %>%
  mutate(Model = 1:5, .before = "X2") %>% as_tibble(.) %>%
  setNames(c("Model", "Chi.Sq", "df", "p"))
brant.df
gt::gt(brant.df)





