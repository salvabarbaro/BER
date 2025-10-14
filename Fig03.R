library(dplyr)
library(reshape2)
library(ggplot2)
library(ggpubr)
#library(lubridate)
library(scales)
library(viridis)
#
setwd("~/Documents/Research/Yardstick/BER/github/BER/")
url <- "https://pada.psycharchives.org/bitstream/9ff033a9-4084-4d0e-87eb-aa963a1324a5"
## ~/Documents/Research/Yardstick/DATA/Lockdown data-V6.0.csv

lockdown.df <- read.csv(url) %>%
  filter(., !Measure == "") %>%
  melt(., id.vars = c("X", "state", "Measure")) %>%
  mutate(Date = as.Date(variable, format = "X%Y.%m.%d")) %>%
  select(., -c("variable")) %>%
  mutate(value = as.numeric(value))
##
abb.df <- data.frame(
  state = unique(lockdown.df$state),
  abbreviation = c(
    "BW", "BY", "BE", "BR",
    "HB", "HH", "HE", "MV",
    "NI", "NW", "RP", "SL",
    "SN", "ST", "SH", "TH")
  )

lockdown.df <-  lockdown.df %>% left_join(x = ., y = abb.df, by = "state")
avg.df <- lockdown.df %>% group_by(Measure, Date) %>%
  reframe(mn = mean(value, na.rm = T))
df <- lockdown.df  %>% 
  left_join(x = ., y = avg.df, 
            by = c("Date", "Measure" ))
############################################################
## Example Fig: School
#ggplot(data = lockdown.df %>% filter(., Measure == "school"),
#       aes(x = Date, y = value, group = abbreviation, colour = abbreviation)) +
#  geom_line() 

meas.fun <- function(m){
  ggplot(data = lockdown.df %>% filter(., Measure == m),
         aes(x = Date, y = as.factor(value), 
             group = abbreviation, colour = abbreviation)) +
    geom_rect(aes(xmin=as.Date("2020-03-02"), xmax=as.Date("2020-05-17"), ymin=-Inf, ymax=Inf), fill='gray97', alpha=.2, col = "gray35") +
    geom_rect(aes(xmin=as.Date("2020-09-28"), xmax=as.Date("2021-02-28"), ymin=-Inf, ymax=Inf), fill='gray97', alpha=.2, col = "gray35") +
    geom_rect(aes(xmin=as.Date("2021-03-01"), xmax=as.Date("2021-06-13"), ymin=-Inf, ymax=Inf), fill='gray97', alpha=.2, col = "gray35") +
    geom_line() + 
    theme_gray(base_size = 22) + 
    theme(legend.position = "none") +
    scale_x_date(date_breaks = "4 month", 
                 labels = date_format("%m-%Y"))  +
    scale_colour_viridis_d(option = "viridis", direction = -1) +
    labs(title = m, y = "Index", x = " ")
}
meas.pics <- lapply(as.list(unique(lockdown.df$Measure)), 
                    meas.fun)
#
meas <- ggarrange(plotlist = meas.pics, 
                  ncol = 4, nrow = 4)
meas
#ggsave("pics/meas.pdf", plot = meas, width = 16, height = 9)

## selected measures:
# unique(lockdown.df$Measure): 1  = leavehome, 
#                              9  = school,
#                              14 = daycare
sel.measfigs <- meas.pics[c(1, 9, 14)]
sel.meas <- ggarrange(plotlist = sel.measfigs, 
                      ncol = 1, nrow = 3, common.legend = F)
sel.meas
ggsave("lockdownfig.pdf", width = 16, height = 9,
       plot = sel.meas)




