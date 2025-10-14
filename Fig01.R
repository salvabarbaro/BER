library(COVID19)
library(dplyr)
library(ggplot2)
library(lubridate)
library(reshape2)
library(zoo)
library(scales)
library(ggpubr)
library(viridis)
library(countrycode)
library(directlabels)

setwd("~/Documents/Research/Yardstick/BER/github/BER/")

G7 <- c("DEU", "ITA", "USA", "GBR", "FRA", "CAN", "JPN" )
covid19.G7 <- covid19(country = G7 , 
                      level = 1, 
                      start = "2020-03-01", 
                      end = "2021-09-27", 
                      verbose = F)  %>%
  group_by(administrative_area_level_1) %>% 
  mutate(dailycases = c(0, diff(confirmed))) %>%
  group_by(administrative_area_level_1) %>% 
  mutate(sevendayssum = rollapplyr(dailycases, 7, sum, partial = TRUE)) %>% 
  mutate(sevendayincidence = sevendayssum / population * 100000) %>%
  mutate(sevendayinc.na = ifelse(sevendayincidence < 0, NA, sevendayincidence ))

cov.g1 <- ggplot(data = covid19.G7 %>% 
         filter(., !iso_alpha_2 %in% c("DE")) , 
       aes(x = date, y = sevendayinc.na, 
           colour = iso_alpha_2, group = iso_alpha_2)) +
  geom_line() +
  geom_dl(aes(label=iso_alpha_2), method=list("last.points", cex = 1.2) ) +
  scale_colour_viridis_d(option = "viridis", direction = -1) +
  geom_line(data = covid19.G7 %>% 
              filter(., iso_alpha_2 %in% c("DE")),
            aes(x = date, y = sevendayinc.na), 
            col = "orange", linewidth = 1.2) +
  geom_dl(data = covid19.G7 %>% 
            filter(., iso_alpha_2 %in% c("DE")),
          aes(x = date, y = sevendayinc.na, label = iso_alpha_2), 
          col = "orange", method = list("last.points", cex = 1.2)) +
  scale_x_date(date_breaks = "4 month", labels = date_format("%m-%Y")) +
  theme_gray(base_size = 22) + 
  theme(legend.position = "none") +
  labs(x = "", y = "7-Day-Incidence")

covid19.laender <- covid19(country = "DEU", 
                           level = 2, 
                           start = "2020-03-01", 
                           end = "2021-09-27", 
                           verbose = F) %>% 
  group_by(administrative_area_level_2) %>% 
  mutate(dailycases = c(0, diff(confirmed))) %>%
  group_by(administrative_area_level_2) %>% 
  mutate(sevendayssum = rollapplyr(dailycases, 7, sum, partial = TRUE)) %>% 
  mutate(sevendayincidence = sevendayssum / population * 100000)

cov.g2 <- ggplot(data = covid19.laender,
                 aes(x = date, y = sevendayincidence, 
                     group = administrative_area_level_2,
                     colour = administrative_area_level_2)) +
  geom_rect(aes(xmin=as.Date("2020-03-02"), xmax=as.Date("2020-05-17"), ymin=-Inf, ymax=Inf), fill='gray97', alpha=.2, col = "gray35") +
  geom_rect(aes(xmin=as.Date("2020-09-28"), xmax=as.Date("2021-02-28"), ymin=-Inf, ymax=Inf), fill='gray97', alpha=.2, col = "gray35") +
  geom_rect(aes(xmin=as.Date("2021-03-01"), xmax=as.Date("2021-06-13"), ymin=-Inf, ymax=Inf), fill='gray97', alpha=.2, col = "gray35") +
  geom_line(col = "#440154FF", alpha = 0.5) +
  geom_line(data = covid19.G7 %>% 
              filter(., iso_alpha_2 %in% c("DE")),
            aes(x = date, y = sevendayincidence), 
            col = "orange", linewidth = 1.2) +
  scale_x_date(date_breaks = "4 month", labels = date_format("%m-%Y")) +
#  scale_x_date(labels = date_format("%m-%Y")) +
  theme_gray(base_size = 22) + theme(legend.position = "none") +
  labs(x = "", y = "7-Day-Incidence")

berfig.01 <- ggarrange(cov.g1, cov.g2, ncol = 2)
ggsave("berfig01.pdf", plot = berfig.01, width = 16, height = 9)
