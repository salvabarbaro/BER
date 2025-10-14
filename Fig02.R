## We successfully run the script in November 2024. Re 
# realized an issue while using the data through gtrendsR::gtrends. Thus, please use the more robust python script 

## Google Trends, Fig 2
library(gtrendsR)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(anytime)
#setwd("~/Documents/Research/Yardstick/BER/github/BER/")

keywords.all <- c("MPK", "Ministerpräsidentenkonferenz",
 "corona mpk", "beschlüsse mpk", 
 "ergebnisse ministerpräsidentenkonferenz")

mpk <- gtrends(keyword = keywords.all, 
               geo = "DE",
               time = "2020-03-01 2021-09-21",
               onlyInterest = TRUE, 
               tz = -120)

mpk.time <- mpk$interest_over_time 
## MPK-Tagungen: https://www.vdek.com/politik/corona-sars-cov-2-politik-archiv.html

mpk.dates <- as.Date(c("2020-03-12", "2020-03-16",
 "2020-03-22", "2020-04-01", "2020-04-15", "2020-04-30",
 "2020-05-06", "2020-05-26",  "2020-06-17", "2020-07-16", "2020-08-27", "2020-09-29","2020-10-14", "2020-10-28", "2020-11-16", "2020-11-25", "2020-12-13", "2021-01-05", "2021-01-19", "2021-02-01", "2021-02-10", "2021-03-03", "2021-03-19", "2021-03-22", "2021-04-26", "2021-05-27", "2021-08-10"))

ggplot(data = mpk.time,
       aes(x = as.Date(date), y = as.numeric(hits), 
           group = keyword, colour = keyword))+
  geom_line() +
  geom_vline(xintercept = as.numeric(mpk.dates), 
             col = "red", linetype = "dashed")

mpk.time.group <- mpk.time %>% group_by(date) %>% 
  reframe(hit.sum = sum(hits, na.rm = T)) %>% 
  mutate(hit.norm = hit.sum / max(hit.sum, na.rm = T) )

ggplot(data = mpk.time.group, aes(x = as.Date(date), y = hit.norm)) +
  geom_line(col = "purple") +
  geom_vline(xintercept = as.numeric(mpk.dates), 
             col = "forestgreen", linetype = "dotted", linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(x = "", y = "Relative Interest") +
  theme_gray(base_size = 20)



rects <- data.frame(
   xmin = mpk.dates - 3,
   xmax = mpk.dates + 1,
   ymin = -Inf,
   ymax = Inf
 ) %>% 
   mutate(startdate = as.Date(xmin),
          enddate =   as.Date(xmax))

 mpkdates.df <- mpk.dates %>% as.data.frame(.) %>% setNames(c("date"))

# geom_rect(data = rects, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
#fill = "green", alpha = 0.2, inherit.aes = FALSE)

all_dates <- seq.Date(from = min(mpk.df$mpk.time), to = max(mpk.df$mpk.time), by = "day")
 
mpkday.df <- data.frame(date = all_dates) %>%
   left_join(x = ., y = mpk.time, by = "date") %>%
   fill(hits, .direction = "down") 
 
 
ggplot(data = mpk.time.group, aes(x = as.Date(date), y = hit.norm)) +
  geom_line(col = "purple") +
  geom_point(data = mpkdates.df, aes(x = date, y = 0), 
             colour = "forestgreen", size = 4) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(x = "", y = "Relative Interest") +
  theme_gray(base_size = 20)
ggsave("~/Documents/Research/Yardstick/BER/mpk.pdf", 
       width = 16, height = 9)

ggplot(data = mpkday.df, 
       aes(x = as.Date(date), y = hits/100)) +
  geom_line(col = "purple") +
  geom_point(data = mpkdates.df, aes(x = date, y = 0), 
             colour = "forestgreen", size = 4) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(x = "", y = "Relative Interest") +
  theme_gray(base_size = 20)
ggsave("~/Documents/Research/Yardstick/BER/mpk.pdf", 
       width = 16, height = 9)

######################################################################
ggplot(data = mpk.time.group, aes(x = as.Date(date), y = hit.norm)) +
  geom_line(col = "purple") +
  geom_rect(data = rects, aes(xmin = startdate, xmax = enddate, 
                              ymin = -Inf, ymax = Inf), 
            fill = "green", alpha = 0.2, inherit.aes = FALSE) +
    scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(x = "", y = "Relative Interest") +
  theme_gray(base_size = 20)




#########################################################################
### new plot with python data:
ggplot(data = mpk.df, 
       aes(x = mpk.time, y = hits)) +
  geom_line() +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  geom_point(data = mpkdates.df, aes(x = date, y = 0), 
             colour = "forestgreen", size = 4) 


## week-wise:
all_dates <- seq.Date(from = min(mpk.df$mpk.time), to = max(mpk.df$mpk.time), by = "day")

mpkday.df <- data.frame(mpk.time = all_dates) %>%
  left_join(x = ., y = mpk.df, by = "mpk.time") %>%
  fill(hits, .direction = "down")


ggplot(data = mpkday.df, 
       aes(x = mpk.time, y = hits/100)) +
  geom_rect(aes(xmin=as.Date("2020-03-02"), xmax=as.Date("2020-05-17"), ymin=-Inf, ymax=Inf), fill='gray97', alpha=.1, col = "gray35") +
  geom_rect(aes(xmin=as.Date("2020-09-28"), xmax=as.Date("2021-02-28"), ymin=-Inf, ymax=Inf), fill='gray97', alpha=.1, col = "gray35") +
  geom_rect(aes(xmin=as.Date("2021-03-01"), xmax=as.Date("2021-06-13"), ymin=-Inf, ymax=Inf), fill='gray97', alpha=.1, col = "gray35") +
  geom_line(col = "purple", linewidth = 1.4) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::percent) +
  geom_point(data = mpkdates.df, aes(x = date, y = 0.0), 
             colour = "forestgreen", size = 5) +
  theme_gray(base_size = 22) +
  labs(x = "", y = "Relative Interest") 

#
ggsave("~/Documents/Research/Yardstick/BER/mpk2.pdf", 
       width = 16, height = 9)

