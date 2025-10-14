## Figure 4 
## picture to address R2's request for a visualization
states <- data.frame(
  statenumber = 2:17, 
  Land = c(
    "BW", "BY", "BE", "BR", "HB", "HH", "HE", "MV",
    "NI", "NW", "RP", "SL", "SN", "ST", "SH", "TH")
  )

## already present: 'dat' from the main replication file
plot.data <- dat %>% 
  dplyr::select(., c("index", "lninc", "lnincfed", "date", "statenumber")) %>%
  mutate(inc.dev = lninc - lnincfed) %>% 
  left_join(x = ., y = states, by = "statenumber")

# General scaling constants
m_inc <- mean(plot.data$inc.dev, na.rm = TRUE)
s_inc <- sd(plot.data$inc.dev, na.rm = TRUE)
m_idx <- mean(plot.data$index,   na.rm = TRUE)
s_idx <- sd(plot.data$index,     na.rm = TRUE)

# Helpers
scale_index_to_inc <- function(x) ((x - m_idx)/s_idx) * s_inc + m_inc
inv_scale          <- function(y) ((y - m_inc)/s_inc) * s_idx + m_idx

## all states
R2pic.all <- ggplot(plot.data, 
       aes(x = date)) +
  geom_line(aes(y = inc.dev, colour = "inc.dev")) +
  geom_point(aes(y = scale_index_to_inc(index), colour = "index"), alpha = 0.7) +
  geom_smooth(
    aes(y = scale_index_to_inc(index), colour = "index (loess)"),
    method = "loess", se = FALSE, linewidth = 0.8
  ) +
  scale_y_continuous(
    name = "inc.dev",
    sec.axis = sec_axis(~ inv_scale(.), name = "index")
  ) +
  scale_colour_manual(values = c("inc.dev" = "steelblue", "index" = "orange"), name = NULL) +
  facet_wrap(~ Land) +
  theme_minimal(base_size = 22) +
  theme(legend.position = "bottom")


## NW, ST, TH
R2pic.three <- ggplot(plot.data %>% filter(., Land %in% c("NW", "ST", "TH")), 
       aes(x = date)) +
  geom_line(aes(y = inc.dev, colour = "inc.dev")) +
  geom_point(aes(y = scale_index_to_inc(index), colour = "index"), alpha = 0.7) +
  geom_smooth(
    aes(y = scale_index_to_inc(index), colour = "index (loess)"),
    method = "loess", se = FALSE, linewidth = 0.8, col = "orange", linetype = "dashed"
  ) +
  scale_y_continuous(
    name = "ln(incstate) - ln(incfed)",
    sec.axis = sec_axis(~ inv_scale(.), name = "index")
  ) +
  scale_colour_manual(values = c("inc.dev" = "steelblue", "index" = "orange"), name = NULL) +
  facet_wrap(~ Land) +
  theme_minimal(base_size = 22) +
  theme(
    axis.title.y = element_text(colour = "steelblue"),
    axis.title.y.right = element_text(colour = "orange"),
    legend.position = "none"
  )#+
#  theme(legend.position = "bottom")
ggsave("R2picTHREE.pdf", 
       width = 16, height = 9, 
       plot = R2pic.three)


#ggsave("~/Documents/Research/Yardstick/BER/git/6842e459f8d02615df55d365/PUCH/R2picALL.pdf", 
#       width = 16, height = 9, plot = R2pic.all)



