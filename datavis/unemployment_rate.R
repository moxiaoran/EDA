library(tidyverse)
library(lubridate)
library(scales)
theme_set(theme_minimal())

ADP <- tq_get("NPPTTL", get = "economic.data")

ADP %>%
  mutate(change = (price - dplyr::lag(price, k = 1 )) / 1000,
         year = year(ymd(date)),
         month = month(ymd(date)),
         time = year + month / 12) %>%
  ggplot(aes(time, change)) +
  geom_line() +
  annotate("rect", xmin = c(2008, 2020), xmax = c(2010, 2020.5), ymin = c(-1, -21), ymax = c(0, 0), alpha = .2, 
           color = "blue", fill = "blue") +
  annotate("curve", alpha = .5, x = 2008, y = -1, xend = 2007, yend = -4,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 2007, y = -5, label = "2008 Great Recession",
           hjust = "left", alpha = .8) +
  annotate("curve", alpha = .5, x = 2020, y = -20, xend = 2018, yend = -15,
           curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x= 2016, y = -15, label = "COVID-19",
           curvature = .3, alpha = .8, arrow = arrow(length = unit(2, "mm"))) +
  scale_y_continuous(labels = comma_format()) +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(title = "ADP Employment Change",
        x= "", y = "",
       subtitle = "Total Nonfarm Private Payroll Employment (in million)",
       caption = "Source: FRED | Author: Yifei Liu") 

ggsave("ADP.png", width = 12, height = 8)



