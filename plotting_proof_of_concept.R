library(tidyverse)
library(lubridate)

all <- tibble(date = ymd(c("2020-1-1", "2021-1-1")),
              pic_obs_cummulative = c(2, 8),
              herbarium_obs_cummulative = c(3, 6))

all$sum <- all$pic_obs_cummulative + all$herbarium_obs_cummulative

ggplot(all) +
  # Fill between x-axis and pic_obs_cummulative
  geom_ribbon(aes(x = date, ymin = 0, ymax = pic_obs_cummulative), fill = "lightblue") +
  # Fill between pic_obs_cummulative and sum
  geom_ribbon(aes(x = date, ymin = pic_obs_cummulative, ymax = sum), fill = "lightgreen") +
  # Add pic_obs_cummulative line
  geom_line(aes(x = date, y = pic_obs_cummulative), color = "red") +
  # Add sum line
  geom_line(aes(x = date, y = sum))

