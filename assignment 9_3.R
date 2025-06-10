library(tidyverse)
library(lubridate)
library(fpp3)
library(dplyr)

kic_df <- read_csv("data/KIC9832227.csv")

kic_df <- kic_df %>%
  rename(JD_Time = "JD Time")

kic_df %>%
  ggplot(aes(x = TIME, y = ndiv_PDCSAP_FLUX)) +
  geom_line(size=1) +
  labs(title = "Flux vs Julian Date", 
       y = "Flux of the star", 
       x = "Julian Date") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )