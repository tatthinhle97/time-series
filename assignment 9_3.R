library(tidyverse)
library(lubridate)
library(fpp3)
library(dplyr)
library(forecast)
library(ggplot2)
library(tseries)

# a.
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

# b.

# Split data: 90% for training, 10% for testing

# Create a time series object
kic_ts <- kic_df %>%
  mutate(index = row_number()) %>%
  as_tsibble(index = index)

total_rows <- nrow(kic_ts)
split_point <- floor(0.9 * total_rows)

train_ts <- kic_ts[1:split_point, ]
test_ts <- kic_ts[(split_point + 1):total_rows, ]

aan_model <- train_ts %>%
  model(ETS(ndiv_PDCSAP_FLUX ~ error("A") + trend("A") + season("N")))

ets_fc <- aan_model %>%
  forecast(h = nrow(test_ts))

ets_fc %>%
  autoplot(train_ts) +
  labs(y = "Flux of the star", x="Time index") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

# C.
arima_model <- train_ts %>%
  model(ARIMA(ndiv_PDCSAP_FLUX))

arima_fc <- arima_model %>% forecast(h = nrow(test_ts))

# Plot
arima_fc %>%
  autoplot(train_ts, level = NULL) +
  labs(y = "Flux of the star", x="Time index") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

# d.
bind_rows(
  accuracy(ets_fc, test_ts) %>% mutate(model = "ETS"),
  accuracy(arima_fc, test_ts) %>% mutate(model = "ARIMA")
)