library(fpp3)
library(dplyr)
library(ggplot2)


# a.
data <- aus_airpassengers %>% 
  filter(Year >= 1970 & Year <= 2011) %>%
  select(Year, Passengers)

arima_model_1 <- data %>% model(ARIMA(Passengers))

report(arima_model_1)

arima_model_1 %>% 
  gg_tsresiduals()

next_10_years_forecast <- arima_model_1 %>% forecast(h = 10)
next_10_years_forecast %>% autoplot(data) +
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
arima_model_2 <- data %>% model(ARIMA(Passengers ~ 1 + pdq(0,1,0)))

report(arima_model_2)

arima_model_1_2 <- data %>%
  model(
    PartA = ARIMA(Passengers),
    PartC = ARIMA(Passengers ~ 1 + pdq(0,1,0))
  )

next_10_years_forecast <- arima_model_1_2 %>% forecast(h = 10)
next_10_years_forecast %>% autoplot(data) +
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
arima_model_1_2_3 <- data %>%
  model(
    PartA = ARIMA(Passengers),
    PartC = ARIMA(Passengers ~ 1 + pdq(0,1,0)),
    PartD = ARIMA(Passengers ~ 1 + pdq(2,1,2))
  )

next_10_years_forecast <- arima_model_1_2_3 %>% forecast(h = 10)
next_10_years_forecast %>% autoplot(data) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

# e.

arima_model_1_4 <- data %>%
  model(
    PartA = ARIMA(Passengers),
    PartE = ARIMA(Passengers ~ 1 + pdq(0,2,1))
  )

next_10_years_forecast <- arima_model_1_4 %>% forecast(h = 10)
next_10_years_forecast %>% autoplot(data) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )