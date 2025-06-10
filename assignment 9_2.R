library(fpp3)
library(dplyr)
library(ggplot2)

# a.
us_gdp <- global_economy %>%
  filter(Country == "United States") %>%
  select(Year, GDP)

lambda <- us_gdp %>% 
  features(GDP, features = guerrero) %>% 
  pull(lambda_guerrero)

us_gdp <- us_gdp %>%
  mutate(GDP_transformed = box_cox(GDP, lambda))

# b.
arima_model <- us_gdp %>%
  model(ARIMA(GDP_transformed))

report(arima_model)

# c.
arima_model_1_2_3 <- us_gdp %>%
  model(
    ARIMA_011 = ARIMA(GDP_transformed ~ pdq(0,1,1)),
    ARIMA_110 = ARIMA(GDP_transformed ~ pdq(1,1,0)),
    ARIMA_111 = ARIMA(GDP_transformed ~ pdq(1,1,1))
  )

glance(arima_model_1_2_3)

# d.
best_model <- arima_model_1_2_3 %>% select(ARIMA_110)
best_model %>% gg_tsresiduals()
best_model %>% augment() %>% features(.innov, ljung_box, lag = 10)

# e.
forecast_arima <- best_model %>% forecast(h = "10 years")

forecast_arima %>%
  autoplot(us_gdp) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

# f.
ets_model <- us_gdp %>%
  model(
    ETS = ETS(GDP)
  )

forecast_ets <- ets_model %>% forecast(h = "10 years")

forecast_ets %>%
  autoplot(us_gdp) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )