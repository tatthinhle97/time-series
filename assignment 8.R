library(fpp3)
library(dplyr)
library(ggplot2)


economy <- global_economy %>%
  filter(Country == "United Kingdom") %>%
  select(Year, Exports)


economy %>%
  autoplot(Exports) +
  labs(x="Year", y="Exports") +
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
ann_model <- economy %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))

next_5_years_ets_ann_forecast <- ann_model %>%
  forecast(h = 5)

next_5_years_ets_ann_forecast %>%
  autoplot(economy) +
  labs(x="Year", y="Exports") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

# c.
ann_rmse <- accuracy(ann_model)$RMSE
print(ann_rmse)

# d. + e.
aan_model <- economy %>%
  model(ETS(Exports ~ error("A") + trend("A") + season("N")))

next_5_years_ets_aan_forecast <- aan_model %>%
  forecast(h = 5)

next_5_years_ets_aan_forecast %>%
  autoplot(economy) +
  labs(x="Year", y="Exports") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

aan_rmse <- accuracy(aan_model)$RMSE
print(aan_rmse)

# f.
ann_first_forecast_mean <- next_5_years_ets_ann_forecast$.mean[1]
aan_first_forecast_mean <- next_5_years_ets_aan_forecast$.mean[1]

ann_95_interval <- ann_first_forecast_mean + c(-1.96, 1.96) * ann_rmse
aan_95_interval <- aan_first_forecast_mean + c(-1.96, 1.96) * aan_rmse

print(ann_95_interval)
print(aan_95_interval)

print(next_5_years_ets_ann_forecast %>% 
        hilo() %>% 
        head(1) %>%
        select(Year, `95%`))
print(next_5_years_ets_aan_forecast %>% 
        hilo() %>% 
        head(1) %>%
        select(Year, `95%`))
