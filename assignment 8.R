library(fpp3)
library(dplyr)


economy <- global_economy %>%
  filter(Country == "United Kingdom") %>%
  select(Year, Exports)


economy %>%
  autoplot(Exports) +
  labs(x="Year", y="Exports") +
  geom_line(size=1) +
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
next_5_years_ets_ann_forecast <- economy %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N"))) %>%
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