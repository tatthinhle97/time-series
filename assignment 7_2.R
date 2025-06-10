library(fpp3)
library(dplyr)

afghanistan_population <- global_economy %>%
  filter(Country == "Afghanistan")

afghanistan_population %>%
  tsibble(index = Year)%>%
  ggplot(aes(x = Year, y = Population)) +
    geom_line(size=1) +
    labs(x = "Year", 
         y = "Population") +
    theme_minimal() +
    theme(
      axis.title.x = element_text(size = 16, hjust = 0.5),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    )


linear_trend_model <- afghanistan_population %>%
  model(Linear = TSLM(Population ~ trend()))

linear_trend_model %>% report()

piecewise_linear_trend_model <- afghanistan_population %>%
  model(Piecewise_Linear = TSLM(Population ~ trend(knots = c(1980, 1989))))

piecewise_linear_trend_model %>% report()

fit <- afghanistan_population %>%
  model(
    Linear = TSLM(Population ~ trend()),
    Piecewise_Linear = TSLM(Population ~ trend(knots = c(1980, 1989)))
  )

augment(fit) %>%
  autoplot(.fitted) +
  geom_line(aes(y = Population), colour = "black", size=1) +
  labs(x = "Year", 
       y = "Population") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )

next_5_years_forecast <- fit %>% forecast(h = "5 years")

autoplot(next_5_years_forecast) +
  autolayer(afghanistan_population, Population) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )