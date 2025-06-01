library(fpp3)

# Ensures that any random sampling or randomness that follows will be 
# reproducible
set.seed(20250528)

series <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))

autoplot(series, Turnover) +
  geom_line(size=1) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black")
  )

gg_season(series, Turnover) +
  geom_line(size=1) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black")
  )

gg_subseries(series, Turnover) +
  geom_line(size=1) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 90),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black")
  )

gg_lag(series, Turnover) +
  geom_line(size=1) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black")
  )

series %>% ACF(Turnover) %>% autoplot() +
  geom_line(size=1) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.line = element_line(color = "black")
  )
