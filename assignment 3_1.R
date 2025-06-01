# Load necessary libraries
library(ggplot2)
library(forecast)

# Read the data
data <- read.csv("data/monthly-temperature-nj.csv", header = TRUE)

# Combine Month and Year into a Date column (assume first day of month)
data$FormattedDate <- as.Date(paste0(data$Date, 1), format = "%Y%m%d")

# View the first few rows to confirm structure
head(data)

# Ensure the data is in chronological order
data <- data[order(data$FormattedDate), ]

# Create a time series object
# Assumes monthly data; start date is taken from the first entry
start_year <- as.numeric(format(min(data$FormattedDate), "%Y"))
start_month <- as.numeric(format(min(data$FormattedDate), "%m"))
# 12: monthly
temp_ts <- ts(data$Value, start = c(start_year, start_month), frequency = 12)

# STL decomposition (Seasonal-Trend-Loess)
stl_result <- stl(temp_ts, s.window = "periodic")

# Plot the decomposition
plot(stl_result)

# Extract and export the trend component
trend_component <- stl_result$time.series[, "trend"]
trend_df <- data.frame(Date = data$FormattedDate, Trend = as.numeric(trend_component))
write.csv(trend_df, "output-data/monthly-temperature-nj-trend-component.csv", row.names = FALSE)

# Plot the trend component separately
ggplot(trend_df, aes(x = Date, y = Trend)) +
  geom_line(size=1) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y")
  xlab("Date") +
  ylab("Temperature (°F)") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16, hjust = 0.5),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14)
  )