# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)
library(tsibble)
library(fabletools)
library(feasts)
library(ggplot2)

# Read the CSV file
file_path <- "data/nvax.csv"
stock_data <- read_csv(file_path)

# Rename columns
colnames(stock_data) <- c("Date", "Close")

# Convert to tsibble and parse date
stock_data <- stock_data %>%
  filter(!is.na(Date)) %>%
  mutate(Date = ymd(Date)) %>%
  arrange(Date) %>%
  as_tsibble(index = Date)

# Aggregate to monthly frequency
monthly_stock <- stock_data %>%
  index_by(Month = yearmonth(Date)) %>%
  summarise(Close = mean(Close, na.rm = TRUE))

# Perform STL decomposition using fpp3's STL()
stl_result <- monthly_stock %>%
  model(stl = STL(Close))

# Extract components and plot
components <- components(stl_result)

# Plot the STL decomposition
autoplot(components)
