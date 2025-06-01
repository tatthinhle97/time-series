library(ggplot2)
library(dplyr)
library(readr)
library(fpp2)


climate_A_df <- read.csv("data/Climate_A.csv")
climate_B_df <- read.csv("data/Climate_B.csv")
climate_C_df <- read.csv("data/Climate_C.csv")


# Functions

transform_data <- function(df) {
  df$MonthName <- factor(df$Month, levels = 1:12, labels = month.abb)
  df$Year_factor <- factor(df$Year)
  return(df)
}

create_scatter_plot <- function(df){
  ggplot(df, 
         aes(x = MonthName, 
             y = Temperature,
             color = Year_factor)) +
    geom_point(size=6) +
    theme_minimal() +
    labs(x = "Month",
         y = "Temperature"
    ) +
    theme(
      axis.title.x = element_text(size = 16, hjust = 0.5),
      axis.title.y = element_text(size = 16),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    )
}

create_seasonal_plot <- function(df){
  # https://otexts.com/fpp2/seasonal-plots.html
  time_series <- ts(df$Temperature,
                start = min(df$Year),
                frequency = 12)
  
  ggseasonplot(time_series, 
               year.labels = TRUE, 
               year.labels.left = TRUE) +
    labs(x = "Month",
         y = "Temperature",
         title = ""
    ) +
    geom_point(size=6) +
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
}

create_seasonal_polar_plot <- function(df){
  # https://otexts.com/fpp2/seasonal-plots.html
  time_series <- ts(df$Temperature,
                    start = min(df$Year),
                    frequency = 12)
  
  ggseasonplot(time_series, 
               year.labels = TRUE,
               polar = TRUE) +
    labs(x = "Month",
         y = "Temperature",
         title = ""
    ) +
    geom_point(size=3) +
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
}

# Transform data

climate_A_df <- transform_data(climate_A_df)
climate_B_df <- transform_data(climate_B_df)
climate_C_df <- transform_data(climate_C_df)

# Plot the temperature versus date as a scatter plot for the three files on 
# separate plots.

create_scatter_plot(climate_A_df)
create_scatter_plot(climate_B_df)
create_scatter_plot(climate_C_df)

# Plot the temperature versus date as a seasonal plot for the three files on 
# separate plots.

create_seasonal_plot(climate_A_df)
create_seasonal_plot(climate_B_df)
create_seasonal_plot(climate_C_df)

# Plot the temperature versus date as a seasonal polar plot for the three files 
# on separate plots
create_seasonal_polar_plot(climate_A_df)
create_seasonal_polar_plot(climate_B_df)
create_seasonal_polar_plot(climate_C_df)


