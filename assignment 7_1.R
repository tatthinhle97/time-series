library(ggplot2)
library(readr)
library(lubridate)
library(MASS)

data_2s_df <- read.csv("data/Chap_7_HW_File_2s.csv")
data_3s_df <- read.csv("data/Chap_7_HW_File_3s.csv")

# Convert to correct date format
data_2s_df$Date <- ymd(data_2s_df$Date)  
data_3s_df$Date <- mdy(data_3s_df$Date) 

# Split the data
split_data <- function(data) {
  total_rows <- nrow(data)
  split_index <- 1:floor(0.9 * total_rows)
  train_df <- data[split_index, ]
  test_df <- data[-split_index, ]
  list(train_df = train_df, test_df = test_df)
}

split_2s_df <- split_data(data_2s_df)
split_3s_df <- split_data(data_3s_df)

# Function to create models
create_models <- function(train_df) {
  models <- list(
    P1       = lm(Data ~ P1, data = train_df),
    P2       = lm(Data ~ P2, data = train_df),
    P3       = lm(Data ~ P3, data = train_df),
    P1_P2    = lm(Data ~ P1 + P2, data = train_df),
    P1_P3    = lm(Data ~ P1 + P3, data = train_df),
    P2_P3    = lm(Data ~ P2 + P3, data = train_df),
    P1_P2_P3 = lm(Data ~ P1 + P2 + P3, data = train_df)
  )
  return(models)
}

models_2s <- create_models(split_2s_df$train_df)
models_3s <- create_models(split_3s_df$train_df)

# Evaluate models
evaluate_models <- function(models, test_df) {
  results_df <- data.frame(Model = character(), 
                           RMSE = numeric(), 
                           stringsAsFactors = FALSE)
  
  for (name in names(models)) {
    model <- models[[name]]
    
    preds <- predict(model, newdata = test_df)
    rmse <- sqrt(mean((test_df$Data - preds)^2))
    
    results_df <- rbind(results_df, 
                        data.frame(Model = name, RMSE = rmse))
  }
  
  results_df <- results_df[order(results_df$RMSE), ]
  
  return(results_df)
}

results_2s <- evaluate_models(models_2s, split_2s_df$test_df)
results_3s <- evaluate_models(models_3s, split_3s_df$test_df)

print(results_2s)
print(results_3s)

# Stepwise selection
stepwise_2s <- stepAIC(lm(Data ~ P1 + P2 + P3, data = split_2s_df$train_df), 
                          direction = "both")
stepwise_3s <- stepAIC(lm(Data ~ P1 + P2 + P3, data = split_3s_df$train_df), 
                          direction = "both")

summary(stepwise_2s)
summary(stepwise_3s)

# Plot best model
plot_best_model <- function(best_model, test_df) {
  preds <- predict(best_model, newdata = test_df)
  plot_df <- data.frame(Actual = test_df$Data, Predicted = preds)
  
  ggplot(plot_df, aes(x = Actual, y = Predicted)) +
    geom_line(size=1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
    labs(x = "Test data", 
         y = "Predicted") +
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

plot_best_model(stepwise_2s, split_2s_df$test_df)
plot_best_model(stepwise_3s, split_3s_df$test_df)