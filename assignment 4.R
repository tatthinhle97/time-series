# Load required packages
library(readr)
library(tsibble)
library(lubridate)
library(feasts)
library(fabletools)
library(ggplot2)
library(dplyr)

# 1. Load and prepare data
data <- read.csv("data/sample_file_poly.csv", stringsAsFactors = FALSE)
data$Date <- mdy(data$Date)  

# 2. Convert to tsibble
ts_data <- data %>%
  arrange(Date) %>%
  tsibble::as_tsibble(index = Date)

# 3. Plot Value vs Date
ggplot(ts_data, aes(x = Date, y = Value)) +
  geom_line() +
  labs(title = "Time Series: Value vs Date", x = "Date", y = "Value")

# 4. STL decomposition
stl_fit <- model(
  ts_data,
  STL(Value ~ season(period = "1 year") + season(period = "1 week"))
)
stl_comp <- components(stl_fit)

# 5. Classical decomposition (corrected)
classical_fit <- model(
  ts_data, 
  classical_decomposition(Value ~ season("year"))
)
classical_comp <- components(classical_fit)
autoplot(classical_comp) + ggtitle("Classical Decomposition")

# 6. Plot both decompositions
autoplot(stl_comp) + ggtitle("STL Decomposition")
autoplot(classical_comp) + ggtitle("Classical Decomposition")

# 7. Extract residuals
stl_resid <- stl_comp[, c("Date", "remainder")]
names(stl_resid)[2] <- "STL_Residual"

# Classical uses 'random' instead of 'remainder'
classical_resid <- classical_comp[, c("Date", "random")]
names(classical_resid)[2] <- "Classical_Residual"

# 8. Merge residuals by Date
merged_resid <- merge(stl_resid, classical_resid, by = "Date", all = TRUE)

# === OPTION 2: Show missing rows, if any
na_rows <- merged_resid[!complete.cases(merged_resid), ]
if (nrow(na_rows) > 0) {
  cat("⚠️ Missing values in residuals at:\n")
  print(na_rows)
}

# 9. Do NOT drop NAs (retain for plotting)
# merged_resid <- na.omit(merged_resid)  # <- Optional, but not used here

# 10. Convert to long format for plotting residuals
merged_resid_long <- data.frame(
  Date = rep(merged_resid$Date, 2),
  Residual = c(merged_resid$STL_Residual, merged_resid$Classical_Residual),
  Method = rep(c("STL", "Classical"), each = nrow(merged_resid))
)

# 11. Plot residual comparison
ggplot(merged_resid_long, aes(x = Date, y = Residual, color = Method)) +
  geom_line(na.rm = TRUE) +   # === OPTION 3: Suppress NA warning
  labs(title = "Residuals: STL vs Classical", x = "Date", y = "Residual")

# 12. Residual summary stats
stl_sd <- sd(merged_resid$STL_Residual, na.rm = TRUE)
classical_sd <- sd(merged_resid$Classical_Residual, na.rm = TRUE)
stl_mean <- mean(merged_resid$STL_Residual, na.rm = TRUE)
classical_mean <- mean(merged_resid$Classical_Residual, na.rm = TRUE)

cat("\nResidual Summary:\n")
cat(sprintf("STL       - Mean: %.4f  SD: %.4f\n", stl_mean, stl_sd))
cat(sprintf("Classical - Mean: %.4f  SD: %.4f\n", classical_mean, classical_sd))
