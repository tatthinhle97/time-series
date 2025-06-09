library(fpp3)
library(dplyr)

gc_tourism <- tourism %>%
  filter(Region == "Gold Coast") %>%
  summarise(Total_Trips = sum(Trips))

gc_tourism

gc_train_1 <- gc_tourism %>% slice(1:(n() - 4))
gc_train_2 <- gc_tourism %>% slice(1:(n() - 6))
gc_train_3 <- gc_tourism %>% slice(1:(n() - 8))


forecast_1_year <- function(train_df) {
  return(train_df %>%
    model(Snaive = SNAIVE(Total_Trips))%>%
    forecast(h = "1 year"))
}

gc_fc_1 <- forecast_1_year(gc_train_1)
gc_fc_2 <- forecast_1_year(gc_train_2)
gc_fc_3 <- forecast_1_year(gc_train_3)

gc_fc_1 %>% accuracy(gc_tourism)
gc_fc_2 %>% accuracy(gc_tourism)
gc_fc_3 %>% accuracy(gc_tourism)

