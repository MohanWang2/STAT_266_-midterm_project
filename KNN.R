library(dplyr)
library(lubridate)
library(readr)
library(caret)
library(ggplot2)

# 1. import CSV
df <- read.csv("PRSA_Data_Tiantan_20130301-20170228.csv")

# 2. compute daily PM2.5 average and assign March-to-March year groups
daily_pm25 <- df %>%
  mutate(
    datetime = make_datetime(year, month, day, hour),
    date     = as_date(datetime),
    pm25_daily = PM2.5
  ) %>%
  group_by(date) %>%
  summarise(pm25_daily = mean(pm25_daily, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    year_group = if_else(month(date) >= 3, year(date), year(date) - 1)
  )

# 3. split into train (2013–2014) and test (2015) sets
train_data <- daily_pm25 %>% filter(year_group %in% c(2013, 2014))
test_data  <- daily_pm25 %>% filter(year_group == 2015)

# 4. prepare feature: convert date to numeric (days since 1970-01-01)
train_x <- data.frame(date_num = as.numeric(train_data$date))
train_y <- train_data$pm25_daily
test_x  <- data.frame(date_num = as.numeric(test_data$date))

# 5. train a KNN regression model (with k = 5, 5-fold CV)
set.seed(42)
ctrl   <- trainControl(method = "cv", number = 5)
knn_fit <- train(
  x         = train_x,
  y         = train_y,
  method    = "knn",
  tuneGrid  = data.frame(k = 5),
  trControl = ctrl
)

# 6. predict on 2015 data
test_data <- test_data %>%
  mutate(knn_pred = predict(knn_fit, newdata = test_x))

# 7. compute RMSE
rmse_knn <- sqrt(mean((test_data$knn_pred - test_data$pm25_daily)^2, na.rm = TRUE))
print(rmse_knn)

# 8. plot actual vs. KNN-predicted
ggplot(test_data, aes(x = date)) +
  geom_line(aes(y = pm25_daily), color = "steelblue", alpha = 0.8) +
  geom_line(aes(y = knn_pred),   color = "firebrick", linetype = "dashed") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(
    title   = "Actual vs. KNN-Predicted Daily PM2.5 (2015)",
    x       = "Date",
    y       = expression("PM2.5 ("*μ*g/m^3*")"),
    caption = paste0("K = 5, RMSE = ", round(rmse_knn, 2))
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
