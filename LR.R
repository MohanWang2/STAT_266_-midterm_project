library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)


df <- read.csv("PRSA_Data_Tiantan_20130301-20170228.csv")

daily_pm25 <- df %>%
  mutate(datetime = make_datetime(year, month, day, hour),
         date     = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(pm25_daily = mean(PM2.5, na.rm = TRUE)) %>%
  ungroup()

assign_year_group <- function(d) {
  y <- year(d)
  if_else(month(d) >= 3, y, y - 1)
}

daily_pm25 <- daily_pm25 %>%
  mutate(year_group = assign_year_group(date))

train_data <- daily_pm25 %>%
  filter(year_group %in% c(2013, 2014))

model <- lm(pm25_daily ~ as.numeric(date), data = train_data)

summary(model)

test_data <- daily_pm25 %>%
  filter(year_group == 2015)

# 2. generate predictions using the previously fitted model
test_data <- test_data %>%
  mutate(pred_pm25 = predict(model, newdata = .))

# 3. compute RMSE to quantify prediction error
rmse_2015 <- sqrt(mean((test_data$pred_pm25 - test_data$pm25_daily)^2, na.rm = TRUE))
print(rmse_2015)

# 4. plot actual vs. predicted for 2015
ggplot(test_data, aes(x = date)) +
 geom_line(aes(y = pm25_daily), color = "steelblue", alpha = 0.8) +
  geom_line(aes(y = pred_pm25), color = "firebrick") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(
    title   = "Actual vs. Predicted Daily PM2.5 in 2015",
    x       = "Date",
    y       = expression("PM2.5 ("*μ*g/m^3*")"),
    caption = paste0("RMSE = ", round(rmse_2015, 2))
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# --------------------------------------------------------------------------

daily_2013 <- daily_pm25 %>%
  filter(lubridate::year(date) == 2013) %>%
  select(date, pm25_2013 = pm25_daily)

daily_2014 <- daily_pm25 %>%
  filter(lubridate::year(date) == 2014) %>%
  select(date, pm25_2014 = pm25_daily)

# 2. Join on the same month–day (so Jan 1 2013 pairs with Jan 1 2014, etc.)
df_lr <- daily_2013 %>%
  mutate(mday = format(date, "%m-%d")) %>%
  inner_join(
    daily_2014 %>% mutate(mday = format(date, "%m-%d")),
    by = "mday"
  ) %>%
  # drop extra cols, keep paired values
  select(pm25_2013, pm25_2014)

# 3. Fit linear regression: 2014 ~ 2013
model2 <- lm(pm25_2013 ~ pm25_2014, data = df_lr)

summary(model2)


# 2. prepare 2015 actual daily pm2.5 with same key
daily_2015 <- daily_pm25 %>%
  filter(year(date) == 2015) %>%
  mutate(mday = format(date, "%m-%d")) %>%
  select(date, mday, pm25_actual = pm25_daily)

# 3. join and predict 2015 using model2 (which was fit: pm25_2014 ~ pm25_2013)
pred_df <- daily_2015 %>%
  left_join(daily_2014, by = "mday") %>%
  mutate(
    pm25_pred = predict(model2, newdata = data.frame(pm25_2013 = pm25_2013))
  )

# 4. compute RMSE
rmse_2015 <- sqrt(mean((pred_df$pm25_pred - pred_df$pm25_actual)^2, na.rm = TRUE))
print(rmse_2015)

# 5. plot actual vs. predicted for 2015
ggplot(pred_df, aes(x = date)) +
  geom_line(aes(y = pm25_actual), color = "steelblue", alpha = 0.8) +
  geom_line(aes(y = pm25_pred),   color = "firebrick", linetype = "dashed") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m") +
  labs(
    title   = "Actual vs. Model2-Predicted Daily PM2.5 in 2015",
    x       = "Date",
    y       = expression("PM2.5 ("*μ*g/m^3*")"),
    caption = paste0("RMSE = ", round(rmse_2015, 2))
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


