library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggfortify)

df <- read.csv("PRSA_Data_Tiantan_20130301-20170228.csv")

daily_pm10 <- df %>%
  mutate(datetime = make_datetime(year, month, day, hour),
         date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(pm10_daily = mean(PM10, na.rm = TRUE)) %>%
  ungroup()

assign_year_group <- function(d) {
  y <- year(d)
  if_else(month(d) >= 3, y, y - 1)
}

daily_pm10 <- daily_pm10 %>%
  mutate(year_group = assign_year_group(date))

daily_2013 <- daily_pm10 %>%
  filter(lubridate::year(date) == 2013) %>%
  select(date, pm10_2013 = pm10_daily)

daily_2014 <- daily_pm10 %>%
  filter(lubridate::year(date) == 2014) %>%
  select(date, pm10_2014 = pm10_daily)

df_lr <- daily_2013 %>%
  mutate(mday = format(date, "%m-%d")) %>%
  inner_join(
    daily_2014 %>% mutate(mday = format(date, "%m-%d")),
    by = "mday"
  ) %>%
  # drop extra cols, keep paired values
  select(pm10_2013, pm10_2014)

model1 <- lm(df_lr$pm10_2013[-1] ~ df_lr$pm10_2013[-nrow(df_lr)])

summary(model1) 

pm10_pred <- predict(model1)

#plot(df_lr$pm10_2013[-1], type = 'line')
#lines(pm10_pred, col = "red")

df <- na.omit(df)

data <- df[, 6:17]

model2 <- lm(PM10 ~ .,data = data)

summary(model2)

model3 <- lm(PM10 ~ TEMP+RAIN, data = data)

summary(model3)


# --------------------------------------------------

plot(
  df_lr$pm10_2013[-1],
  type = "l",            # line plot
  lwd  = 2,
  col  = "black",
  xlab = "Day Index (2013-03-02 … 2014-03-01)",
  ylab = "PM10 (µg/m³)",
  main = "PM10 2013 — Observed vs. Predicted"
)
lines(
  pm10_pred,
  lwd = 2,
  col = "red",
  lty = 2               # dashed line for predictions
)
legend(
  "topright",
  legend = c("Observed", "Predicted"),
  col    = c("black", "red"),
  lwd    = 2,
  lty    = c(1, 2),
  bty    = "n"
)

# Model 1  (AR(1)  on 2013 PM10)
autoplot(model1) + ggtitle("Model 1 Diagnostics — on 2013 PM10")

# Model 2  (all predictors)
autoplot(model2) + ggtitle("Model 2 Diagnostics — Full Variable Set")

# Model 3  (TEMP + RAIN)
autoplot(model3) + ggtitle("Model 3 Diagnostics — TEMP + RAIN")

#----------------------------------------------------------


