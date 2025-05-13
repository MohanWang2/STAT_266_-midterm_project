library(lubridate)
library(tidyverse)
library(dplyr)

data <- read.csv("PRSA_Data_Tiantan_20130301-20170228.csv")

daydata <- data %>%
  mutate(datetime = make_datetime(year, month, day, hour)) %>%
  select(No, datetime, PM2.5, PM10)

daily_pm25 <- daydata %>%
  mutate(date = as_date(datetime)) %>%       # 从 datetime 中提取日期
  group_by(date) %>%                          # 按日期分组
  summarise(pm25_daily = mean(PM2.5, na.rm = TRUE)) %>%
  ungroup()

# 2. 查看结果
head(daily_pm25)

pm25_2013 <- daily_pm25 %>%
  filter(year(date) == 2013)

# Plot daily PM2.5 for 2013
ggplot(pm25_2013, aes(x = date, y = pm25_daily)) +
  geom_line(color = "steelblue") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%Y-%m"
  ) +
  labs(
    title = "Daily Average PM2.5 in Beijing for 2013",
    x     = "Date",
    y     = expression("PM2.5 ("*μ*g/m^3*")")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
