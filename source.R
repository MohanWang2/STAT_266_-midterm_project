library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)

data <- read.csv("PRSA_Data_Tiantan_20130301-20170228.csv")

daydata <- data %>%
  mutate(datetime = make_datetime(year, month, day, hour)) %>%
  select(No, datetime, PM2.5, PM10)

daily_pm25 <- daydata %>%
  mutate(date = as_date(datetime)) %>%       
  group_by(date) %>%                         
  summarise(pm25_daily = mean(PM2.5, na.rm = TRUE)) %>%
  ungroup()

daily_pm10 <- daydata %>%
  mutate(date = as_date(datetime)) %>%      
  group_by(date) %>%                          
  summarise(pm10_daily = mean(PM10, na.rm = TRUE)) %>%
  ungroup()


head(daily_pm25)


start_2013 <- ymd_hms("2013-03-01 00:00:00")
end_2013   <- ymd_hms("2014-03-01 00:00:00")

start_2014 <- ymd_hms("2014-03-01 00:00:00")
end_2014   <- ymd_hms("2015-03-01 00:00:00")

start_2015 <- ymd_hms("2015-03-01 00:00:00")
end_2015   <- ymd_hms("2016-03-01 00:00:00")

start_2016 <- ymd_hms("2016-03-01 00:00:00")
end_2016   <- ymd_hms("2017-03-01 00:00:00")


daydata_2013 <- daydata %>%
  filter(datetime >= start_2013, datetime <  end_2013)

daydata_2014 <- daydata %>%
  filter(datetime >= start_2014, datetime <  end_2014)

daydata_2015 <- daydata %>%
  filter(datetime >= start_2015, datetime <  end_2015)

daydata_2016 <- daydata %>%
  filter(datetime >= start_2016, datetime <  end_2016)

# 对 daily_pm25（同理，用 date 列）
daily_pm25_2013 <- daily_pm25 %>%
  filter(date >= as_date(start_2013), date < as_date(end_2013))

daily_pm25_2014 <- daily_pm25 %>%
  filter(date >= as_date(start_2014), date < as_date(end_2014))

daily_pm25_2015 <- daily_pm25 %>%
  filter(date >= as_date(start_2015), date < as_date(end_2015))

daily_pm25_2016 <- daily_pm25 %>%
  filter(date >= as_date(start_2016), date < as_date(end_2016))

daily_pm10_2013 <- daily_pm10 %>%
  filter(date >= as_date(start_2013), date < as_date(end_2013))

daily_pm10_2014 <- daily_pm10 %>%
  filter(date >= as_date(start_2014), date < as_date(end_2014))

daily_pm10_2015 <- daily_pm10 %>%
  filter(date >= as_date(start_2015), date < as_date(end_2015))

daily_pm10_2016 <- daily_pm10 %>%
  filter(date >= as_date(start_2016), date < as_date(end_2016))

all_pm25 <- bind_rows(
  daily_pm25_2013 %>% mutate(year = 2013),
  daily_pm25_2014 %>% mutate(year = 2014),
  daily_pm25_2015 %>% mutate(year = 2015),
  daily_pm25_2016 %>% mutate(year = 2016)
)


all_pm10 <- bind_rows(
  daily_pm10_2013 %>% mutate(year=2013),
  daily_pm10_2014 %>% mutate(year=2014),
  daily_pm10_2015 %>% mutate(year=2015),
  daily_pm10_2016 %>% mutate(year=2016)
) %>%
  mutate(
    day = as.integer(date - as.Date(paste0(year, "-03-01")))
  ) %>%
  filter(day >= 0, day <= 365)



ggplot(all_pm25, aes(x = date, y = pm25_daily, color = factor(year))) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Set1", name = "Year") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y-%m") +
  labs(
    title = "2013–2016 day average PM2.5",
    x     = "day",
    y     = "PM2.5 day average (μg/m³)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


ggplot(all_pm25, aes(x = day, y = pm25_daily, color = factor(year))) +
  geom_line() +
  labs(
    x = "day",
    y = "PM2.5 day average (μg/m³)",
    color = "year",
    title = "2013–2016 PM2.5 day average comparison"
  ) +
  theme_minimal()

counts <- all_pm25 %>%
  group_by(year) %>%
  summarise(
    `35~75`  = sum(pm25_daily > 35  & pm25_daily < 75, na.rm=TRUE),
    `>75`    = sum(pm25_daily > 75, na.rm=TRUE)
  ) %>%
  pivot_longer(-year, names_to="range", values_to="days")

ggplot(counts, aes(x=factor(year), y=days, fill=range)) +
  geom_col(position="dodge") +
  geom_text(aes(label=days),
            position=position_dodge(width=0.8),
            vjust=-0.5, size=3) +
  labs(
    x     = "year",
    y     = "day",
    fill  = "PM2.5 (μg/m³)",
    title = "2013–2016 PM2.5 day average 35–75 / >75 "
  ) +
  theme_minimal()

stats <- all_pm25 %>%
  group_by(year) %>%
  summarise(median_pm25 = median(pm25_daily, na.rm = TRUE))


ggplot(all_pm25, aes(x = factor(year), y = pm25_daily)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 0.5) +
  geom_text(
    data = stats,
    aes(x = factor(year), y = median_pm25 + 5, label = round(median_pm25, 1)),
    size = 3
  ) +
  labs(
    x     = "year",
    y     = "PM2.5 day average (μg/m³)",
    title = "2013–2016 PM2.5 day average boxplot"
  ) +
  theme_minimal()


ggplot(all_pm10, aes(x=day, y=pm10_daily, color=factor(year))) +
  geom_line() +
  labs(
    x = "day",
    y = "PM10 day average (μg/m³)",
    color = "year",
    title = "2013–2016 PM10 day comparison（3/1→next year 3/1）"
  ) +
  theme_minimal()

ggplot(all_pm10, aes(x = factor(year), y = pm10_daily)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.2, size = 0.5) +
  geom_text(
    data = stats,
    aes(x = factor(year), y = median_pm25 + 5, label = round(median_pm25, 1)),
    size = 3
  ) +
  labs(
    x     = "year",
    y     = "PM10 day average (μg/m³)",
    title = "2013–2016 PM10 day average boxplot"
  ) +
  theme_minimal()

counts_pm10 <- all_pm10 %>%
  group_by(year) %>%
  summarise(
    moderate = sum(pm10_daily > 50  & pm10_daily <= 150, na.rm = TRUE),
    severe   = sum(pm10_daily > 150, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols      = c(moderate, severe),
    names_to  = "category",
    values_to = "days"
  )

ggplot(counts_pm10, aes(x = factor(year), y = days, fill = category)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = days),
            position = position_dodge(width = 0.7),
            vjust = -0.3,
            size = 3) +
  scale_fill_manual(
    values = c("moderate" = "#66c2a5", "severe" = "#fc8d62"),
    labels = c("50 < PM10 ≤ 150", "PM10 > 150")
  ) +
  labs(
    x     = "year",
    y     = "day count",
    fill  = "PM10",
    title = "2013–2016 PM10 over standar 50<PM10≤150 vs PM10>150"
  ) +
  theme_minimal() +
  theme(
    axis.text      = element_text(size = 10),
    axis.title     = element_text(size = 12),
    legend.title   = element_text(size = 11),
    legend.text    = element_text(size = 10)
  )



# ------------wind---------------------

data_ws <- data %>%
  filter(!is.na(WSPM) & WSPM > 3)

daydata_ws <- data_ws %>%
  mutate(datetime = make_datetime(year, month, day, hour)) %>%
  select(No, datetime, PM2.5, PM10, wd, WSPM)

df_long <- daydata_ws %>%
  select(datetime, WSPM, `PM2.5`, PM10) %>%
  rename(
    WindSpeed = WSPM,
    PM25      = `PM2.5`,
    PM10      = PM10
  ) %>%
  pivot_longer(
    cols      = c(WindSpeed, PM25, PM10),
    names_to  = "Variable",
    values_to = "Value"
  )

ggplot(df_long, aes(x = datetime, y = Value, color = Variable)) +
  geom_line() +
  labs(
    x     = "time",
    y     = "μg/m³",
    color = "variable",
    title = "wind speed、PM2.5 and PM10 follow by time（ WSPM > 3）"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

corrs <- daydata_ws %>%
  summarise(
    cor_PM25 = cor(WSPM, `PM2.5`, use = "complete.obs"),
    cor_PM10 = cor(WSPM, PM10,   use = "complete.obs")
  )
print(corrs)
#> # A tibble: 1 × 2
#>    cor_PM25 cor_PM10
#>      <dbl>    <dbl>
#> 1    -0.45    -0.38


df_long <- daydata_ws %>%
  select(WSPM, `PM2.5`, PM10) %>%
  rename(PM25 = `PM2.5`) %>%
  pivot_longer(c(PM25, PM10), names_to = "Pollutant", values_to = "Conc")

ggplot(daydata_ws, aes(x = WSPM, y = `PM2.5`)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x     = "wind speed (m/s)",
    y     = "PM2.5 day average (μg/m³)",
    title = "wind speed vs PM2.5"
  ) +
  theme_minimal()

# PM10 vs wd 
ggplot(daydata_ws, aes(x = WSPM, y = PM10)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    x     = "wind speed (m/s)",
    y     = "PM10 day average (μg/m³)",
    title = "wind speed vs PM10"
  ) +
  theme_minimal()

daily_ws <- daydata_ws %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(ws_daily = mean(WSPM, na.rm = TRUE)) %>%
  ungroup()

# Plot time series of daily wind speed
ggplot(daily_ws, aes(x = date, y = ws_daily)) +
  geom_line() +
  labs(
    x     = "Date",
    y     = "Daily Average Wind Speed (m/s)",
    title = "Daily Average Wind Speed Over Time"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

daily_pm10 <- daydata_ws %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(pm10_daily = mean(PM10, na.rm = TRUE)) %>%
  ungroup()



