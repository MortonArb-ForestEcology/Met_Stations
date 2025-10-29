library(ggplot2)
library(tidyverse)
library(readr)

# Load data
N115 <- read_csv("~/Google Drive/My Drive/East Woods/Rollinson_Monitoring/Data/Met Stations/Single_Plots/Data_processed/N115_2023_up_to_2023-11-10.csv")
head(N115)

# Filtering for PAR, parsing for time stamp
par.dat <- N115 %>%
  mutate(
    Timestamp = ymd_hms(Timestamp), 
    Date = as.Date(Timestamp),       
    Month = month(Timestamp),
    DOY = yday(Timestamp),
    Hour = hour(Timestamp)
  ) %>%
  filter(!is.na(PAR))

# Extracting midday PAR (11:00-14:00) as mean and max par for this period
midday.par <- par.dat %>%
  filter(Hour >= 11 & Hour <= 14) %>%
  group_by(Date, DOY, Month) %>%
  summarise(
    max.par = max(PAR, na.rm = TRUE),
    mean.par = mean(PAR, na.rm = TRUE),
    n_obs = n(),
    .groups = 'drop'
  ) 


# Visualization mean par
ggplot(midday.par, aes(x = DOY, y = mean.par)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = TRUE) +
  labs(title = "Midday PAR throughout 2023",
       subtitle = "Plot N115",
       x = "Day of Year",
       y = "Mean PAR",) +
  theme_minimal()

# Visualization max par
ggplot(midday.par, aes(x = DOY, y = max.par)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = TRUE) +
  labs(title = "Midday PAR throughout 2023",
       subtitle = "Plot N115",
       x = "Day of Year",
       y = "Max PAR",) +
  theme_minimal()
###########################
# Filtering for Precipitation, parsing for time stamp
prcp.dat <- N115 %>%
  mutate(
    Timestamp = ymd_hms(Timestamp), 
    Date = as.Date(Timestamp),       
    Month = month(Timestamp),
    DOY = yday(Timestamp),
    Hour = hour(Timestamp)
  ) %>%
  filter(!is.na(mm.Precipitation))

# Extracting  mean and max precipitation for this period
prcp.dat <- prcp.dat %>%
  group_by(Date, DOY, Month) %>%
  summarise(
    max.prcp = max(mm.Precipitation, na.rm = TRUE),
    mean.prcp = mean(mm.Precipitation, na.rm = TRUE),
    n_obs = n(),
    .groups = 'drop'
  ) 


# Visualization mean precip
ggplot(prcp.dat, aes(x = DOY, y = mean.prcp)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = TRUE) +
  labs(title = "Mean mm Preciptation throughout 2023",
       subtitle = "Plot N115",
       x = "Day of Year",
       y = "Mean mm precipitation",) +
  theme_minimal()

# Visualization mean precip
ggplot(prcp.dat, aes(x = DOY, y = max.prcp)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = TRUE) +
  labs(title = "Max mm Preciptation throughout 2023",
       subtitle = "Plot N115",
       x = "Day of Year",
       y = "Max mm precipitation",) +
  theme_minimal()
################
#lightening activity, this is a dumb way to look at this
# Filtering for lighting activity, parsing for time stamp
LA.dat <- N115 %>%
  mutate(
    Timestamp = ymd_hms(Timestamp), 
    Date = as.Date(Timestamp),       
    Month = month(Timestamp),
    DOY = yday(Timestamp),
    Hour = hour(Timestamp)
  ) %>%
  filter(!is.na(Lightning.Activity))

# Extracting  mean and max precipitation for this period
LA.dat <- LA.dat %>%
  group_by(Date, DOY, Month) %>%
  summarise(
    max.LA = max(Lightning.Activity, na.rm = TRUE),
    mean.LA = mean(Lightning.Activity, na.rm = TRUE),
    n_obs = n(),
    .groups = 'drop'
  ) 


# Visualization mean precip
ggplot(LA.dat, aes(x = DOY, y = mean.LA)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = TRUE) +
  labs(title = "Mean Lightneing Activity throughout 2023",
       subtitle = "Plot N115",
       x = "Day of Year",
       y = "Mean Lightening Activity",) +
  theme_minimal()

# Visualization mean precip
ggplot(LA.dat, aes(x = DOY, y = max.LA)) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = TRUE) +
  labs(title = "Max Lightneing Activity throughout 2023",
       subtitle = "Plot N115",
       x = "Day of Year",
       y = "Max mm Lighting Activity",) +
  theme_minimal()
