install.packages("tidyverse")
install.packages("contrib.url")
install.packages("lubridate")
install.packages("janitor")
install.packages("ggplot2")
install.packages("dplyr")
library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)
library(dplyr)


#Import data
setwd("C:/Users/MashaMeilikhov/Documents/GOOGLE_CERTIFICAT/Case_Study_Bike/Bike_data_oct2021-2022/CSV_Original")
data_2021_10 <- read.csv("202110-divvy-tripdata.csv")
data_2021_11 <- read.csv("202111-divvy-tripdata.csv")
data_2021_12 <- read.csv("202112-divvy-tripdata.csv")
data_2022_01 <- read.csv("202201-divvy-tripdata.csv")
data_2022_02 <- read.csv("202202-divvy-tripdata.csv")
data_2022_03 <- read.csv("202203-divvy-tripdata.csv")
data_2022_04 <- read.csv("202204-divvy-tripdata.csv")
data_2022_05 <- read.csv("202205-divvy-tripdata.csv")
data_2022_06 <- read.csv("202206-divvy-tripdata.csv")
data_2022_07 <- read.csv("202207-divvy-tripdata.csv")
data_2022_08 <- read.csv("202208-divvy-tripdata.csv")
data_2022_09 <- read.csv("202209-divvy-publictripdata.csv")


#Check column names to ensure we can join all the data
colnames(data_2021_10)
colnames(data_2021_11)
colnames(data_2021_12)
colnames(data_2022_01)
colnames(data_2022_02)
colnames(data_2022_03)
colnames(data_2022_04)
colnames(data_2022_05)
colnames(data_2022_06)
colnames(data_2022_07)
colnames(data_2022_08)
colnames(data_2022_09)

#combining all table into 1 data frame
all_trips_2021 <- rbind(data_2021_10,data_2021_11,data_2021_12,data_2022_01,data_2022_02,data_2022_03,data_2022_04,data_2022_05,data_2022_06,data_2022_07,data_2022_08,data_2022_09)

#removing unnecessary columns
all_bike_trip_2021_good <- select(all_trips_2021, -9:-12)

# Add a "ride_length" calculation to all_trips (in seconds)
all_bike_trip_2021_good$started_at <- lubridate:: ymd_hms(all_bike_trip_2021_good$started_at)
all_bike_trip_2021_good$ended_at <- lubridate:: ymd_hms(all_bike_trip_2021_good$ended_at)
all_bike_trip_2021_good$ride_length <- difftime(all_bike_trip_2021_good$ended_at,all_bike_trip_2021_good$started_at)

#Convert "ride_length" to numeric
all_bike_trip_2021_good$ride_length <- as.numeric(as.character(all_bike_trip_2021_good$ride_length_K))
is.numeric(all_bike_trip_2021_good$ride_length)
colnames(all_bike_trip_2021_good)
str(all_bike_trip_2021_good)
summary(all_bike_trip_2021_good)

# Add columns that list the date, month, day, and year of each ride
all_bike_trip_2021_good$date <- as.Date(all_bike_trip_2021_good$started_at)
all_bike_trip_2021_good$month <- format(as.Date(all_bike_trip_2021_good$date), "%m")
all_bike_trip_2021_good$day <- format(as.Date(all_bike_trip_2021_good$date), "%d")
all_bike_trip_2021_good$year <- format(as.Date(all_bike_trip_2021_good$date), "%Y")
all_bike_trip_2021_good$day_of_week <- format(as.Date(all_bike_trip_2021_good$date), "%A")

#removing "bad data" - ride_length less then 60 sec
all_bike_trip_2021_good_v2 <- all_bike_trip_2021_good[!(all_bike_trip_2021_good$ride_length<60),]
summary(all_bike_trip_2021_good_v2)

# Descriptive analysis on ride_length (all figures in seconds)
summary(all_bike_trip_2021_good_v2$ride_length)

#Inspect data to find cleaning opportunities
colnames(all_bike_trip_2021_good_v2)
nrow(all_bike_trip_2021_good_v2)
dim(all_bike_trip_2021_good_v2)
head(all_bike_trip_2021_good_v2)
tail(all_bike_trip_2021_good_v2)
str(all_bike_trip_2021_good_v2)
summary(all_bike_trip_2021_good_v2)
table(all_bike_trip_2021_good_v2$start_station_name)
table(all_bike_trip_2021_good_v2$rideable_type)

#removing "bad data" - rideable_type "docked_bike"
all_bike_trip_2021_good_v3 <- all_bike_trip_2021_good[!(all_bike_trip_2021_good$rideable_type=="docked_bike"),]
summary(all_bike_trip_2021_good_v3)
summary(all_bike_trip_2021_good_v3$rideable_type)
summary(all_bike_trip_2021_good_v3$ride_length)


# Compare members and casual users (in general)
aggregate(all_bike_trip_2021_good_v3$ride_length ~ all_bike_trip_2021_good_v3$member_casual, FUN = mean)
#aggregate(all_bike_trip_2021_good_v3$ride_length ~ all_bike_trip_2021_good_v3$member_casual, FUN = median)
#aggregate(all_bike_trip_2021_good_v3$ride_length ~ all_bike_trip_2021_good_v3$member_casual, FUN = max)
#aggregate(all_bike_trip_2021_good_v3$ride_length ~ all_bike_trip_2021_good_v3$member_casual, FUN = min)

# Compare members and casual users for each day:
aggregate(all_bike_trip_2021_good_v3$ride_length ~ all_bike_trip_2021_good_v3$member_casual + all_bike_trip_2021_good_v3$day_of_week, FUN = mean)
#Since the days are not organized by the week order->re-organize
all_bike_trip_2021_good_v3$day_of_week <- ordered(all_bike_trip_2021_good_v3$day_of_week, levels=c("Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
#Compare again
aggregate(all_bike_trip_2021_good_v3$ride_length ~ all_bike_trip_2021_good_v3$member_casual + all_bike_trip_2021_good_v3$day_of_week, FUN = mean)
all_bike_trip_2021_good_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

#Show average duration
all_bike_trip_2021_good_v3 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) + geom_col(position = "dodge")





