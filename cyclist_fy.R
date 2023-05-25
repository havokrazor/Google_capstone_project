#loading the data

read.csv("202101-divvy-tripdata.csv") -> cyclist1
read.csv("202102-divvy-tripdata.csv") -> cyclist2
read.csv("202103-divvy-tripdata.csv") -> cyclist3
read.csv("202104-divvy-tripdata.csv") -> cyclist4
read.csv("202105-divvy-tripdata.csv") -> cyclist5
read.csv("202106-divvy-tripdata.csv") -> cyclist6
read.csv("202107-divvy-tripdata.csv") -> cyclist7
read.csv("202108-divvy-tripdata.csv") -> cyclist8
read.csv("202109-divvy-tripdata.csv") -> cyclist9
read.csv("202110-divvy-tripdata.csv") -> cyclist10
read.csv("202111-divvy-tripdata.csv") -> cyclist11
read.csv("202112-divvy-tripdata.csv") -> cyclist12

#loadin the library

install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("lubridate")
library(lubridate)
install.packages("janitor")
library(janitor)

#combining all the dataset into one 

rbind(cyclist10,cyclist11,cyclist12,cyclist2,cyclist3,cyclist4,cyclist5,cyclist6,cyclist7,cyclist8,cyclist9,cyclist1) -> cyclist_fy

View(cyclist_fy)

#removing some col that's not needed for analysis

cyclist_fy %>% select(-c(start_lat,start_lng,end_lat,end_lng,
                         start_station_id, end_station_id)) -> cyclist_fy

cyclist_fy[cyclist_fy==""] <-NA
na.omit(cyclist_fy) -> cyclist_fy

nrow(cyclist_FY)
dim(cyclist_FY)
str(cyclist_FY)


#creating addational columns 

as.Date(cyclist_fy$started_at) -> cyclist_fy$date
format(as.Date(cyclist_fy$date), "%m") -> cyclist_fy$month
format(as.Date(cyclist_fy$date), "%d") -> cyclist_fy$day
format(as.Date(cyclist_fy$date), "%Y" ) -> cyclist_fy$year

#day_of_week
format(as.Date(cyclist_fy$date), "%A") -> cyclist_fy$day_of_week

#time
format(as.Date(cyclist_fy$started_at,format = "%H:%M") -> cyclist_fy$time

cyclist_fy$time <- format(cyclist_fy$started_at, format= "%H:%M")

cyclist_fy$time <- as.POSIXct(cyclist_fy$time, format= "%H:%M")

#cal ride_length 
cyclist_fy$ride_length <- (as.double(difftime(cyclist_fy$ended_at, cyclist_fy$started_at))) / 60

cyclist_fy$ride_length <- as.numeric(as.character(cyclist_fy$ride_length))

cyclist_fy<- cyclist_fy[!(cyclist_fy$start_station_name == "HQ QR" | cyclist_fy$ride_length<0),]

#summarizing the ride_lenth to check min , max , mean
summary(cyclist_fy$ride_length)

aggregate(cyclist_fy$ride_length ~ cyclist_fy$member_casual, FUN =  mean)
aggregate(cyclist_fy$ride_length ~ cyclist_fy$member_casual, FUN =  median)
aggregate(cyclist_fy$ride_length ~ cyclist_fy$member_casual, FUN =  max)
aggregate(cyclist_fy$ride_length ~ cyclist_fy$member_casual, FUN =  min)


#weekday
cyclist_fy %>% group_by(member_casual , day_of_week) %>% summarise(number_of_riders = n())

#data visualization
#rides per day 
cyclist_fy %>%                            
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = member_casual)) + geom_col(position = "dodge") + 
  labs(x='Day of Week', y='Total Number of Rides', title='Rides per Day of Week', fill = 'Type of Membership') + 
  scale_y_continuous(breaks = c(250000, 400000, 550000), labels = c("250K", "400K", "550K"))

##rider by month 

cyclist_fy %>% 
  group_by(member_casual, month) %>%  
  summarise(total_rides = n(),`average_duration_(mins)` = mean(ride_length)) %>% 
  arrange(member_casual)%>%
  ggplot(aes(x=month, y=total_rides, fill = member_casual)) + geom_col(position = "dodge") + 
  labs(x= "Month", y= "Total Number of Rides", title = "Rides per Month", fill = "Type of Membership") + 
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000), labels = c("100K", "200K", "300K", "400K")) 

##types of rides 

cyclist_fy %>%  
  ggplot(aes(x = rideable_type, fill = member_casual)) + geom_bar(position = "dodge") + 
  labs(x=  'Type of Bike', y='Number of Rentals', title='Which bike used the most', fill = 'Type of Membership') +
  scale_y_continuous(breaks = c(500000, 1000000, 1500000), labels = c("500K", "1M", "1.5M"))

##Avg time spent riding by each membership type per week

cyclist_fy %>%       
  group_by(member_casual, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") + labs(x='Days of the week', y='Duration(Hours)', title='Average ride time per week', fill='Type of Membership')

  
 

