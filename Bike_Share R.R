#------- PrePare
install.packages("tidyverse")
install.packages("dplyr")
install.packages("readr")
install.packages("gridExtra")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("geosphere")
install.packages("ggmap")

library("tidyverse")
library("dplyr")
library("readr")
library("gridExtra")
library("ggplot2")
library("lubridate")
library("geosphere")
library("ggmap")

# First load all the data in R
data_202104 <- read_csv("original data set/202104-divvy-tripdata.csv")
data_202105 <- read_csv("original data set/202105-divvy-tripdata.csv")
data_202106 <- read_csv("original data set/202106-divvy-tripdata.csv")
data_202107 <- read_csv("original data set/202107-divvy-tripdata.csv")
data_202108 <- read_csv("original data set/202108-divvy-tripdata.csv")
data_202109 <- read_csv("original data set/202109-divvy-tripdata.csv")
data_202110 <- read_csv("original data set/202110-divvy-tripdata.csv")
data_202111 <- read_csv("original data set/202111-divvy-tripdata.csv")
data_202112 <- read_csv("original data set/202112-divvy-tripdata.csv")
data_202201 <- read_csv("original data set/202201-divvy-tripdata.csv")
data_202202 <- read_csv("original data set/202202-divvy-tripdata.csv")
data_202203 <- read_csv("original data set/202203-divvy-tripdata.csv")

# Then merge all data sets into one data frame
Alltrip_data <- bind_rows(data_202104, data_202105, data_202106, data_202107, data_202108, data_202109, data_202110, data_202111, data_202112, data_202201, data_202202, data_202203)

#------- Process
# Take a look of the data
glimpse(Alltrip_data)
summary(Alltrip_data)

# output summary as .txt
capture.output(summary(Alltrip_data), file = "Trip_summary.txt")

# Drop "NA" columns:
Cleaned_Alltrip_data <- drop_na(Alltrip_data)

# Add a ride_length
Cleaned_Alltrip_data$ride_length <- difftime(Cleaned_Alltrip_data$ended_at, Cleaned_Alltrip_data$started_at)

# Convert ride_length from Factor to Numeric
Cleaned_Alltrip_data$ride_length <- as.numeric(as.character(Cleaned_Alltrip_data$ride_length))
is.numeric(Cleaned_Alltrip_data$ride_length)

# ride distance traveled in km
Cleaned_Alltrip_data$ride_distance <- distGeo(matrix(c(Cleaned_Alltrip_data$start_lng, Cleaned_Alltrip_data$start_lat), ncol=2), matrix (c(Cleaned_Alltrip_data$end_lng, Cleaned_Alltrip_data$end_lat), ncol=2))
Cleaned_Alltrip_data$ride_distance <- Cleaned_Alltrip_data$ride_distance/1000 


#----- Analyze
summary(Cleaned_Alltrip_data)

# count total users
# Plotting differences between usage of member vs. casual
Usage_Differences <- Cleaned_Alltrip_data %>% 
  group_by(member_casual) %>%
  summarise(total_ride = length(ride_id),
            average_ride_length = mean(ride_length), 
            median_ride_length = median(ride_length), 
            average_ride_distance = mean(ride_distance),
            median_ride_distance = median(ride_distance))

total_users <- ggplot(Usage_Differences) + 
  geom_col(mapping=aes(x=member_casual,y=total_ride,fill=member_casual), show.legend = FALSE)+
  labs(title = "Count Rides",x="User Type",y="Count Ride In Total")

time_differences <- ggplot(Usage_Differences) + 
                    geom_col(mapping=aes(x=member_casual,y=average_ride_length,fill=member_casual), show.legend = FALSE)+
                    labs(title = "Avg_Travel time differences",x="User Type",y="Avg_Time In Sec")

distance_differences <- ggplot(Usage_Differences) + 
                        geom_col(mapping=aes(x=member_casual,y=average_ride_distance,fill=member_casual), show.legend = FALSE)+
                        labs(title = "Avg_Travel distance differences",x="User Type",y="Avg_Distance In Km")

grid.arrange(total_users, time_differences, distance_differences, ncol = 3) 

# get differences in numbers between member and casual by days of the week
Cleaned_Alltrip_data %>% 
  mutate(days_of_week = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, days_of_week) %>% 
  summarise(number_of_rides = n(),.groups = 'drop') %>% 
  arrange(member_casual, days_of_week)  %>% 
  ggplot(aes(x = days_of_week, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Total rides of Members and Casual riders Vs. Day of the week",x="Days of the week",y="Number of
       rides", fill="User type")

# Export cleaned data set as .csv. I'll be using tableau to visualize.
write.csv(Cleaned_Alltrip_data,"Bike_Share Capstone/Alltrip_data.csv", row.names = FALSE)

# Use tableau to viz






