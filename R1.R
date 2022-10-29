setwd("C:/Users/T O S H I B A/Desktop/Google Case Study/Archived/trips 1")
getwd()
install.packages("plyr")
install.packages("dplyr")

library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data

#Importing files
q2_2019<-read.csv("Divvy_Trips_2019_Q2.CSV")
q3_2019<-read.csv("Divvy_Trips_2019_Q3.CSV")
q4_2019<-read.csv("Divvy_Trips_2019_Q4.CSV")
q1_2020<-read.csv("Divvy_Trips_2020_Q1.CSV")

colnames(q1_2020)
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)


conflict_prefer("rename", "dplyr")
(q4_2019 <- rename(q4_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q3_2019 <- rename(q3_2019
                   ,ride_id = trip_id
                   ,rideable_type = bikeid 
                   ,started_at = start_time  
                   ,ended_at = end_time  
                   ,start_station_name = from_station_name 
                   ,start_station_id = from_station_id 
                   ,end_station_name = to_station_name 
                   ,end_station_id = to_station_id 
                   ,member_casual = usertype))

(q2_2019 <- rename(q2_2019
                   ,  "01 - Rental Details Rental ID"=ride_id
                   ,  "01 - Rental Details Bike ID"=rideable_type 
                   ,  "01 - Rental Details Local Start Time"  started_at
                   ,  "01 - Rental Details Local End Time"  ended_at
                   ,  "03 - Rental Start Station Name" start_station_name
                   ,  "03 - Rental Start Station ID"start_station_id
                   ,  "02 - Rental End Station Name" end_station_name
                   ,  "02 - Rental End Station ID"end_station_id
                   ,  "User Type"=member_casual))

(q2_2019 <- rename(q2_2019
                   ,ride_id = "X01...Rental.Details.Rental.ID"    
                   ,rideable_type = "X01...Rental.Details.Bike.ID"  
                   ,started_at = "X01...Rental.Details.Local.Start.Time"    
                   ,ended_at = "X01...Rental.Details.Local.End.Time"   
                   ,start_station_name = "X03...Rental.Start.Station.Name"  
                   ,start_station_id = "X03...Rental.Start.Station.ID"  
                   ,end_station_name = "X02...Rental.End.Station.Name"      
                   ,end_station_id = "X02...Rental.End.Station.ID"   
                   ,member_casual = "User.Type" ))
str(q1_2020)
str(q4_2019)
str(q3_2019)
str(q2_2019)
conflict_prefer("mutate", "dplyr")

q4_2019 <-  mutate(q4_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q3_2019 <-  mutate(q3_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
q2_2019 <-  mutate(q2_2019, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 

#bind all
All_trips<- bind_rows(q2_2019,q3_2019,q4_2019,q1_2020)

# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020
All_trips <- All_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender,
            "01 - Rental Details Duration In Seconds Uncapped",
            "05 - Member Details Member Birthday Year",
            "Member Gender", 
            "tripduration"))

all_trips_v2<- subset(all_trips_v2,select = -c(Customer,subscriber))
  select(-c( "X05...Member.Details.Member.Birthday.Year"        
, "tripduration"                                     
, "gender"                                           
, "birthyear"                                        
, "start_lat"                                        
, "start_lng"                                        
, "end_lat"                                          
, "end_lng"))

# Inspect the new table that has been created
colnames(All_trips)  #List of column names
nrow(All_trips)  #How many rows are in data frame?
dim(All_trips)  #Dimensions of the data frame?
head(All_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(All_trips)  #See list of columns and data types (numeric, character, etc)
summary(All_trips)  #Statistical summary of data. Mainly for numerics

table(All_trips$member_casual)


All_trips <-  All_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))


All_trips$Date<-as.Date(All_trips$started_at)
All_trips$Year<-format(as.Date(All_trips$Date),"%y")
All_trips$Month<- format(as.Date(All_trips$Date), "%m")
All_trips$Day<-format(as.Date(All_trips$Date),"%d")
All_trips$day_of_week<- format(as.Date(All_trips$Date),"%A")

head(All_trips)

All_trips$ride_length<- difftime(All_trips$ended_at,All_trips$started_at)
str(All_trips)


# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(All_trips$ride_length)                  
All_trips$ride_length <- as.numeric(as.character(All_trips$ride_length))
is.numeric(All_trips$ride_length)

all_trips_v2<-All_trips[!(All_trips$start_station_name=="HQ QR"| All_trips$ride_length<0),]


summary(all_trips_v2)

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)



# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week,levels=c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

library(dplyr)
conflict_prefer("arrange","dplyr")
conflict_prefer("summarise","dplyr")


# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts


# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")


# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")


write.csv(all_trips_v2,"C:/Users/T O S H I B A/Desktop/Google Case Study/R//expFromR.csv", row.names = FALSE)
