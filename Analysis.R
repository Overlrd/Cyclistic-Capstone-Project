library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(janitor)



##lets import our data

df1 = read.csv("Data/202101-divvy-tripdata.csv", header = TRUE)
df2 = read.csv("Data/202102-divvy-tripdata.csv", header = TRUE)
df3 = read.csv("Data/202103-divvy-tripdata.csv", header = TRUE)
df4 = read.csv("Data/202104-divvy-tripdata.csv", header = TRUE)
df5 = read.csv("Data/202105-divvy-tripdata.csv", header = TRUE)
df6 = read.csv("Data/202106-divvy-tripdata.csv", header = TRUE)
df7 = read.csv("Data/202107-divvy-tripdata.csv", header = TRUE)
df8 = read.csv("Data/202108-divvy-tripdata.csv", header = TRUE)
df9 = read.csv("Data/202109-divvy-tripdata.csv", header = TRUE)
df10 = read.csv("Data/202110-divvy-tripdata.csv", header = TRUE)
df11 = read.csv("Data/202111-divvy-tripdata.csv", header = TRUE)
df12 = read.csv("Data/202112-divvy-tripdata.csv", header = TRUE)

#check if our data sets are bindable
compare_df_cols_same(
  df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12,
  bind_method = c("bind_rows", "rbind"),
  verbose = TRUE
)

#binding all the data sets
cyclistic  <- rbind (df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
head(cyclistic)

##Process
#remove the geographical indication contained in the column from 9 to 12
cyclistic <- cyclistic[ -c(9:12) ]
head(cyclistic)

#assign NA to empty cells and remove al the rows containing NAs
cyclistic[cyclistic == '' ] <- NA
cyclistic<- na.omit(cyclistic)
view(head(cyclistic))

#convert ended_at end started_at to date-time
cyclistic$ended_at <- as.POSIXct(cyclistic$ended_at, format="%Y-%m-%d %H:%M:%S", tz="UTC")
cyclistic$started_at <- as.POSIXct(cyclistic$started_at, format="%Y-%m-%d %H:%M:%S", tz="UTC")


#calculate every ride length in minutes
ride_minutes <- difftime(cyclistic$ended_at,cyclistic$started_at, units = "mins")
cyclistic <- mutate(cyclistic,ride_length = ride_minutes)
head(cyclistic)

#get the hour of the ride
hour_ride <- hour(cyclistic$started_at)
cyclistic <- mutate(cyclistic, ride_time = hour_ride)

#get the day of the ride
day_ride <- wday(cyclistic$started_at, TRUE)
cyclistic <- mutate(cyclistic, ride_day = day_ride)

#get the month of the ride
month_ride <- month(cyclistic$started_at)
cyclistic <- mutate(cyclistic, ride_month = month_ride)

#Removing negative and short rides (less or equal to 1 minute)
cyclistic <- filter(cyclistic, ride_length >= 1)

#data set export
write.csv(cyclistic, "cyclistic.csv")

#lets use a sample 
#sample size from https://www.calculator.net/sample-size-calculator.html
#confidence level :95
#margin of error : 5
#population size :  4481079
#data size :385

cyclistic = read.csv('cyclistic.csv')

sampled_data = sample_n(cyclistic, 385)
write.csv(sampled_data, "final_cyclistic.csv")

view(sampled_data)




