# intervals.R

library(lubridate)

# add a column (new_date_time), converting the input date_time column to POSIXct date time format
taxi_data$new_date_time <- ymd_hms(taxi_data$date_time)

# add a column (new_date_time_m1) with the new_date_time of the previous row 
num_datam1 <- nrow(taxi_data) - 1
t <- taxi_data$new_date_time[1:num_datam1]
t <- c(t[1],t)
taxi_data$new_date_time_m1 <- t
rm(t)

# add a column to taxi_data (interval_sec) with the duration (in seconds) of the interval between 
# new_date_time and new_date_time_m1. 
taxi_data$interval_sec <- as.duration(interval(taxi_data$new_date_time_m1,taxi_data$new_date_time))

# set interval_sec for the beginning of a new taxi_id to NA
taxi_data$interval_sec[which(taxi_data$begin == 1)] <- NA

# Drop the date_time and new_date_time_m1 columns
taxi_data <- subset.data.frame(taxi_data,select = c("taxi_id","longitude","latitude","begin","new_date_time","interval_sec"))
