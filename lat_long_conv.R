# lat_long_conv.R

# Add columns lat_m1 and long_m1 to *taxi_data* for the latitudes and longitudes of the previous rows.

num_datam1 <- nrow(taxi_data) - 1
t <- taxi_data$latitude[1:num_datam1]
t <- c(t[1],t)
taxi_data$lat_m1 <- t

t <- taxi_data$longitude[1:num_datam1]
t <- c(t[1],t)
taxi_data$long_m1 <- t

rm(t)

# Add columns diff_lat, diff_long, and ave_lat to *taxi_data* using latitude, lat_m1, longitude, and long_m1 

taxi_data$diff_lat <- abs(taxi_data$latitude - taxi_data$lat_m1)
taxi_data$diff_long <- abs(taxi_data$longitude - taxi_data$long_m1)
taxi_data$ave_lat <- (taxi_data$latitude + taxi_data$lat_m1)/2

# remove intermediary columns lat_m1 and long_m1 

taxi_data <- subset.data.frame(taxi_data,select = c("taxi_id","longitude","latitude","begin","new_date_time","interval_sec","diff_lat","diff_long","ave_lat"))

# Convert differences in latitude and longitude to distances in meters. 

# this section uses the equations used by, for example:
# http://www.csgnetwork.com/degreelenllavcalc.html,
# which is referenced in Wikipedia (https://en.wikipedia.org/wiki/Geographic_coordinate_system),
# and is apparently accurate to 1 cm per degree latitude/longitude

m1 <- 111132.92     # latitude calculation term 1
m2 <- -559.82       # latitude calculation term 2
m3 <- 1.175         # latitude calculation term 3
m4 <- -0.0023       # latitude calculation term 4
p1 <- 111412.84     # longitude calculation term 1
p2 <- -93.5         # longitude calculation term 2
p3 <- -0.118         # longitude calculation term 3

# Add columns lat_meters_per_1deg and lat_meters_per_1deg to *taxi_data* computing the length of 1 degree latitude 
# and longitude for the average latitude between 2 adjacent points (using the equations and parameters above)

taxi_data$lat_meters_per_1deg <- m1 + m2 * cos(2 * taxi_data$ave_lat*pi/180) + m3 * cos(4 * taxi_data$ave_lat*pi/180) + m4 * cos(6 * taxi_data$ave_lat*pi/180)
taxi_data$long_meters_per_1deg <- p1 * cos(taxi_data$ave_lat*pi/180) + p2 * cos(3 * taxi_data$ave_lat*pi/180) + p3 * cos(5 * taxi_data$ave_lat*pi/180)

# convert diff_lat and diff_long from degrees to meters

taxi_data$diff_long <- taxi_data$diff_long*taxi_data$long_meters_per_1deg
taxi_data$diff_lat <- taxi_data$diff_lat*taxi_data$lat_meters_per_1deg

# remove intermediary columns ave_lat, lat_meters_per_1deg, and long_meters_per_1deg 

taxi_data <- subset.data.frame(taxi_data,select = c("taxi_id","longitude","latitude","begin","new_date_time","interval_sec","diff_lat","diff_long"))

# Add column distance (in meters) to *taxi_data* by computing Euclidean distance from columns diff_lat and diff_long

taxi_data$distance <- sqrt(taxi_data$diff_lat^2 + taxi_data$diff_long^2)

# set distance for the beginning of a new taxi_id to NA

taxi_data$distance[which(taxi_data$begin == 1)] <- NA

# remove intermediary columns diff_lat and diff_long 

taxi_data <- subset.data.frame(taxi_data,select = c("taxi_id","longitude","latitude","begin","new_date_time","interval_sec","distance"))
