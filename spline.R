# spline.R

# calc_speed returns a vector of speeds in meters/sec.  is essentially the code for lat_long_conv.R, with an added 
# calculation for speed = abs(distance/time) 

calc_speed <- function(lat,long,int_sec) {
    
    # add columns lat_m1 and long_m1 in dataframe tmp with the latitides and longitudes of the previous rows
    
    tmp <- data.frame(lat,long,int_sec)
    np <- nrow(tmp)
    t <- tmp$lat[1:(np - 1)]
    t <- c(t[1],t)
    tmp$lat_m1 <- t
    
    t <- tmp$long[1:(np - 1)]
    t <- c(t[1],t)
    tmp$long_m1 <- t
    
    # add columns to tmp for diff_lat, diff_long, and ave_lat
    
    tmp$diff_lat <- abs(tmp$lat - tmp$lat_m1)
    tmp$diff_long <- abs(tmp$long - tmp$long_m1)
    tmp$ave_lat <- (tmp$lat + tmp$lat_m1)/2
    
    # this section uses the equations used by, for example:
    # http://www.csgnetwork.com/degreelenllavcalc.html
    # which is referenced in Wikipedia (https://en.wikipedia.org/wiki/Geographic_coordinate_system)
    # and is apparently accurate to 1 cm per degree latitude/longitude
    
    m1 <- 111132.92     # latitude calculation term 1
    m2 <- -559.82       # latitude calculation term 2
    m3 <- 1.175         # latitude calculation term 3
    m4 <- -0.0023       # latitude calculation term 4
    p1 <- 111412.84     # longitude calculation term 1
    p2 <- -93.5         # longitude calculation term 2
    p3 <- -0.118         # longitude calculation term 3
    
    # add columns lat_meters_per_1deg and long_meters_per_1deg to tmp 
    # these equations give the length of 1 deg (lat and long) in meters for a given latitude, using the ave of the latitudes
    
    tmp$lat_meters_per_1deg <- m1 + m2 * cos(2 * tmp$ave_lat*pi/180) + m3 * cos(4 * tmp$ave_lat*pi/180) + m4 * cos(6 * tmp$ave_lat*pi/180)
    tmp$long_meters_per_1deg <- p1 * cos(tmp$ave_lat*pi/180) + p2 * cos(3 * tmp$ave_lat*pi/180) + p3 * cos(5 * tmp$ave_lat*pi/180)
    
    # now convert differences in degrees to meters
    tmp$diff_long <- tmp$diff_long*tmp$long_meters_per_1deg
    tmp$diff_lat <- tmp$diff_lat*tmp$lat_meters_per_1deg
    
    # calculate Euclidean distance and speed, m/sec
    tmp$distance <- sqrt(tmp$diff_lat^2 + tmp$diff_long^2)
    tmp$speed <- abs(tmp$distance/as.numeric(tmp$int_sec))
    
    # return the vector for speed  
    ret <- tmp$speed 
    return(ret)
    
}

# beginning of spline 

library(lubridate)
mps_limit <- 100

#home <- "C:/Users/ibshi/Desktop/startup.ml/taxi"
home <- "C:/Users/Steve S/Desktop/startup.ml/taxi"
setwd(home)

# read.csv infile into dataframe raw

#infile <- "/06/39.txt"
infile <- "/06/220.txt"
#infile <- "/06/234.txt"
infile <- paste(home, infile,sep="")

raw <- read.csv(infile, header = FALSE)
colnames(raw) <- c("taxi_id","date_time","longitude","latitude")
npoints_orig <- nrow(raw)

# add column new_date_time to raw, converting input date_time to POSIXct

raw$new_date_time <- ymd_hms(raw$date_time)

# add column new_date_time_m1 to raw, with the new_date_time from the previous rows

t <- raw$new_date_time[1:(npoints_orig-1)]
t <- c(t[1],t)
raw$new_date_time_m1 <- t

# add column interval_sec to raw, set first interval to NA 

raw$interval_sec <- as.duration(interval(raw$new_date_time_m1,raw$new_date_time))
raw$interval_sec[1] <- NA

# set filtered dataframe equal to raw

filtered <- raw

# take out the rows with durations equal to zero

# For the rows one back from rows having an interval of zero, set the latitudes 
# and longitudes equal to the average of the two rows

for (k in 2:npoints_orig) {
    if (filtered$interval_sec[k] == as.duration(0)){
        filtered$latitude[k-1] <- (filtered$latitude[k-1]+filtered$latitude[k])/2
        filtered$longitude[k-1] <- (filtered$longitude[k-1]+filtered$longitude[k])/2
    }
}

# remove the rows with interval_sec equal to 0

filtered <- filtered[which(filtered$interval_sec != as.duration(0)),]
npoints_nozero <- nrow(filtered)

# remove outliers in speed (above mps_limit)

# set the initial lat, long, int_sec values as input to calc_speed

lat <- filtered$latitude
long <- filtered$long
int_sec <- filtered$interval_sec

# calculate the first pass of calc_speed, adding column speed to filtered

filtered$speed <- calc_speed(lat, long, int_sec)

# set s as a vector containing indices of rows with speeds above thresholds

s <- which(filtered$speed > mps_limit)

# while the length of s != 0 (while there are speeds above threshold)

while (length(s) != 0) {
    
    # remove the first instance of speed above threshold
    
    curr_ind <- s[1]
    curr_nrow <- nrow(filtered) 
    filtered <- rbind(filtered[1:(curr_ind-1),],filtered[(curr_ind+1):curr_nrow,])
    
    # recalculate speed 
    
    lat <- filtered$latitude
    long <- filtered$long
    int_sec <- filtered$interval_sec
    filtered$speed <- calc_speed(lat, long, int_sec)
    
    # re-set s as vector containing indices of rows with speeds above thresholds
    
    s <- which(filtered$speed > mps_limit)
    
}

# print out number of points after each stage of filtering

npoints <- nrow(filtered)
print(paste("npoints orig",as.character(npoints_orig)))
print(paste("npoints no zeroes",as.character(npoints_nozero)))
print(paste("npoints removing high speeds",as.character(npoints)))

# perform splines upon latitude and longitude (wrt new_date_time)

smoothed_lat  <- spline(filtered$new_date_time,filtered$latitude,n=length(filtered$new_date_time))
smoothed_long <- spline(filtered$new_date_time,filtered$longitude,n=length(filtered$new_date_time))

# plot smoothed_latitude wrt time

pdf(file = "smooth_lat.pdf")
plot(filtered$new_date_time,filtered$latitude,ylab = "latitude",xlab = "time",main="Smoothed Latitude wrt Time")
mtext("Points represent the raw data. Lines represent the smoothed spline trajectory.",side = 3)
lines(smoothed_lat$x,smoothed_lat$y)
dev.off()

# plot smoothed_longitude wrt time 

pdf(file = "smooth_long.pdf")
plot(filtered$new_date_time,filtered$longitude,ylab = "longitude",xlab = "time",main="Smoothed Longitude wrt Time")
mtext("Points represent the raw data. Lines represent the smoothed spline trajectory.",side = 3)
lines(smoothed_long$x,smoothed_long$y)
dev.off()

# plot smoothed Latitude vs. Longitude

pdf(file = "smooth_lat_v_long.pdf")
plot(filtered$latitude,filtered$longitude,ylab = "longitude",xlab = "latitude",main = "Smoothed Latitude and Longitude")
mtext("Points represent the raw data. Lines represent the smoothed spline trajectory.",side = 3)
lines(smoothed_lat$y,smoothed_long$y)
dev.off()

