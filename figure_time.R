# figure_time.R

# Set *t* (a temporary vector) equal to taxi_data$interval_sec
t <- taxi_data$interval_sec

# convert intervals from sec to min
t <- as.single(t)/60

# Count the number of rows with interval > 12 min 
over_12 <- length(which(t>12))

# Set the values over 12 to NA.
t[which(t>12)] <- NA

# Draw a density plot (h) to stdout. 
h <- hist(t,xlim=c(0,12),freq=FALSE,breaks =c(seq(0,12,by =0.5)))

# The plot h is converted to proportions with the count for over_12 included, 
# so that the proportions represent those over all the data (including those above 12).
h$counts <- h$counts/(sum(h$counts) + over_12)

# The plot is written to time.pdf.
pdf(file ="time.pdf")
plot(h,xlab = "time interval (minutes)",ylab = "proportion",main="", col = 34,xaxp  = c(0, 12, 6),yaxp =c(0,.4,8),tck = 0.01)
dev.off()
rm(t)
