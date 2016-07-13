# figure_dist.R

# Count the number of rows with distance > 8000 (over_8000).
over_8000 <- length(which(taxi_data$distance>8000))

# In t (a temporary vector equal to taxi_data$distance), set those over_8000 to NA. 
t <- taxi_data$distance
t[which(t>8000)] <- NA

# Draw a density plot (h) to stdout.
h <- hist(t,xlim=c(0,8000),freq=FALSE,breaks =c(seq(0,8000,by =500)))

# The plot h is converted to proportions with the count for over_8000 included, so that the proportions represent 
# those over all the data (including those above 8000).
h$counts <- h$counts/(sum(h$counts) + over_8000)

# The plot is written to distance.pdf
pdf(file ="distance.pdf")
plot(h,xlab = "distance interval (meters)",ylab = "proportion",main="", col = 34,xaxp  = c(0, 8000, 8),yaxp = c(0,.7,7),tck = 0.01)
dev.off()
rm(t)
