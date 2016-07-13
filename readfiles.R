# readfiles.R

#num_files <- nrow(filelist)
num_files <- 100 # to test code for subsequent steps
print(paste("num_files = ",as.character(num_files)))

# s is a null dataframe for read.csv() failures
s <- data.frame(NULL,NULL,NULL,NULL)    

# give s column names that match a successful read.csv() result
s <- colnames(c("V1","V2","V3","V4")) 

# loop through filelist to read data into taxi_data

for (j in 1:num_files) {
    
    # tempname is the full name of a txt file
    
    t_ind <- as.integer(filelist[j,1]) 
    tempname <- paste(home,indir[t_ind],filelist[j,2],sep="/") 
    
    # a tryCatch that returns a temporary dataframe t with data from the tempdata file, or a null s dataframe
    
    t <- tryCatch(read.csv(tempname, header = FALSE),error = function(e) {print(paste("error retrieving",tempname));return(s);})
    
    # conditional upon t$V1[1] not being null, 2 steps were done: 
    # 1. add begin column to t to indicate a new taxi_id 
    # 2. add t to taxi_data  
    
    if (!is.null(t$V1[1])) { 
        
        # 1. add begin column to t to indicate a new taxi_id 
        
        t_len <- nrow(t)                
        t_begin <- c(rep(0,t_len))
        t_begin[1] <- 1
        t_begin <- as.factor(t_begin)
        t <- cbind(t,t_begin)   
        
        # 2. add t to taxi_data
        
        if (j == 1) {
            taxi_data <- t
        } 
        else {
            taxi_data <- rbind(taxi_data,t)
        }
    }
    
} 

colnames(taxi_data) <- c("taxi_id","date_time","longitude","latitude","begin")
