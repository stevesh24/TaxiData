# getfilenames.R

#home <- "C:/Users/ibshi/Desktop/startup.ml/taxi"
home <- "C:/Users/Steve S/Desktop/startup.ml/taxi"
setwd(home)

indir <- c("06","07","08","09","010","011","012","013","014")
num_indir <- length(indir)

# loop through each subdirectory

for (i in 1:num_indir) { 
    
    # temp holds the subdirectory name
    temp <- paste(home, indir[i], sep="/")
    
    # put file names into l
    l <- list.files(path=temp)
    
    # add an index for the subdirectory name vector indir to l
    index <- c(rep.int(i,length(l)))
    l <- cbind(index,l)
    
    # add l to filelist
    if (i == 1) {
        filelist <- l
    } 
    else {
        filelist <- rbind(filelist,l)
    }
    
} 
