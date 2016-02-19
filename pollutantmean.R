pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    ##init monitor variable to store each mean
    ttl.sum <- 0
    ttl.cnt <- 0

    for (i in id) {
        
        ## add leading zeros
        if (nchar(i) == 1) {
            file.name <- paste("00",i,".csv", sep="", collapse ="")
        } else if (nchar(i) == 2) {
            file.name <- paste("0",i,".csv", sep="", collapse="")
        } else {
            file.name <- paste(i,".csv", sep="", collapse="")
        }
        
        ## build file path by adding directory
        file.path = paste(directory,"/",file.name, sep="", collapse="")
        
        ## load file
        file.data <- read.csv(file.path, header=TRUE)
        
        row.sum <- sum(file.data[[pollutant]], na.rm = TRUE)
        row.cnt <- length(which(!is.na(file.data[[pollutant]])))
    
        ttl.sum <- ttl.sum + row.sum
        ttl.cnt <- ttl.cnt + row.cnt
    
    }
    
    ##output mean of all monitors
    ##mean(monitor)
    ttl.sum/ttl.cnt
    
    
}