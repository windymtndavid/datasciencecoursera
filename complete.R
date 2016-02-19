complete <- function(directory, id = 1:332) {
    
    nobs.id = c()
    nobs.cnt = c()
    
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

        file.data <- read.csv(file.path, header=TRUE)
        
        ## get count where both !is.na
        cnt <- length(which(!is.na(file.data$sulfate[which(!is.na(file.data$nitrate))])))
        
        nobs.id <- append(nobs.id, c(i))
        nobs.cnt <- append(nobs.cnt,c(cnt))
        
        nobs.df <- data.frame(id=nobs.id,nobs=nobs.cnt)
    
    }
    
    nobs.df

}
