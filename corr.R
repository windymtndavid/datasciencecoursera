corr <- function(directory, threshold = 0) {
    
    nob.meet = c()
    
    df <- complete(directory)
    
    ## get each file that meets threshold
    for (i in 1:nrow(df)) {
        if (df[i,"nobs"] > 0 && df[i,"nobs"] >= threshold) {
            nob.meet = append(nob.meet,df[i,"id"])
        }
    }
    
    ## if nob.meet is is null than no files met threshold
    if (is.null(nob.meet)) {
       numeric()
    } else {
        correlations <- c()
        for (i in 1:length(nob.meet)) {
            
            file.num <- nob.meet[i]
            ## add leading zeros
            if (nchar(file.num) == 1) {
                file.name <- paste("00",file.num,".csv", sep="", collapse ="")
            } else if (nchar(file.num) == 2) {
                file.name <- paste("0",file.num,".csv", sep="", collapse="")
            } else {
                file.name <- paste(file.num,".csv", sep="", collapse="")
            }
            
            ## build file path by adding directory
            file.path = paste(directory,"/",file.name, sep="", collapse="")
            
            file.data <- read.csv(file.path, header=TRUE)
          
            notNA <- intersect(which(!is.na(file.data$sulfate)),which(!is.na(file.data$nitrate)))
            
            cor.result <- cor(file.data$nitrate[notNA],file.data$sulfate[notNA])

            correlations <- append(correlations, c(cor.result))
            
        }
        correlations
    }
}