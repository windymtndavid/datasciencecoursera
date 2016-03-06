## The purpose of this function is to read a data file and output a specific summary
## The data file is hospital performace data for each state and various conditions (outcomes)
## The output is a list of a hospital for each state
## The hospital listed is either the "best", "worst" or a given rank number (integer value 1 or greater)

rankall <- function(outcome, num = "best") {
    
    ## read csv to dataframe
    in_df <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    ## this is the key observation for each outcome     
    prefix <- "Hospital.30.Day.Death..Mortality..Rates.from."
    
    ## format the input to match column names
    ## capitalize the first letter of each word
    s <- strsplit(outcome, " ")[[1]]
    outcome_capitalized <- paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
    
    ## replace space with dot 
    outcome_no_space <- gsub(" ", ".", outcome_capitalized)
    
    ## create full column name to search
    column_name <- paste(prefix, outcome_no_space, sep = "")
    
    ## will return an object of length 1 or 0
    col_pos <- which(names(in_df) == column_name)
    
    ## if length 0 then column does not exist
    ## outcome parameter is not valid
    if (length(col_pos) == 0) stop ("invalid outcome")
    
    ## get each state and sort
    all_state <- sort(unique(in_df$State))
    
    ## initiaize out going dataframe
    state <- NULL
    hospital <- NULL
    out_df <- data.frame(hospital, state)
    
    ## loop through each state
    for (i in 1:length(all_state)) {
        
        ## subset data frame by state
        state <- all_state[i]
        by_state <- subset(in_df, State == state)
        
        ## subset data frame by complete cases
        complete <- subset(by_state, complete.cases(by_state[[column_name]]))
        
        num_int <- paste(num, "L", sep = "")
        
        ## handle ranking parameter: num
        if (num == "best") {
            
            ## sort with key outcome ascending, hospital name ascending
            ## top item
            sorted <- complete$Hospital.Name[order(as.numeric(complete[[column_name]]),complete$Hospital.Name)]
            hospital <- head(sorted, 1)
            
        } else if (num == "worst") {
            
            ## sort with key outcome descending, hopistal name ascending
            ## top item
            sorted <- complete$Hospital.Name[order(-as.numeric(complete[[column_name]]),complete$Hospital.Name)]
            hospital <- head(sorted, 1)
            
        } else {
            
            ## sort with key outcome ascending, hospital name ascending
            ## [num] item
            sorted <- complete$Hospital.Name[order(as.numeric(complete[[column_name]]),complete$Hospital.Name)]
            hospital <- sorted[num]
        }
    
        ## ammend new row to data frame
        row <- data.frame(hospital, state)
        out_df <- rbind(out_df, row)
    
    }
    
    ## output data frame
    out_df    

}