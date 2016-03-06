rankhospital <- function(state, outcome, num = "best") {
    
    ## read csv to dataframe
    df <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    ## subset the data frame by the {state} parameter
    by_state <- subset(df, State == state)
    if (nrow(by_state) == 0) stop ("invalid state")
    
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
    col_pos <- which(names(by_state) == column_name)
    
    ## if length 0 then column does not exist
    ## outcome parameter is not valid
    if (length(col_pos) == 0) stop ("invalid outcome")
    
    ## create subset of data by key state
    complete <- subset(by_state, complete.cases(by_state[[column_name]]))

    ## handle num parameter
    if (num == "best") {
        
        ## sort with key outcome ascending, hospital name ascending
        ## top item
        sorted <- complete$Hospital.Name[order(as.numeric(complete[[column_name]]),complete$Hospital.Name)]
        head(sorted, 1)
        
    } else if (num == "worst") {
        
        ## sort with key outcome descending, hopistal name ascending
        ## top item
        sorted <- complete$Hospital.Name[order(-as.numeric(complete[[column_name]]),complete$Hospital.Name)]
        head(sorted, 1)
        
    } else {
        
        ## sort with key outcome ascending, hospital name ascending
        ## [num] item
        sorted <- complete$Hospital.Name[order(as.numeric(complete[[column_name]]),complete$Hospital.Name)]
        sorted[num]
        
    }
  
}