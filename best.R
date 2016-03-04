best <- function(state, outcome, num = "best") {
    
    ## read csv to dataframe
    df <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
    
    ## subset the data frame by the {state} parameter
    by_state <- subset(df, State == state)
    if (length(by_state) == 0) stop ("invalid state")
    
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

        ##get the min value of key observation
        key_value <- min(as.numeric(by_state[[column_name]]), na.rm = TRUE)
        
        ## multi step
        ## 1. subset by key observation equal to min value (can be more than one if tied)
        ## 2. sort by name
        ## 3. return first item
        head(sort(by_state$Hospital.Name[which(as.numeric(by_state[[column_name]]) == key_value)]),1)

}