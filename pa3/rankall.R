rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    d <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
    
    valid_outcome = c('heart attack', 'heart failure', 'pneumonia')
    
    if(! outcome %in% valid_outcome) stop('invalid outcome')
    
    fullnames <- c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 
                  'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',
                  'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
    )
    
    ## get the column name
    fullname = fullnames[match(outcome, valid_outcome)]
    
    ## find unique state names
    statesNames <- unique(d$State)
    ## sort the names
    statesNames <- statesNames[order(statesNames)]
    
    state <- character()
    hospital <- character()
    
    ## loop each state
    for(s in statesNames) {
        
        state.d = subset(d, State == s)
        
        state.d[, fullname] <- as.numeric(state.d[, fullname])
    
        state.d <- state.d[order(state.d[fullname], state.d$Hospital.Name),]
    
        l <- sum(complete.cases(state.d[fullname]))
    
        ## row_idx
        row_idx <- if (num == 'best') 1 else if (num == 'worst') l else num
    
        name <- state.d[row_idx,]$Hospital.Name
        
        ## append result to vector
        state <- c(state, s)
        hospital <- c(hospital, name)
    }
    
    ## return a dataframe
    ## hack to use tow state vectors
    ## one as column, one as row name, must turn off check.names
    data.frame(hospital, state, state, row.names=2, check.names = F)
}
