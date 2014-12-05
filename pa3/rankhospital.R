rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    d <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
    
    d <- subset(d, State == state)
    
    ## if no obs on that state ...
    if (length(d$State) == 0) stop("invalid state")
    
    valid_outcome = c('heart attack', 'heart failure', 'pneumonia')
    
    if(! outcome %in% valid_outcome) stop('invalid outcome')
    
    fullnames <- c('Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack', 
                  'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',
                  'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
    )
    
    ## get the column name
    fullname = fullnames[match(outcome, valid_outcome)]
    
    d[, fullname] <- as.numeric(d[, fullname])
    
    d <- d[order(d[fullname], d$Hospital.Name),]
    
    l <- sum(complete.cases(d[fullname]))
    
    ## row_idx
    row_idx <- if (num == 'best') 1 else if (num == 'worst') l else num
    
    name <- d[row_idx,]$Hospital.Name
    
    name
}
