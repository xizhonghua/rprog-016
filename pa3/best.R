best <- function(state, outcome) {
    ## Read outcome data
    d <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
    
    ## Check that state and outcome are valid
    
    d <- subset(d, State == state)
    
    if (length(d$State) == 0) stop("invalid state")
    
    if (outcome == 'heart attack') {
        d <- d[complete.cases(d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack), ]
        best_name <- d[which.min(d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack),]$Hospital.Name
    } else if (outcome == 'heart failure') {
        d <- d[complete.cases(d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure), ]
        best_name <- d[which.min(d$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure),]$Hospital.Name
    } else if (outcome == 'pneumonia') {
        d <- d[complete.cases(d$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia), ]
        best_name <- d[which.min(d$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia),]$Hospital.Name
    } else {
        stop('invalid outcome')
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    as.character(best_name)
}