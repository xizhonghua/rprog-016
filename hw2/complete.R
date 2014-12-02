complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    ## create empty vectors
    ids = numeric();
    nobs = numeric();
    
    ## loop all ids
    for (i in id) {
        ## generate the filename
        filename <- sprintf("%s/%03d.csv",directory,i)
        ## read the csv file
        data <- read.csv(filename)
        ## 1. use complete.cases() to test complete cases
        ## 2. index the complete cases
        ## 3. count the complete cases via rnow()
        nob <- nrow(data[complete.cases(data), ])
        ## append the id and number of complete cases to the vector
        ids = c(ids, i)
        nobs = c(nobs, nob)
    }
    
    ## construct the output data frame
    df <- data.frame(ids, nobs)
    ## output the data frame
    df
}
