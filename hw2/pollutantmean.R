## Write a function named 'pollutantmean' that calculates the
## mean of a pollutant (sulfate or nitrate) across a specified
## list of monitors. The function 'pollutantmean' takes three
## arguments: 'directory', 'pollutant', and 'id'. Given a 
## vector monitor ID numbers, 'pollutantmean' reads that 
## monitors' particulate matter data from the directory 
## specified in the 'directory' argument and returns the mean 
## of the pollutant across all of the monitors, ignoring any 
## missing values coded as NA. A prototype of the function is
## as follows

pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    ## select the col
    col <- if (pollutant == "sulfate") 2 else 3;
    
    ## create a empty (length = 0) numeric vector
    table <- numeric();
    
    ## loop each file id
    for(i in id) {
        ## generate the filename
        filename <- sprintf("%s/%03d.csv", directory, i)
        ## read the cvs, store the only needed column
        data <- read.csv(filename)[, col]
        ## append the rows to the big table
        table <- c(table, data)
    }
    
    ## remove the NAs, and compute the mean on all rows acrossing
    ## all files
    mean(table, na.rm = T)
}
