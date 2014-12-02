corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    ## create an empty vector
    CO <- numeric()
    
    ## loop all ids (hard coded here...)
    for (i in 1:332) {
        ## generate the filename
        filename <- sprintf("%s/%03d.csv",directory,i)
        ## read the data
        data <- read.csv(filename)
        ## subset the complete cases
        data <- data[complete.cases(data), ]
        ## count the number of complete cases
        nob <- nrow(data)
        ## compare with the threshold
        if (nob > threshold) {
            ## compute the corrlation of two columns
            co <- cor(data[,2], data[,3])
            ## append the result
            CO <- c(CO, co)    
        }
    }
    
    ## output the result
    CO
}
