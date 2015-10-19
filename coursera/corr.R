corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!
  
  correlations <- numeric(0)
  
  # get complete observations as a data frame where ID = 1:332
  completeobservations <- complete("specdata")
  
  # apply threshold
  completeobservations <- na.omit(completeobservations[completeobservations$nobs > threshold, ])
  
  for (ids in completeobservations$ID) {
    ids <- as.numeric(ids)
    
    # get data for complete observations that exceed threshold
    monitordata <- getmonitordata(directory, ids)
    
    # correlation between $sulfate and $nitrate using pairwise complete observation method
    correlations <- c(correlations, cor(monitordata$sulfate, monitordata$nitrate, use = "pairwise.complete.obs"))
  }
  
  return(correlations)
}