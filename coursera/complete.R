complete <- function(directory, ids = 1:332) {
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

  ## get raw monitor data from requested monitors
  ## monitor_data is a data frame
  monitordata <- getmonitordata(directory, ids)
  
  ## remove NA's from consideration
  cols <- c("sulfate", "nitrate")
  completeobservations <- na.omit(monitordata[!rowSums(is.na(monitordata[cols])), ])
  
  ## group observations by ID, using table function & match functions
  completeobservationsgrouped <- data.frame(table(completeobservations$ID))
  cobstemp <- completeobservationsgrouped[match(ids, completeobservationsgrouped$Var1),]
  
  ## re-label cols for output
  completeobservationsgroupedfinal <- rename(cobstemp, c("Var1"="ID", "Freq"="nobs"))
  
  ## row numbers are reversed, although order is as request. How to change this?
  ## rownums??
  
  completeobservationsgroupedfinal
  
}