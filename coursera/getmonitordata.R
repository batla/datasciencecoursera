getmonitordata <- function(directory, ids = 1:332) {
  ## function to get raw monitor data from .csv files into a data frame
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  if(grep("specdata",directory) ==1) 
  {
    directory <- ("/Users/sbatla/specdata/")
  }
  
  ## list of all monitor files to put filenames in ordered list to match id variable to filenames
  ## NOTE: can likely do this in a more robust fashion, using regex to avoid creating ordered list
  monitorfilelist <- list.files(directory)
  
  monitorfiles <- c()
  monitordata <- data.frame()
  
  ## only read monitor files requested, stitch together all data
  for (i in ids){
    monitorfiles[[i]] <- read.csv(monitorfilelist[[i]])
    monitordata <- rbind(monitordata, monitorfiles[[i]])
  }
  
  return(monitordata)
}