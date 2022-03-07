# function to anonymize CSV data
# imports raw data, drops identifying variables, and outputs anonymized data
#
# example usage:
#
# library(plyr)
# library(readr)
#
# pathRaw <- '' # set raw data directory
# pathAnon <- '' # set anonymized data directory
# dropVarCols <- c(5:7) # list identifying variables to be dropped (by dataframe column numbers)
# outputStemAnon <- 'faceMask_anon_' # state anonymized CSV filename stem
# 
# anonymize(pathRaw, pathAnon, dropVarCols, outputStemAnon)

anonymize <- function(pathRaw, pathAnon, dropVarCols, outputStemAnon) {
  
  # set raw data path
  setwd(pathRaw)
  
  # generate list of datafiles
  myfiles <- list.files(path=pathRaw, pattern="*.csv", full.names=TRUE)
  
  # set anonymized data path
  setwd(pathAnon)
  
  # specify columns with otherwise incorrect ID/parsing
  colTypes <- cols(rt = col_double())
  
  # loop through datafiles
  for(j in c(1:length(myfiles))) {
    # load data (checking 2000 rows to correctly ID/parse most columns)
    data <- read_csv(myfiles[j], na="null", guess_max=2000, col_types = colTypes)
    
    # drop identifying variables
    data <- data[,-dropVarCols]
    
    # write CSV with anonymized data
    eval(parse(text=paste(paste('write_csv(data, "', outputStemAnon, toString(j), sep = ""), '.csv")', sep = "")))
  }
}
