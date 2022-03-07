# function to load anonymized CSV data
#
# example usage:
#
# library(plyr)
# library(readr)
#
# pathAnon <- '' # set anonymized data directory
#
# d <- import(pathAnon)

import <- function(pathAnon) {
  
  # set anonymized data path
  setwd(pathAnon)
  
  # generate list of datafiles 
  myfiles <- list.files(path=pathAnon, pattern="*.csv", full.names=TRUE)
  
  # specify columns with otherwise incorrect ID/parsing
  colTypes <- cols(rt = col_double())
  
  # load data (checking 2000 rows to correctly ID/parse most columns)
  d <- ldply(myfiles, read_csv, na="NA", guess_max=2000, col_types = colTypes)
}
