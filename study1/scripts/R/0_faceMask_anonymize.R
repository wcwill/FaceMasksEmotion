# faceMask anonymizing script
# 7/31/20
library(plyr)
library(readr)

# set path
path <- ""
setwd(path)

# generate list of datafiles
myfiles <- list.files(path=path, pattern="*.csv", full.names=TRUE)

# update path
path <- ""
setwd(path)

# specify columns with otherwise incorrect ID/parsing
colTypes <- cols(rt = col_double())

# loop through datafiles
for(j in c(1:length(myfiles))) {
  # load data (checking 2000 rows to correctly ID/parse most columns)
  data <- read_csv(myfiles[j], na="null", guess_max=2000, col_types = colTypes)
  
  # drop identifying variables
  data <- data[,-c(5:7)]

  # write CSV with anonymized data
  # eval(parse(text=paste(paste('write_csv(data, "faceMask_anon_', toString(j), sep = ""), '.csv")', sep = "")))
}
