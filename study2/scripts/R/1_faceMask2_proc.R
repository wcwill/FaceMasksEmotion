# faceMask study 2 data cleaning and processing script
# 10/15/20
library(plyr)
library(readr)

# set path and load functions
path <- ""
setwd(path)
source('importCSVs.R')
source('scoreQs.R')

# set path to anonymized data
pathAnon <- ""

####---- setup data ----####
# import anonymized data
d <- import(pathAnon)

# identify and remove participants failing attention checks
attentionCheckFail <- unique(d$subject[d$attentionCheck != 2])
d <- d[! d$subject %in% attentionCheckFail,]

# identify and remove participants failing and repeating more than 4 of the 9 instruction checks
dInsCheck <- subset(d, !is.na(d$checkRatingAccuracy))
dInsCheckLength <- aggregate(whichWindow ~ subject, data=dInsCheck, FUN=length)
insCheckFail4 <- dInsCheckLength$subject[dInsCheckLength$whichWindow > 13]
d <- d[! d$subject %in% insCheckFail4,]


####---- setup questionnaire data ----####
# subset questionnaire trials
dq <- subset(d, d$whichWindow=='questionnaire')

# aggregate questionnaire data by subject ID
dqa <- aggregate(dq, by=list(dq$subject), FUN=mean, na.rm=TRUE)

# list questionnaires to be scored
whichQs <- c('AQ_C', 'BEQ_S', 'BFI', 'INDCOL', 'IRQ')

# calculate questionnaire scores
dqa <- scoreQs(dqa, whichQs)

# subset questionnaire totals
dqa <- dqa[,-c(2:150)]
colnames(dqa)[1] <- 'subject'


####---- setup task data ----####
# subset task trials
d <- subset(d, d$whichWindow=='emotionRatingFaceWindow' | d$whichWindow=='personRatingFaceWindow')

# drop empty variables
d <- d[,-c(1:4,11:17,32:149)]

# reorder columns
colOrder <- c("subject", "subjectOS", "subjectBrowser", "keyPair", "block", "faceList", "emotionRating", "personRating", "trial", "stimulus", "face", "model", 
              "ethnicity", "sex", "expression", "mask", "key_press", "rateEmotion", "ratePerson", "rt")
d <- d[,colOrder]

# recode expressions
d$expression[d$expression=='A'] <- 'angry'
d$expression[d$expression=='D'] <- 'disgusted'
d$expression[d$expression=='F'] <- 'fearful'
d$expression[d$expression=='H'] <- 'happy'
d$expression[d$expression=='R'] <- 'surprised'
d$expression[d$expression=='S'] <- 'sad'

# recode and relevel masks
d$mask[d$mask=='L'] <- 'lower'
d$mask[d$mask=='U'] <- 'upper'
d$mask[d$mask=='X'] <- 'none'
d$mask <- as.factor(d$mask)
d$mask <- relevel(d$mask, 'none')

# generate face emotion variables
d$faceAngry[d$expression != 'angry'] <- 0
d$faceAngry[d$expression == 'angry' & d$mask != 'none'] <- 50
d$faceAngry[d$expression == 'angry' & d$mask == 'none'] <- 100

d$faceDisgusted[d$expression != 'disgusted'] <- 0
d$faceDisgusted[d$expression == 'disgusted' & d$mask != 'none'] <- 50
d$faceDisgusted[d$expression == 'disgusted' & d$mask == 'none'] <- 100

d$faceFearful[d$expression != 'fearful'] <- 0
d$faceFearful[d$expression == 'fearful' & d$mask != 'none'] <- 50
d$faceFearful[d$expression == 'fearful' & d$mask == 'none'] <- 100

d$faceHappy[d$expression != 'happy'] <- 0
d$faceHappy[d$expression == 'happy' & d$mask != 'none'] <- 50
d$faceHappy[d$expression == 'happy' & d$mask == 'none'] <- 100

d$faceSad[d$expression != 'sad'] <- 0
d$faceSad[d$expression == 'sad' & d$mask != 'none'] <- 50
d$faceSad[d$expression == 'sad' & d$mask == 'none'] <- 100

d$faceSurprised[d$expression != 'surprised'] <- 0
d$faceSurprised[d$expression == 'surprised' & d$mask != 'none'] <- 50
d$faceSurprised[d$expression == 'surprised' & d$mask == 'none'] <- 100


####---- merge task and questionnaire data ----####
d <- merge(d, dqa, by="subject")
