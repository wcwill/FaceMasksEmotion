# faceMask study 2 data cleaning and processing script
# 10/15/20
library(plyr)
library(readr)

# set path and load functions
path <- ""
setwd(path)
source('importCSVs.R')

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

# calculate questionnaire scores
# AQ
dqa$AQC1 <- 3- dqa$AQC1
dqa$AQC3 <- 3- dqa$AQC3
dqa$AQC4 <- 3- dqa$AQC4
dqa$AQC7 <- 3- dqa$AQC7
dqa$AQC8 <- 3- dqa$AQC8
dqa$AQC10 <- 3- dqa$AQC10

dqa$AQ_C <- dqa$AQC1 + dqa$AQC2 + dqa$AQC3 + dqa$AQC4 + dqa$AQC5 + dqa$AQC6 + dqa$AQC7 + dqa$AQC8 + dqa$AQC9 + dqa$AQC10

# BEQ
dqa$BEQ_S <- rowMeans(dqa[c('BEQS1', 'BEQS2', 'BEQS3', 'BEQS4', 'BEQS5', 'BEQS6')])

# BFI
dqa$BFI2 <- 4 - dqa$BFI2
dqa$BFI6 <- 4 - dqa$BFI6
dqa$BFI8 <- 4 - dqa$BFI8
dqa$BFI9 <- 4 - dqa$BFI9
dqa$BFI12 <- 4 - dqa$BFI12
dqa$BFI18 <- 4 - dqa$BFI18
dqa$BFI21 <- 4 - dqa$BFI21
dqa$BFI23 <- 4 - dqa$BFI23
dqa$BFI24 <- 4 - dqa$BFI24
dqa$BFI27 <- 4 - dqa$BFI27
dqa$BFI31 <- 4 - dqa$BFI31
dqa$BFI34 <- 4 - dqa$BFI34
dqa$BFI35 <- 4 - dqa$BFI35
dqa$BFI37 <- 4 - dqa$BFI37
dqa$BFI41 <- 4 - dqa$BFI41
dqa$BFI43 <- 4 - dqa$BFI43
dqa$BFI_A <- rowMeans(dqa[c('BFI2', 'BFI7', 'BFI12', 'BFI17', 'BFI22', 'BFI27', 'BFI32', 'BFI37', 'BFI42')])
dqa$BFI_C <- rowMeans(dqa[c('BFI3', 'BFI8', 'BFI13', 'BFI18', 'BFI23', 'BFI28', 'BFI33', 'BFI38', 'BFI43')])
dqa$BFI_E <- rowMeans(dqa[c('BFI1', 'BFI6', 'BFI11', 'BFI16', 'BFI21', 'BFI26', 'BFI31', 'BFI36')])
dqa$BFI_N <- rowMeans(dqa[c('BFI4', 'BFI9', 'BFI14', 'BFI19', 'BFI24', 'BFI29', 'BFI34', 'BFI39')])
dqa$BFI_O <- rowMeans(dqa[c('BFI5', 'BFI10', 'BFI15', 'BFI20', 'BFI25', 'BFI30', 'BFI35', 'BFI40', 'BFI41', 'BFI44')])

# INDCOL
dqa$INDCOL_HC <- dqa$INDCOL1 + dqa$INDCOL5 + dqa$INDCOL10 + dqa$INDCOL14
dqa$INDCOL_VC <- dqa$INDCOL2 + dqa$INDCOL3 + dqa$INDCOL7 + dqa$INDCOL12
dqa$INDCOL_HI <- dqa$INDCOL6 + dqa$INDCOL8 + dqa$INDCOL11
dqa$INDCOL_VI <- dqa$INDCOL4 + dqa$INDCOL9 + dqa$INDCOL13
dqa$INDCOL_C <- dqa$INDCOL_HC + dqa$INDCOL_VC
dqa$INDCOL_I <- dqa$INDCOL_HI + dqa$INDCOL_VI

# IRQ
dqa$IRQ_NT <- dqa$IRQ1 + dqa$IRQ2 + dqa$IRQ3 + dqa$IRQ4
dqa$IRQ_NE <- dqa$IRQ5 + dqa$IRQ6 + dqa$IRQ7 + dqa$IRQ8
dqa$IRQ_PT <- dqa$IRQ9 + dqa$IRQ10 + dqa$IRQ11 + dqa$IRQ12
dqa$IRQ_PE <- dqa$IRQ13 + dqa$IRQ14 + dqa$IRQ15 + dqa$IRQ16
dqa$IRQ_Total <- dqa$IRQ_NT + dqa$IRQ_NE + dqa$IRQ_PT + dqa$IRQ_PE

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
