# faceMask data cleaning and processing script
# 7/31/20
library(plyr)
library(readr)

# set path
path <- ""
setwd(path)

####---- setup data ----####
# generate list of datafiles 
myfiles <- list.files(path=path, pattern="*.csv", full.names=TRUE)

# specify columns with otherwise incorrect ID/parsing and load data (checking 2000 rows to correctly ID/parse most columns)
colTypes <- cols(rt = col_double())
d <- ldply(myfiles, read_csv, na="NA", guess_max=2000, col_types = colTypes)

# identify and remove participants failing attention checks
attentionCheckFail <- unique(d$subject[d$attentionCheck != 2])
d <- d[! d$subject %in% attentionCheckFail,]

# identify and remove participants failing and repeating more than 3 of the 6 instruction checks
dInsCheck <- subset(d, !is.na(d$checkRatingAccuracy))
dInsCheckLength <- aggregate(whichWindow ~ subject, data=dInsCheck, FUN=length)
insCheckFail3 <- dInsCheckLength$subject[dInsCheckLength$whichWindow > 9]
d <- d[! d$subject %in% insCheckFail3,]


####---- setup questionnaire data ----####
# subset questionnaire trials
dq <- subset(d, d$whichWindow=='questionnaire')

# aggregate questionnaire data by subject ID
dqa <- aggregate(dq, by=list(dq$subject), FUN=mean, na.rm=TRUE)

# calculate questionnaire scores
# AQ
# NOTE: AQ items were accidentally reverse-coded at data collection (e.g. 0 = "definitely agree" & 3 = "definitely disagree")
# so leaving the reversed items as they are, and reversing the *positive* items instead
dqa$AQ3 <- 3- dqa$AQ3
dqa$AQ5 <- 3- dqa$AQ5
dqa$AQ6 <- 3- dqa$AQ6
dqa$AQ10 <- 3- dqa$AQ10
dqa$AQ11 <- 3- dqa$AQ11
dqa$AQ14 <- 3- dqa$AQ14
dqa$AQ15 <- 3- dqa$AQ15
dqa$AQ17 <- 3- dqa$AQ17
dqa$AQ19 <- 3- dqa$AQ19
dqa$AQ20 <- 3- dqa$AQ20

dqa$AQ_C <- dqa$AQ2 + dqa$AQ6 + dqa$AQ7 + dqa$AQ9 + dqa$AQ10 + dqa$AQ11 + dqa$AQ12 + dqa$AQ13 + dqa$AQ15 + dqa$AQ16
dqa$AQ_SS <- dqa$AQ1 + dqa$AQ3 + dqa$AQ4 + dqa$AQ5 + dqa$AQ8 + dqa$AQ14 + dqa$AQ17 + dqa$AQ18 + dqa$AQ19 + dqa$AQ20

# BEQ
dqa$BEQ3 <- 6 - dqa$BEQ3
dqa$BEQ8 <- 6 - dqa$BEQ8
dqa$BEQ9 <- 6 - dqa$BEQ9
dqa$BEQ_N <- rowMeans(dqa[c('BEQ3', 'BEQ5', 'BEQ8', 'BEQ9', 'BEQ13', 'BEQ16')])
dqa$BEQ_P <- rowMeans(dqa[c('BEQ1', 'BEQ4', 'BEQ6', 'BEQ10')])
dqa$BEQ_S <- rowMeans(dqa[c('BEQ2', 'BEQ7', 'BEQ11', 'BEQ12', 'BEQ14', 'BEQ15')])

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

# IRI
dqa$IRI2 <- 4 - dqa$IRI2
dqa$IRI3 <- 4 - dqa$IRI3
dqa$IRI7 <- 4 - dqa$IRI7
dqa$IRI8 <- 4 - dqa$IRI8
dqa$IRI9 <- 4 - dqa$IRI9
dqa$IRI9 <- 4 - dqa$IRI9
dqa$IRI_EC <- dqa$IRI1 + dqa$IRI3 + dqa$IRI5 + dqa$IRI7 + dqa$IRI9 + dqa$IRI10 + dqa$IRI12
dqa$IRI_PT <- dqa$IRI2 + dqa$IRI4 + dqa$IRI6 + dqa$IRI8 + dqa$IRI11 + dqa$IRI13 + dqa$IRI14

# IRQ
dqa$IRQ_NT <- dqa$IRQ1 + dqa$IRQ2 + dqa$IRQ3 + dqa$IRQ4
dqa$IRQ_NE <- dqa$IRQ5 + dqa$IRQ6 + dqa$IRQ7 + dqa$IRQ8
dqa$IRQ_PT <- dqa$IRQ9 + dqa$IRQ10 + dqa$IRQ11 + dqa$IRQ12
dqa$IRQ_PE <- dqa$IRQ13 + dqa$IRQ14 + dqa$IRQ15 + dqa$IRQ16
dqa$IRQ_Total <- dqa$IRQ_NT + dqa$IRQ_NE + dqa$IRQ_PT + dqa$IRQ_PE

# subset questionnaire totals
dqa <- dqa[,-c(2:153)]
colnames(dqa)[1] <- 'subject'


####---- setup task data ----####
# subset task trials
d <- subset(d, d$whichWindow=='faceWindow')

# drop empty variables
d <- d[,-c(1:4,11:17,29:152)]

# reorder columns
colOrder <- c("subject", "subjectOS", "subjectBrowser", "keyPair", "block", "faceList", "emotionRating", "trial", "stimulus", "face", "model", 
              "sex", "expression", "mask", "key_press", "rateEmotion", "rt")
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
