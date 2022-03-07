# funciton to score questionnaires
# ...
#
# example usage:
#
# whichQs <- c('AQ_C', 'BEQ_S', 'BFI', 'INDCOL', 'IRQ')
#
# dqa <- scoreQs(dqa, whichQs)

scoreQs <- function(dataFrame, whichQs) {
  
  # define dataframe with questionnaire responses
  dqa <- dataFrame
  
  ## calculate questionnaire scores
  # NOTE: AQ items were accidentally reverse-coded at data collection (e.g. 0 = "definitely agree" & 3 = "definitely disagree")
  # so leaving the reversed items as they are, and reversing the *positive* items instead
  
  # AQ_C & AQ_SS
  if(('AQ_C' %in% whichQs) & ('AQ_SS' %in% whichQs)) {
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
  }
  
  # AQ_C & *not* AQ_SS
  if(('AQ_C' %in% whichQs) & !('AQ_SS' %in% whichQs)) {
    dqa$AQC1 <- 3- dqa$AQC1
    dqa$AQC3 <- 3- dqa$AQC3
    dqa$AQC4 <- 3- dqa$AQC4
    dqa$AQC7 <- 3- dqa$AQC7
    dqa$AQC8 <- 3- dqa$AQC8
    dqa$AQC10 <- 3- dqa$AQC10
    dqa$AQ_C <- dqa$AQC1 + dqa$AQC2 + dqa$AQC3 + dqa$AQC4 + dqa$AQC5 + dqa$AQC6 + dqa$AQC7 + dqa$AQC8 + dqa$AQC9 + dqa$AQC10
  }
  
  # BEQ_N & BEQ_P & BEQ_S
  if(('BEQ_N' %in% whichQs) & ('BEQ_P' %in% whichQs) & ('BEQ_S' %in% whichQs)) {
    dqa$BEQ3 <- 6 - dqa$BEQ3
    dqa$BEQ8 <- 6 - dqa$BEQ8
    dqa$BEQ9 <- 6 - dqa$BEQ9
    dqa$BEQ_N <- rowMeans(dqa[c('BEQ3', 'BEQ5', 'BEQ8', 'BEQ9', 'BEQ13', 'BEQ16')])
    dqa$BEQ_P <- rowMeans(dqa[c('BEQ1', 'BEQ4', 'BEQ6', 'BEQ10')])
    dqa$BEQ_S <- rowMeans(dqa[c('BEQ2', 'BEQ7', 'BEQ11', 'BEQ12', 'BEQ14', 'BEQ15')])
  }
  
  # BEQ_S & *not* BEQ_N/P
  if(('BEQ_S' %in% whichQs) & !('BEQ_N' %in% whichQs) & !('BEQ_P' %in% whichQs)) {dqa$BEQ_S <- rowMeans(dqa[c('BEQS1', 'BEQS2', 'BEQS3', 'BEQS4', 'BEQS5', 'BEQS6')])}
  
  # BFI
  if('BFI' %in% whichQs) {
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
  }
  
  # INDCOL
  if('INDCOL' %in% whichQs) {
    dqa$INDCOL_HC <- dqa$INDCOL1 + dqa$INDCOL5 + dqa$INDCOL10 + dqa$INDCOL14
    dqa$INDCOL_VC <- dqa$INDCOL2 + dqa$INDCOL3 + dqa$INDCOL7 + dqa$INDCOL12
    dqa$INDCOL_HI <- dqa$INDCOL6 + dqa$INDCOL8 + dqa$INDCOL11
    dqa$INDCOL_VI <- dqa$INDCOL4 + dqa$INDCOL9 + dqa$INDCOL13
    dqa$INDCOL_C <- dqa$INDCOL_HC + dqa$INDCOL_VC
    dqa$INDCOL_I <- dqa$INDCOL_HI + dqa$INDCOL_VI
  }
  
  # IRI
  if('IRI' %in% whichQs) {
    dqa$IRI2 <- 4 - dqa$IRI2
    dqa$IRI3 <- 4 - dqa$IRI3
    dqa$IRI7 <- 4 - dqa$IRI7
    dqa$IRI8 <- 4 - dqa$IRI8
    dqa$IRI9 <- 4 - dqa$IRI9
    dqa$IRI9 <- 4 - dqa$IRI9
    dqa$IRI_EC <- dqa$IRI1 + dqa$IRI3 + dqa$IRI5 + dqa$IRI7 + dqa$IRI9 + dqa$IRI10 + dqa$IRI12
    dqa$IRI_PT <- dqa$IRI2 + dqa$IRI4 + dqa$IRI6 + dqa$IRI8 + dqa$IRI11 + dqa$IRI13 + dqa$IRI14
  }
  
  # IRQ
  if('IRQ' %in% whichQs) {
    dqa$IRQ_NT <- dqa$IRQ1 + dqa$IRQ2 + dqa$IRQ3 + dqa$IRQ4
    dqa$IRQ_NE <- dqa$IRQ5 + dqa$IRQ6 + dqa$IRQ7 + dqa$IRQ8
    dqa$IRQ_PT <- dqa$IRQ9 + dqa$IRQ10 + dqa$IRQ11 + dqa$IRQ12
    dqa$IRQ_PE <- dqa$IRQ13 + dqa$IRQ14 + dqa$IRQ15 + dqa$IRQ16
    dqa$IRQ_Total <- dqa$IRQ_NT + dqa$IRQ_NE + dqa$IRQ_PT + dqa$IRQ_PE
  }
  
  dqa
}
