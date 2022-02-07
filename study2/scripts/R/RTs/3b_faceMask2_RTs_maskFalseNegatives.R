# faceMask study 2 RT analysis script: mask analyses of false negatives (failing to ID correct emotion)
# note: need to have run '1_faceMask2_behav_proc.R' script first
# 11/3/20
library(lme4)
library(lmerTest)
library(ggplot2)
library(RColorBrewer)
library(moments)
library(interactions)

####---- setup ----####
# subset RTs > 100 ms (see Luce, 1986; Whelan, 2008)
d <- subset(d, rt >= 100)

# check RTs for skewness and log transform
hist(d$rt)
skewness(d$rt)
d$logrt <- log10(d$rt)

# flip RTs
d$logrt <- d$logrt * -1

# subset all congruent trials for main effect analyses
dxx <- subset(d, emotionRating==expression)

# recode face ratings
dxx$rateEmotion[dxx$rateEmotion == 1] <- 'correct'
dxx$rateEmotion[dxx$rateEmotion == 0] <- 'incorrect'
dxx$rateEmotion <- as.factor(dxx$rateEmotion)
dxx$rateEmotion <- relevel(dxx$rateEmotion, 'incorrect')

# subset by emotion rating type and expression
daa <- subset(dxx, emotionRating=='angry' & expression=='angry')
ddd <- subset(dxx, emotionRating=='disgusted' & expression=='disgusted')
dff <- subset(dxx, emotionRating=='fearful' & expression=='fearful')
dhh <- subset(dxx, emotionRating=='happy' & expression=='happy')
dss <- subset(dxx, emotionRating=='sad' & expression=='sad')
drr <- subset(dxx, emotionRating=='surprised' & expression=='surprised')

## further subset for masks of interest and re-level
# all ratings (congruent faces)
dxxxu <- subset(dxx, dxx$mask != 'lower')
dxxxu$mask <- factor(dxxxu$mask, levels=c('none', 'upper'))
dxxxl <- subset(dxx, dxx$mask != 'upper')
dxxxl$mask <- factor(dxxxl$mask, levels=c('none', 'lower'))
dxxul <- subset(dxx, dxx$mask != 'none')
dxxul$mask <- factor(dxxul$mask, levels=c('upper', 'lower'))

# angry
daaxu <- subset(daa, daa$mask != 'lower')
daaxu$mask <- factor(daaxu$mask, levels=c('none', 'upper'))
daaxl <- subset(daa, daa$mask != 'upper')
daaxl$mask <- factor(daaxl$mask, levels=c('none', 'lower'))
daaul <- subset(daa, daa$mask != 'none')
daaul$mask <- factor(daaul$mask, levels=c('upper', 'lower'))

# disgusted
dddxl <- subset(ddd, ddd$mask != 'upper')
dddxl$mask <- factor(dddxl$mask, levels=c('none', 'lower'))
dddxu <- subset(ddd, ddd$mask != 'lower')
dddxu$mask <- factor(dddxu$mask, levels=c('none', 'upper'))
dddul <- subset(ddd, ddd$mask != 'none')
dddul$mask <- factor(dddul$mask, levels=c('upper', 'lower'))

# fearful
dffxl <- subset(dff, dff$mask != 'upper')
dffxl$mask <- factor(dffxl$mask, levels=c('none', 'lower'))
dffxu <- subset(dff, dff$mask != 'lower')
dffxu$mask <- factor(dffxu$mask, levels=c('none', 'upper'))
dfful <- subset(dff, dff$mask != 'none')
dfful$mask <- factor(dfful$mask, levels=c('upper', 'lower'))

# happy
dhhxu <- subset(dhh, dhh$mask != 'lower')
dhhxu$mask <- factor(dhhxu$mask, levels=c('none', 'upper'))
dhhxl <- subset(dhh, dhh$mask != 'upper')
dhhxl$mask <- factor(dhhxl$mask, levels=c('none', 'lower'))
dhhul <- subset(dhh, dhh$mask != 'none')
dhhul$mask <- factor(dhhul$mask, levels=c('upper', 'lower'))

# sad
dssxl <- subset(dss, dss$mask != 'upper')
dssxl$mask <- factor(dssxl$mask, levels=c('none', 'lower'))
dssxu <- subset(dss, dss$mask != 'lower')
dssxu$mask <- factor(dssxu$mask, levels=c('none', 'upper'))
dssul <- subset(dss, dss$mask != 'none')
dssul$mask <- factor(dssul$mask, levels=c('upper', 'lower'))

# surprised
drrxl <- subset(drr, drr$mask != 'upper')
drrxl$mask <- factor(drrxl$mask, levels=c('none', 'lower'))
drrxu <- subset(drr, drr$mask != 'lower')
drrxu$mask <- factor(drrxu$mask, levels=c('none', 'upper'))
drrul <- subset(drr, drr$mask != 'none')
drrul$mask <- factor(drrul$mask, levels=c('upper', 'lower'))


####---- RTs by emotion ratings * masks: lower mask vs. no mask ----####
## all ratings (lower-masked congruent faces)
lmer_dxxxl_logrt_rateEmotion_lowerMask <- with(dxxxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dxxxl_logrt_rateEmotion_lowerMask) # interaction ns
# rated correct
dxxxlRateCorr <- subset(dxxxl, dxxxl$rateEmotion=='correct')
lmer_dxxxlRateCorr_logrt_lowerMask <- with(dxxxlRateCorr, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dxxxlRateCorr_logrt_lowerMask) # slower rating faces correctly with lower mask  vs. no mask
# rated incorrect
dxxxlRateIncorr <- subset(dxxxl, dxxxl$rateEmotion=='incorrect')
lmer_dxxxlRateIncorr_logrt_lowerMask <- with(dxxxlRateIncorr, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dxxxlRateIncorr_logrt_lowerMask) # ns

## rating anger (lower-masked angry faces)
lmer_daaxl_logrt_rateAngry_lowerMask <- with(daaxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_daaxl_logrt_rateAngry_lowerMask) # interaction ns
# rated angry
daaxlRateAng <- subset(daaxl, daaxl$rateEmotion=='correct')
lmer_daaxlRateAng_logrt_lowerMask <- with(daaxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_daaxlRateAng_logrt_lowerMask) # ns
# rated not angry
daaxlRateNotAng <- subset(daaxl, daaxl$rateEmotion=='incorrect')
lmer_daaxlRateNotAng_logrt_lowerMask <- with(daaxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_daaxlRateNotAng_logrt_lowerMask) # ns

## rating disgust (lower-masked disgusted faces)
lmer_dddxl_logrt_rateDisgust_lowerMask <- with(dddxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dddxl_logrt_rateDisgust_lowerMask) # interaction *
# rated disgusted
dddxlRateDis <- subset(dddxl, dddxl$rateEmotion=='correct')
lmer_dddxlRateDis_logrt_lowerMask <- with(dddxlRateDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dddxlRateDis_logrt_lowerMask) # slower rating faces 'disgusted' with lower mask vs. no mask
# rated not disgusted
dddxlRateNotDis <- subset(dddxl, dddxl$rateEmotion=='incorrect')
lmer_dddxlRateNotDis_logrt_lowerMask <- with(dddxlRateNotDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dddxlRateNotDis_logrt_lowerMask) # ns

## rating fear (lower-masked fearful faces)
lmer_dffxl_logrt_rateFearful_lowerMask <- with(dffxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dffxl_logrt_rateFearful_lowerMask) # interaction ns
# rated fearful
dffxlRateFear <- subset(dffxl, dffxl$rateEmotion=='correct')
lmer_dffxlRateFear_logrt_lowerMask <- with(dffxlRateFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dffxlRateFear_logrt_lowerMask) # slower rating faces 'fearful' with lower mask vs. no mask
# rated not fearful
dffxlRateNotFear <- subset(dffxl, dffxl$rateEmotion=='incorrect')
lmer_dffxlRateNotFear_logrt_lowerMask <- with(dffxlRateNotFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dffxlRateNotFear_logrt_lowerMask) # ns

## rating happiness (lower-masked happy faces)
lmer_dhhxl_logrt_rateHappy_lowerMask <- with(dhhxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dhhxl_logrt_rateHappy_lowerMask) # interaction *
# rated happy
dhhxlRateHappy <- subset(dhhxl, dhhxl$rateEmotion=='correct')
lmer_dhhxlRateHappy_logrt_lowerMask <- with(dhhxlRateHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhhxlRateHappy_logrt_lowerMask) # slower rating faces 'happy' with lower mask vs. no mask
# rated not happy
dhhxlRateNotHappy <- subset(dhhxl, dhhxl$rateEmotion=='incorrect')
lmer_dhhxlRateNotHappy_logrt_lowerMask <- with(dhhxlRateNotHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhhxlRateNotHappy_logrt_lowerMask) # ns

## rating sadness (lower-masked sad faces)
lmer_dssxl_logrt_rateSad_lowerMask <- with(dssxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dssxl_logrt_rateSad_lowerMask) # interaction ns
# rated sad
dssxlRateSad <- subset(dssxl, dssxl$rateEmotion=='correct')
lmer_dssxlRateSad_logrt_lowerMask <- with(dssxlRateSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dssxlRateSad_logrt_lowerMask) # slower rating faces 'sad' with lower mask vs. no mask
# rated not sad
dssxlRateNotSad <- subset(dssxl, dssxl$rateEmotion=='incorrect')
lmer_dssxlRateNotSad_logrt_lowerMask <- with(dssxlRateNotSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dssxlRateNotSad_logrt_lowerMask) # ns

## rating surprise (lower-masked surprised faces)
lmer_drrxl_logrt_rateSurprise_lowerMask <- with(drrxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drrxl_logrt_rateSurprise_lowerMask) # interaction ns
# rated surprised
drrxlRateSur <- subset(drrxl, drrxl$rateEmotion=='correct')
lmer_drrxlRateSur_logrt_lowerMask <- with(drrxlRateSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drrxlRateSur_logrt_lowerMask) # slower rating faces 'surprised' with lower mask vs. no mask
# rated not surprised
drrxlRateNotSur <- subset(drrxl, drrxl$rateEmotion=='incorrect')
lmer_drrxlRateNotSur_logrt_lowerMask <- with(drrxlRateNotSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drrxlRateNotSur_logrt_lowerMask) # ns


####---- RTs by emotion ratings * masks: upper mask vs. no mask ----####
## all ratings (upper-masked congruent faces)
lmer_dxxxu_logrt_rateEmotion_upperMask <- with(dxxxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dxxxu_logrt_rateEmotion_upperMask) # interaction ns
# rated correct
dxxxuRateCorr <- subset(dxxxu, dxxxu$rateEmotion=='correct')
lmer_dxxxuRateCorr_logrt_upperMask <- with(dxxxuRateCorr, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dxxxuRateCorr_logrt_upperMask) # slower rating faces correctly with upper mask vs. no mask
# rated incorrect
dxxxuRateIncorr <- subset(dxxxu, dxxxu$rateEmotion=='incorrect')
lmer_dxxxuRateIncorr_logrt_upperMask <- with(dxxxuRateIncorr, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dxxxuRateIncorr_logrt_upperMask) # ns

## rating anger (upper-masked angry faces)
lmer_daaxu_logrt_rateAngry_upperMask <- with(daaxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_daaxu_logrt_rateAngry_upperMask) # interaction ns
# rated angry
daaxuRateAng <- subset(daaxu, daaxu$rateEmotion=='correct')
lmer_daaxuRateAng_logrt_upperMask <- with(daaxuRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_daaxuRateAng_logrt_upperMask) # slower rating faces 'angry' with upper mask vs. no mask
# rated not angry
daaxuRateNotAng <- subset(daaxu, daaxu$rateEmotion=='incorrect')
lmer_daaxuRateNotAng_logrt_upperMask <- with(daaxuRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_daaxuRateNotAng_logrt_upperMask) # ns

## rating disgust (upper-masked disgusted faces)
lmer_dddxu_logrt_rateDisgust_upperMask <- with(dddxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dddxu_logrt_rateDisgust_upperMask) # interaction ns
# rated disgusted
dddxuRateDis <- subset(dddxu, dddxu$rateEmotion=='correct')
lmer_dddxuRateDis_logrt_upperMask <- with(dddxuRateDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dddxuRateDis_logrt_upperMask) # ns
# rated not disgusted
dddxuRateNotDis <- subset(dddxu, dddxu$rateEmotion=='incorrect')
lmer_dddxuRateNotDis_logrt_upperMask <- with(dddxuRateNotDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dddxuRateNotDis_logrt_upperMask) # ns

## rating fear (upper-masked fearful faces)
lmer_dffxu_logrt_rateFearful_upperMask <- with(dffxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dffxu_logrt_rateFearful_upperMask) # interaction ns
# rated fearful
dffxuRateFear <- subset(dffxu, dffxu$rateEmotion=='correct')
lmer_dffxuRateFear_logrt_upperMask <- with(dffxuRateFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dffxuRateFear_logrt_upperMask) # slower rating faces 'fearful' with upper mask vs. no mask
# rated not fearful
dffxuRateNotFear <- subset(dffxu, dffxu$rateEmotion=='incorrect')
lmer_dffxuRateNotFear_logrt_upperMask <- with(dffxuRateNotFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dffxuRateNotFear_logrt_upperMask) # ns

## rating happiness (upper-masked happy faces)
lmer_dhhxu_logrt_rateHappy_upperMask <- with(dhhxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dhhxu_logrt_rateHappy_upperMask) # interaction ns
# rated happy
dhhxuRateHappy <- subset(dhhxu, dhhxu$rateEmotion=='correct')
lmer_dhhxuRateHappy_logrt_upperMask <- with(dhhxuRateHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhhxuRateHappy_logrt_upperMask) # slower rating faces 'happy' with upper mask vs. no mask
# rated not happy
dhhxuRateNotHappy <- subset(dhhxu, dhhxu$rateEmotion=='incorrect')
lmer_dhhxuRateNotHappy_logrt_upperMask <- with(dhhxuRateNotHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhhxuRateNotHappy_logrt_upperMask) # ns

## rating sadness (upper-masked sad faces)
lmer_dssxu_logrt_rateSad_upperMask <- with(dssxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dssxu_logrt_rateSad_upperMask) # interaction ns
# rated sad
dssxuRateSad <- subset(dssxu, dssxu$rateEmotion=='correct')
lmer_dssxuRateSad_logrt_upperMask <- with(dssxuRateSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dssxuRateSad_logrt_upperMask) # ns
# rated not sad
dssxuRateNotSad <- subset(dssxu, dssxu$rateEmotion=='incorrect')
lmer_dssxuRateNotSad_logrt_upperMask <- with(dssxuRateNotSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dssxuRateNotSad_logrt_upperMask) # ns

## rating surprise (upper-masked surprised faces)
lmer_drrxu_logrt_rateSurprise_upperMask <- with(drrxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drrxu_logrt_rateSurprise_upperMask) # interaction ns
# rated surprised
drrxuRateSur <- subset(drrxu, drrxu$rateEmotion=='correct')
lmer_drrxuRateSur_logrt_upperMask <- with(drrxuRateSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drrxuRateSur_logrt_upperMask) # ns
# rated not surprised
drrxuRateNotSur <- subset(drrxu, drrxu$rateEmotion=='incorrect')
lmer_drrxuRateNotSur_logrt_upperMask <- with(drrxuRateNotSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drrxuRateNotSur_logrt_upperMask) # ns


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## all ratings (congruent faces)
lmer_dxxul_logrt_rateEmotion_lowerMask <- with(dxxul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dxxul_logrt_rateEmotion_lowerMask) # interaction ns
# rated correct
dxxulRateCorr <- subset(dxxul, dxxul$rateEmotion=='correct')
lmer_dxxulRateCorr_logrt_lowerMask <- with(dxxulRateCorr, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dxxulRateCorr_logrt_lowerMask) # slower rating faces correctly with lower mask vs. upper mask
# rated incorrect
dxxulRateIncorr <- subset(dxxul, dxxul$rateEmotion=='incorrect')
lmer_dxxulRateIncorr_logrt_lowerMask <- with(dxxulRateIncorr, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dxxulRateIncorr_logrt_lowerMask) # ns

## rating anger (angry faces)
lmer_daaul_logrt_rateAngry_lowerMask <- with(daaul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_daaul_logrt_rateAngry_lowerMask) # interaction **
# rated angry
daaulRateAng <- subset(daaul, daaul$rateEmotion=='correct')
lmer_daaulRateAng_logrt_lowerMask <- with(daaulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_daaulRateAng_logrt_lowerMask) # *faster* rating faces correctly with lower mask vs. upper mask
# rated not angry
daaulRateNotAng <- subset(daaul, daaul$rateEmotion=='incorrect')
lmer_daaulRateNotAng_logrt_lowerMask <- with(daaulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_daaulRateNotAng_logrt_lowerMask) # ns

## rating disgust (disgusted faces)
lmer_dddul_logrt_rateDisgust_lowerMask <- with(dddul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dddul_logrt_rateDisgust_lowerMask) # interaction ***
# rated disgusted
dddulRateDis <- subset(dddul, dddul$rateEmotion=='correct')
lmer_dddulRateDis_logrt_lowerMask <- with(dddulRateDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dddulRateDis_logrt_lowerMask) # slower rating faces 'disgusted' with lower mask vs. upper mask
# rated not disgusted
dddulRateNotDis <- subset(dddul, dddul$rateEmotion=='incorrect')
lmer_dddulRateNotDis_logrt_lowerMask <- with(dddulRateNotDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dddulRateNotDis_logrt_lowerMask) # ns

## rating fear (fearful faces)
lmer_dfful_logrt_rateFearful_lowerMask <- with(dfful, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfful_logrt_rateFearful_lowerMask) # interaction *
# rated fearful
dffulRateFear <- subset(dfful, dfful$rateEmotion=='correct')
lmer_dffulRateFear_logrt_lowerMask <- with(dffulRateFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dffulRateFear_logrt_lowerMask) # *faster* rating faces fearful with lower mask vs. upper mask
# rated not fearful
dffulRateNotFear <- subset(dfful, dfful$rateEmotion=='incorrect')
lmer_dffulRateNotFear_logrt_lowerMask <- with(dffulRateNotFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dffulRateNotFear_logrt_lowerMask) # ns

## rating happiness (happy faces)
lmer_dhhul_logrt_rateHappy_lowerMask <- with(dhhul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dhhul_logrt_rateHappy_lowerMask) # interaction ns
# rated happy
dhhulRateHappy <- subset(dhhul, dhhul$rateEmotion=='correct')
lmer_dhhulRateHappy_logrt_lowerMask <- with(dhhulRateHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhhulRateHappy_logrt_lowerMask) # slower rating faces 'happy' with lower mask vs. upper mask
# rated not happy
dhhulRateNotHappy <- subset(dhhul, dhhul$rateEmotion=='incorrect')
lmer_dhhulRateNotHappy_logrt_lowerMask <- with(dhhulRateNotHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhhulRateNotHappy_logrt_lowerMask) # ns

## rating sadness (sad faces)
lmer_dssul_logrt_rateSad_lowerMask <- with(dssul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dssul_logrt_rateSad_lowerMask) #interaction ns
# rated sad
dssulRateSad <- subset(dssul, dssul$rateEmotion=='correct')
lmer_dssulRateSad_logrt_lowerMask <- with(dssulRateSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dssulRateSad_logrt_lowerMask) # slower rating faces 'sad' with lower mask vs. upper mask
# rated not sad
dssulRateNotSad <- subset(dssul, dssul$rateEmotion=='incorrect')
lmer_dssulRateNotSad_logrt_lowerMask <- with(dssulRateNotSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dssulRateNotSad_logrt_lowerMask) # ns

## rating surprise (surprised faces)
lmer_drrul_logrt_rateSurprise_lowerMask <- with(drrul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drrul_logrt_rateSurprise_lowerMask) #  interaction ns
# rated surprised
drrulRateSur <- subset(drrul, drrul$rateEmotion=='correct')
lmer_drrulRateSur_logrt_lowerMask <- with(drrulRateSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drrulRateSur_logrt_lowerMask) # ns
# rated not surprised
drrulRateNotSur <- subset(drrul, drrul$rateEmotion=='incorrect')
lmer_drrulRateNotSur_logrt_lowerMask <- with(drrulRateNotSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drrulRateNotSur_logrt_lowerMask) # ns


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask ----####
## all ratings (lower-masked congruent faces)
# interaction
confint.merMod(lmer_dxxxl_logrt_rateEmotion_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.02856931 0.009360802

# rated correct: lower mask vs. no mask
confint.merMod(lmer_dxxxlRateCorr_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04004998 -0.02438459

# rated incorrect: lower mask vs. no mask
confint.merMod(lmer_dxxxlRateIncorr_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03683998 0.003944783


## rating anger (lower-masked angry faces)
# interaction
confint.merMod(lmer_daaxl_logrt_rateAngry_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.01205681 0.1033555

# rated angry (correct): lower mask vs. no mask
confint.merMod(lmer_daaxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.001125442 0.03616256

# rated not angry (incorrect): lower mask vs. no mask
confint.merMod(lmer_daaxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1239294 0.0264012


## rating disgust (lower-masked disgusted faces)
# interaction
confint.merMod(lmer_dddxl_logrt_rateDisgust_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotiondisgusted:masklower -0.08979781 -0.003308395

# rated disgusted (correct): lower mask vs. no mask
confint.merMod(lmer_dddxlRateDis_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0906334 -0.03924992

# rated not disgusted (incorrect): lower mask vs. no mask
confint.merMod(lmer_dddxlRateNotDis_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04912885 0.01992983


## rating fear (lower-masked fearful faces)
# interaction
confint.merMod(lmer_dffxl_logrt_rateFearful_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.02060486 0.06126168

# rated fearful (correct): lower mask vs. no mask
confint.merMod(lmer_dffxlRateFear_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03339187 -0.002305339

# rated not fearful (incorrect): lower mask vs. no mask
confint.merMod(lmer_dffxlRateNotFear_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.08790824 0.02126787


## rating happiness (lower-masked happy faces)
# interaction
confint.merMod(lmer_dhhxl_logrt_rateHappy_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.09333855 -0.004912308

# rated happy (correct): lower mask vs. no mask
confint.merMod(lmer_dhhxlRateHappy_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0860888 -0.05798412

# rated not happy (incorrect): lower mask vs. no mask
confint.merMod(lmer_dhhxlRateNotHappy_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05982504 0.02558458


## rating sadness (lower-masked sad faces)
# interaction
confint.merMod(lmer_dssxl_logrt_rateSad_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.07490957 0.01154658

# rated sad (correct): lower mask vs. no mask
confint.merMod(lmer_dssxlRateSad_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.07943852 -0.0435491

# rated not sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dssxlRateNotSad_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.08432682 0.01344246


## rating surprise (lower-masked surprised faces)
# interaction
confint.merMod(lmer_drrxl_logrt_rateSurprise_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.05700408 0.02575564

# rated surprised (correct): lower mask vs. no mask
confint.merMod(lmer_drrxlRateSur_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# -0.0438138 -0.008780593

# rated not surprised (incorrect): lower mask vs. no mask
confint.merMod(lmer_drrxlRateNotSur_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.07103902 0.02871056


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: upper mask vs. no mask ----####
## all ratings (upper-masked congruent faces)
# interaction
confint.merMod(lmer_dxxxu_logrt_rateEmotion_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.02685705 0.01011543

# rated correct: upper mask vs. no mask
confint.merMod(lmer_dxxxuRateCorr_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.02820268 -0.01366808

# rated incorrect: upper mask vs. no mask
confint.merMod(lmer_dxxxuRateIncorr_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.02723869 0.01354636


## rating anger (upper-masked angry faces)
# interaction
confint.merMod(lmer_daaxu_logrt_rateAngry_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.06431085 0.03456965 

# rated angry (correct): upper mask vs. no mask
confint.merMod(lmer_daaxuRateAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.05691565 -0.01530133

# rated not angry (incorrect): upper mask vs. no mask
confint.merMod(lmer_daaxuRateNotAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.08344627 0.01301053


## rating disgust (upper-masked disgusted faces)
# interaction
confint.merMod(lmer_dddxu_logrt_rateDisgust_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.004560878 0.07509465

# rated disgusted (correct): upper mask vs. no mask
confint.merMod(lmer_dddxuRateDis_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.01137581 0.02078532

# rated not disgusted (incorrect): upper mask vs. no mask
confint.merMod(lmer_dddxuRateNotDis_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.08031284 0.008209966


## rating fear (upper-masked fearful faces)
# interaction
confint.merMod(lmer_dffxu_logrt_rateFearful_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.06531735 0.02155194

# rated fearful (correct): upper mask vs. no mask
confint.merMod(lmer_dffxuRateFear_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.07777435 -0.04291024

# rated not fearful (incorrect): upper mask vs. no mask
confint.merMod(lmer_dffxuRateNotFear_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.06439816 0.01763527


## rating happiness (upper-masked happy faces)
# interaction
confint.merMod(lmer_dhhxu_logrt_rateHappy_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.08318784 0.007200982

# rated happy (correct): upper mask vs. no mask
confint.merMod(lmer_dhhxuRateHappy_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.04327469 -0.01531584

# rated not happy (incorrect): upper mask vs. no mask
confint.merMod(lmer_dhhxuRateNotHappy_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.05611052 0.03973486


## rating sadness (upper-masked sad faces)
# interaction
confint.merMod(lmer_dssxu_logrt_rateSad_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.08012526 0.01158506

# rated sad (correct): upper mask vs. no mask
confint.merMod(lmer_dssxuRateSad_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.02885248 0.001417261

# rated not sad (incorrect): upper mask vs. no mask
confint.merMod(lmer_dssxuRateNotSad_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.02240168 0.08844534


## rating surprise (upper-masked surprised faces)
# interaction
confint.merMod(lmer_drrxu_logrt_rateSurprise_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.07728782 0.01050179

# rated surprised (correct): upper mask vs. no mask
confint.merMod(lmer_drrxuRateSur_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.02886352 0.002242112

# rated not surprised (incorrect): upper mask vs. no mask
confint.merMod(lmer_drrxuRateNotSur_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.03904668 0.06937391


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## all ratings (lower-masked congruent faces)
# interaction
confint.merMod(lmer_dxxul_logrt_rateEmotion_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.01839268 0.01555471

# rated correct: lower mask vs. upper mask
confint.merMod(lmer_dxxulRateCorr_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.01863227 -0.002979642 

# rated incorrect: lower mask vs. upper mask
confint.merMod(lmer_dxxulRateIncorr_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0226335 0.008685812 


## rating anger (lower-masked angry faces)
# interaction
confint.merMod(lmer_daaul_logrt_rateAngry_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.01374969 0.1015077

# rated angry (correct): lower mask vs. upper mask
confint.merMod(lmer_daaulRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.03828693 0.0733122

# rated not angry (incorrect): lower mask vs. upper mask
confint.merMod(lmer_daaulRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04137986 0.05651293 


## rating disgust (lower-masked disgusted faces)
# interaction
confint.merMod(lmer_dddul_logrt_rateDisgust_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.1359174 -0.04809468

# rated disgusted (correct): lower mask vs. upper mask
confint.merMod(lmer_dddulRateDis_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1022499 -0.05199326 

# rated not disgusted (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dddulRateNotDis_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.01941898 0.04446378 


## rating fear (lower-masked fearful faces)
# interaction
confint.merMod(lmer_dfful_logrt_rateFearful_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.005209751 0.08371327

# rated fearful (correct): lower mask vs. upper mask
confint.merMod(lmer_dffulRateFear_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.02547086 0.06265995 

# rated not fearful (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dffulRateNotFear_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04764467 0.03522253 


## rating happiness (lower-masked happy faces)
# interaction
confint.merMod(lmer_dhhul_logrt_rateHappy_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.04921224 0.03980912

# rated happy (correct): lower mask vs. upper mask
confint.merMod(lmer_dhhulRateHappy_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05718838 -0.02302835 

# rated not happy (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dhhulRateNotHappy_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.07780429 0.01311608 


## rating sadness (lower-masked sad faces)
# interaction
confint.merMod(lmer_dssul_logrt_rateSad_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.04810527 0.03528957 

# rated sad (correct): lower mask vs. upper mask
confint.merMod(lmer_dssulRateSad_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.06443144 -0.02497338 

# rated not sad (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dssulRateNotSad_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.08340995 -0.0007313328 


## rating surprise (lower-masked surprised faces)
# interaction
confint.merMod(lmer_drrul_logrt_rateSurprise_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.0329722 0.04254052

# rated surprised (correct): lower mask vs. upper mask
confint.merMod(lmer_drrulRateSur_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03294418 0.002687326 

# rated not surprised (incorrect): lower mask vs. upper mask
confint.merMod(lmer_drrulRateNotSur_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0686664 0.01468291


####---- plot RTs by emotion ratings and masks: lower mask vs. no mask ----####
# plot all rating RTs by correct ratings and lower-mask
my_palette <- brewer.pal(9, "Greys")[c(6,9)]
cat_plot(lmer_dxxxl_logrt_rateEmotion_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = my_palette) + 
  coord_cartesian(ylim=c(-3.00,-2.85)) + 
  theme_minimal(base_size=24)

# plot angry rating RTs by angry ratings and lower-mask
cat_plot(lmer_daaxl_logrt_rateAngry_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Reds") + 
  coord_cartesian(ylim=c(-3.00,-2.85)) + 
  theme_minimal(base_size=24)

# plot disgust rating RTs by disgust ratings and lower-mask
cat_plot(lmer_dddxl_logrt_rateDisgust_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Oranges") + 
  coord_cartesian(ylim=c(-3.05,-2.85)) + 
  theme_minimal(base_size=24)

# plot fearful rating RTs by fearful ratings and lower-mask
cat_plot(lmer_dffxl_logrt_rateFearful_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Blues") + 
  coord_cartesian(ylim=c(-3.00,-2.85)) + 
  theme_minimal(base_size=24)

# plot happy rating RTs by happy ratings and lower-mask
cat_plot(lmer_dhhxl_logrt_rateHappy_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greens") + 
  coord_cartesian(ylim=c(-2.95,-2.75)) + 
  theme_minimal(base_size=24)

# plot sad rating RTs by sad ratings and lower-mask
cat_plot(lmer_dssxl_logrt_rateSad_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greys") + 
  coord_cartesian(ylim=c(-3.05,-2.85)) + 
  theme_minimal(base_size=24)

# plot surprised rating RTs by surprise ratings and lower-mask
cat_plot(lmer_drrxl_logrt_rateSurprise_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Purples") + 
  coord_cartesian(ylim=c(-3.00,-2.85)) + 
  theme_minimal(base_size=24)


####---- plot RTs by emotion ratings and masks: upper mask vs. no mask ----####
# plot all rating RTs by correct ratings and upper-mask
my_palette <- brewer.pal(9, "Greys")[c(6,9)]
cat_plot(lmer_dxxxu_logrt_rateEmotion_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = my_palette) + 
  coord_cartesian(ylim=c(-3.00,-2.85)) + 
  theme_minimal(base_size=24)

# plot angry rating RTs by angry ratings and upper-mask
cat_plot(lmer_daaxu_logrt_rateAngry_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Reds") + 
  coord_cartesian(ylim=c(-3.05,-2.85)) + 
  theme_minimal(base_size=24)

# plot disgust rating RTs by disgust ratings and upper-mask
cat_plot(lmer_dddxu_logrt_rateDisgust_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Oranges") + 
  coord_cartesian(ylim=c(-3.00,-2.85)) + 
  theme_minimal(base_size=24)

# plot fearful rating RTs by fearful ratings and upper-mask
cat_plot(lmer_dffxu_logrt_rateFearful_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Blues") + 
  coord_cartesian(ylim=c(-3.05,-2.85)) + 
  theme_minimal(base_size=24)

# plot happy rating RTs by happy ratings and upper-mask
cat_plot(lmer_dhhxu_logrt_rateHappy_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greens") + 
  coord_cartesian(ylim=c(-2.95,-2.75)) + 
  theme_minimal(base_size=24)

# plot sad rating RTs by sad ratings and upper-mask
cat_plot(lmer_dssxu_logrt_rateSad_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greys") + 
  coord_cartesian(ylim=c(-3.00,-2.85)) + 
  theme_minimal(base_size=24)

# plot surprised rating RTs by surprise ratings and upper-mask
cat_plot(lmer_drrxu_logrt_rateSurprise_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Purples") + 
  coord_cartesian(ylim=c(-3.00,-2.80)) + 
  theme_minimal(base_size=24)


####---- plot RTs by emotion ratings and masks: lower mask vs. upper mask ----####
# plot all rating RTs by correct ratings and lower-mask
my_palette <- brewer.pal(9, "Greys")[c(6,9)]
cat_plot(lmer_dxxul_logrt_rateEmotion_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = my_palette) + coord_cartesian(ylim=c(-3.00,-2.85)) + 
  theme_minimal(base_size=24)

# plot angry rating RTs by angry ratings and lower-mask
cat_plot(lmer_daaul_logrt_rateAngry_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Reds") + coord_cartesian(ylim=c(-3.05,-2.85)) + 
  theme_minimal(base_size=24)

# plot disgust rating RTs by disgust ratings and lower-mask
cat_plot(lmer_dddul_logrt_rateDisgust_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Oranges") + coord_cartesian(ylim=c(-3.05,-2.85)) + 
  theme_minimal(base_size=24)

# plot fearful rating RTs by fearful ratings and lower-mask
cat_plot(lmer_dfful_logrt_rateFearful_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Blues") + coord_cartesian(ylim=c(-3.05,-2.90)) + 
  theme_minimal(base_size=24)

# plot happy rating RTs by happy ratings and lower-mask
cat_plot(lmer_dhhul_logrt_rateHappy_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greens") + coord_cartesian(ylim=c(-3.00,-2.75)) + 
  theme_minimal(base_size=24)

# plot sad rating RTs by sad ratings and lower-mask
cat_plot(lmer_dssul_logrt_rateSad_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greys") + coord_cartesian(ylim=c(-3.05,-2.85)) + 
  theme_minimal(base_size=24)

# plot surprised rating RTs by surprise ratings and lower-mask
cat_plot(lmer_drrul_logrt_rateSurprise_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Purples") + coord_cartesian(ylim=c(-3.00,-2.85)) + 
  theme_minimal(base_size=24)
