# faceMask study 2 RT analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating surprise
# note: need to have run '1_faceMask2_proc.R' script first
# 11/6/20
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

# flip ratings
d$rateEmotion <- 1 - d$rateEmotion

# subset by emotion rating type
dr <- subset(d, emotionRating=='surprised')

# recode face ratings
dr$rateEmotion[dr$rateEmotion == 1] <- 'correct'
dr$rateEmotion[dr$rateEmotion == 0] <- 'incorrect'
dr$rateEmotion <- as.factor(dr$rateEmotion)
dr$rateEmotion <- relevel(dr$rateEmotion, 'incorrect')

# further subset by facial expression
dra <- subset(dr, expression=='angry')
drd <- subset(dr, expression=='disgusted')
drf <- subset(dr, expression=='fearful')
drs <- subset(dr, expression=='sad')

# further subset for masks of interest and re-level
# angry faces
draxu <- subset(dra, dra$mask != 'lower')
draxu$mask <- factor(draxu$mask, levels=c('none', 'upper'))
draxl <- subset(dra, dra$mask != 'upper')
draxl$mask <- factor(draxl$mask, levels=c('none', 'lower'))
draul <- subset(dra, dra$mask != 'none')
draul$mask <- factor(draul$mask, levels=c('upper', 'lower'))
# disgusted faces
drdxu <- subset(drd, drd$mask != 'lower')
drdxu$mask <- factor(drdxu$mask, levels=c('none', 'upper'))
drdxl <- subset(drd, drd$mask != 'upper')
drdxl$mask <- factor(drdxl$mask, levels=c('none', 'lower'))
drdul <- subset(drd, drd$mask != 'none')
drdul$mask <- factor(drdul$mask, levels=c('upper', 'lower'))
# fearful faces
drfxu <- subset(drf, drf$mask != 'lower')
drfxu$mask <- factor(drfxu$mask, levels=c('none', 'upper'))
drfxl <- subset(drf, drf$mask != 'upper')
drfxl$mask <- factor(drfxl$mask, levels=c('none', 'lower'))
drful <- subset(drf, drf$mask != 'none')
drful$mask <- factor(drful$mask, levels=c('upper', 'lower'))
# sad faces
drsxu <- subset(drs, drs$mask != 'lower')
drsxu$mask <- factor(drsxu$mask, levels=c('none', 'upper'))
drsxl <- subset(drs, drs$mask != 'upper')
drsxl$mask <- factor(drsxl$mask, levels=c('none', 'lower'))
drsul <- subset(drs, drs$mask != 'none')
drsul$mask <- factor(drsul$mask, levels=c('upper', 'lower'))


####---- RTs by emotion ratings * masks: lower mask vs. no mask ----####
## lower-masked angry faces
lmer_draxl_logrt_ratesad_lowerMask <- with(draxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_draxl_logrt_ratesad_lowerMask) # interaction ns
# rated surprised (incorrect)
draxlRateAng <- subset(draxl, draxl$rateEmotion=='incorrect')
lmer_draxlRateAng_logrt_lowerMask <- with(draxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_draxlRateAng_logrt_lowerMask) # ns
# rated not surprised (correct)
draxlRateNotAng <- subset(draxl, draxl$rateEmotion=='correct')
lmer_draxlRateNotAng_logrt_lowerMask <- with(draxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_draxlRateNotAng_logrt_lowerMask) # slower rating angry faces 'not surprised' with lower mask ***

## lower-masked disgusted faces
lmer_drdxl_logrt_ratesad_lowerMask <- with(drdxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drdxl_logrt_ratesad_lowerMask) # interaction ***
# rated surprised (incorrect)
drdxlRateAng <- subset(drdxl, drdxl$rateEmotion=='incorrect')
lmer_drdxlRateAng_logrt_lowerMask <- with(drdxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drdxlRateAng_logrt_lowerMask) # slower rating disgusted faces 'surprised' with lower mask *
# rated not surprised (correct)
drdxlRateNotAng <- subset(drdxl, drdxl$rateEmotion=='correct')
lmer_drdxlRateNotAng_logrt_lowerMask <- with(drdxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drdxlRateNotAng_logrt_lowerMask) # faster rating disgusted faces 'not surprised' with lower mask ***


####---- RTs by emotion ratings * masks: upper mask vs. no mask ----####
## upper-masked fearful faces
lmer_drfxu_logrt_ratesad_upperMask <- with(drfxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drfxu_logrt_ratesad_upperMask) # interaction ***
# rated surprised (incorrect)
drfxuRateAng <- subset(drfxu, drfxu$rateEmotion=='incorrect')
lmer_drfxuRateAng_logrt_upperMask <- with(drfxuRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drfxuRateAng_logrt_upperMask) # slower rating fearful faces 'surprised' with upper mask *
# rated not surprised (correct)
drfxuRateNotAng <- subset(drfxu, drfxu$rateEmotion=='correct')
lmer_drfxuRateNotAng_logrt_upperMask <- with(drfxuRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drfxuRateNotAng_logrt_upperMask) # faster rating fearful faces 'not surprised' with upper mask *

## upper-masked sad faces
lmer_drsxu_logrt_ratesad_upperMask <- with(drsxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drsxu_logrt_ratesad_upperMask) # interaction ns
# rated surprised (incorrect)
drsxuRateAng <- subset(drsxu, drsxu$rateEmotion=='incorrect')
lmer_drsxuRateAng_logrt_upperMask <- with(drsxuRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drsxuRateAng_logrt_upperMask) # ns
# rated not surprised (correct)
drsxuRateNotAng <- subset(drsxu, drsxu$rateEmotion=='correct')
lmer_drsxuRateNotAng_logrt_upperMask <- with(drsxuRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drsxuRateNotAng_logrt_upperMask) # faster rating sad faces 'not surprised' with upper mask *


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## angry faces
lmer_draul_logrt_ratesad_mask <- with(draul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_draul_logrt_ratesad_mask) # interaction ns
# rated surprised (incorrect)
draulRateAng <- subset(draul, draul$rateEmotion=='incorrect')
lmer_draulRateAng_logrt_mask <- with(draulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_draulRateAng_logrt_mask) # ns
# rated not surprised (correct)
draulRateNotAng <- subset(draul, draul$rateEmotion=='correct')
lmer_draulRateNotAng_logrt_mask <- with(draulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_draulRateNotAng_logrt_mask) # slower rating angry faces 'not surprised' with lower vs. upper mask ***

## disgusted faces
lmer_drdul_logrt_ratesad_mask <- with(drdul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drdul_logrt_ratesad_mask) # interaction *
# rated surprised (incorrect)
drdulRateAng <- subset(drdul, drdul$rateEmotion=='incorrect')
lmer_drdulRateAng_logrt_mask <- with(drdulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drdulRateAng_logrt_mask) # ns
# rated not surprised (correct)
drdulRateNotAng <- subset(drdul, drdul$rateEmotion=='correct')
lmer_drdulRateNotAng_logrt_mask <- with(drdulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drdulRateNotAng_logrt_mask) # faster rating disgusted faces 'not surprised' with lower vs. upper mask ***

## fearful faces
lmer_drful_logrt_ratesad_mask <- with(drful, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drful_logrt_ratesad_mask) # interaction ns
# rated surprised (incorrect)
drfulRateAng <- subset(drful, drful$rateEmotion=='incorrect')
lmer_drfulRateAng_logrt_mask <- with(drfulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drfulRateAng_logrt_mask) # faster rating fearful faces 'surprised' with lower vs. upper mask **
# rated not surprised (correct)
drfulRateNotAng <- subset(drful, drful$rateEmotion=='correct')
lmer_drfulRateNotAng_logrt_mask <- with(drfulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drfulRateNotAng_logrt_mask) # ns

## sad faces
lmer_drsul_logrt_ratesad_mask <- with(drsul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drsul_logrt_ratesad_mask) # interaction ns
# rated surprised (incorrect)
drsulRateAng <- subset(drsul, drsul$rateEmotion=='incorrect')
lmer_drsulRateAng_logrt_mask <- with(drsulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drsulRateAng_logrt_mask) # ns
# rated not surprised (correct)
drsulRateNotAng <- subset(drsul, drsul$rateEmotion=='correct')
lmer_drsulRateNotAng_logrt_mask <- with(drsulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drsulRateNotAng_logrt_mask) # slower rating sad faces 'not surprised' with lower vs. upper mask ***


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask ----####
## lower-masked angry faces
# interaction
confint.merMod(lmer_draxl_logrt_ratesad_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.0800517 0.04201484

# rated surprised (incorrect): lower mask vs. no mask
confint.merMod(lmer_draxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.08099713 0.1109805 

# rated not surprised (correct): lower mask vs. no mask
confint.merMod(lmer_draxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04262964 -0.01238467


## lower-masked disgusted faces
# interaction
confint.merMod(lmer_drdxl_logrt_ratesad_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.05629146 0.150448

# rated surprised (incorrect): lower mask vs. no mask
confint.merMod(lmer_drdxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1099529 -0.009672858 

# rated not surprised (correct): lower mask vs. no mask
confint.merMod(lmer_drdxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.03414587 0.07640043


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: upper mask vs. no mask ----####
## lower-masked fearful faces
# interaction
confint.merMod(lmer_drfxu_logrt_ratesad_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper 0.02803666 0.1059476

# rated surprised (incorrect): lower mask vs. no mask
confint.merMod(lmer_drfxuRateAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.05422012 -0.006399415 

# rated not surprised (correct): lower mask vs. no mask
confint.merMod(lmer_drfxuRateNotAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper 0.01012431 0.0709503 


## lower-masked sad faces
# interaction
confint.merMod(lmer_drsxu_logrt_ratesad_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.1025651 0.04047543

# rated surprised (incorrect): lower mask vs. no mask
confint.merMod(lmer_drsxuRateAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.02775704 0.1638929 

# rated not surprised (correct): lower mask vs. no mask
confint.merMod(lmer_drsxuRateNotAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper 0.001958874 0.03157713 


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## angry faces
# interaction
confint.merMod(lmer_draul_logrt_ratesad_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.07837635 0.04750771

# rated surprised (incorrect): lower mask vs. no mask
confint.merMod(lmer_draulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1092429 0.1258029 

# rated not surprised (correct): lower mask vs. no mask
confint.merMod(lmer_draulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05729378 -0.02827881


## disgusted faces
# interaction
confint.merMod(lmer_drdul_logrt_ratesad_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.01430338 0.1005788

# rated surprised (incorrect): lower mask vs. no mask
confint.merMod(lmer_drdulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.08202598 0.02112219 

# rated not surprised (correct): lower mask vs. no mask
confint.merMod(lmer_drdulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.01423435 0.04850777


## fearful faces
# interaction
confint.merMod(lmer_drful_logrt_ratesad_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.06469569 0.0184807

# rated surprised (incorrect): lower mask vs. no mask
confint.merMod(lmer_drfulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.008874831 0.06133094 

# rated not surprised (correct): lower mask vs. no mask
confint.merMod(lmer_drfulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0141397 0.05037386 


## sad faces
# interaction
confint.merMod(lmer_drsul_logrt_ratesad_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.03304728 0.1086287

# rated surprised (incorrect): lower mask vs. no mask
confint.merMod(lmer_drsulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1519134 0.05695997 

# rated not surprised (correct): lower mask vs. no mask
confint.merMod(lmer_drsulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05770929 -0.02584622 



####---- plot RTs by surprised ratings and masks: lower mask vs. no mask ----####
# plot RTs by surprised ratings and lower-masked angry faces
cat_plot(lmer_draxl_logrt_ratesad_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Reds") +
  coord_cartesian(ylim=c(-3.00,-2.85)) +
  theme_minimal(base_size=24)

# plot RTs by surprised ratings and lower-masked disgusted faces
cat_plot(lmer_drdxl_logrt_ratesad_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Oranges") +
  coord_cartesian(ylim=c(-3.05,-2.85)) +
  theme_minimal(base_size=24)


####---- plot RTs by surprised ratings and masks: upper mask vs. no mask ----####
# plot RTs by surprised ratings and upper-masked fearful faces
cat_plot(lmer_drfxu_logrt_ratesad_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Blues") +
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)

# plot RTs by surprised ratings and upper-masked sad faces
cat_plot(lmer_drsxu_logrt_ratesad_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greys") +
  coord_cartesian(ylim=c(-2.95,-2.75)) +
  theme_minimal(base_size=24)


####---- plot RTs by surprised ratings and masks: lower mask vs. upper mask ----####
# plot RTs by surprised ratings and masked angry faces
cat_plot(lmer_draul_logrt_ratesad_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Reds") +
  coord_cartesian(ylim=c(-3.00,-2.85)) +
  theme_minimal(base_size=24)

# plot RTs by surprised ratings and masked disgusted faces
cat_plot(lmer_drdul_logrt_ratesad_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Oranges") +
  coord_cartesian(ylim=c(-3.05,-2.85)) +
  theme_minimal(base_size=24)

# plot RTs by surprised ratings and masked fearful faces
cat_plot(lmer_drful_logrt_ratesad_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Blues") +
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)

# plot RTs by surprised ratings and masked sad faces
cat_plot(lmer_drsul_logrt_ratesad_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greys") +
  coord_cartesian(ylim=c(-2.95,-2.75)) +
  theme_minimal(base_size=24)
