# faceMask study 2 RT analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating anger
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
da <- subset(d, emotionRating=='angry')

# recode face ratings
da$rateEmotion[da$rateEmotion == 1] <- 'correct'
da$rateEmotion[da$rateEmotion == 0] <- 'incorrect'
da$rateEmotion <- as.factor(da$rateEmotion)
da$rateEmotion <- relevel(da$rateEmotion, 'incorrect')

# further subset by facial expression
dad <- subset(da, expression=='disgusted')
daf <- subset(da, expression=='fearful')
dah <- subset(da, expression=='happy')
das <- subset(da, expression=='sad')
dar <- subset(da, expression=='surprised')

# further subset for masks of interest and re-level
# disgusted faces
dadxu <- subset(dad, dad$mask != 'lower')
dadxu$mask <- factor(dadxu$mask, levels=c('none', 'upper'))
dadxl <- subset(dad, dad$mask != 'upper')
dadxl$mask <- factor(dadxl$mask, levels=c('none', 'lower'))
dadul <- subset(dad, dad$mask != 'none')
dadul$mask <- factor(dadul$mask, levels=c('upper', 'lower'))
# fearful faces
dafxu <- subset(daf, daf$mask != 'lower')
dafxu$mask <- factor(dafxu$mask, levels=c('none', 'upper'))
dafxl <- subset(daf, daf$mask != 'upper')
dafxl$mask <- factor(dafxl$mask, levels=c('none', 'lower'))
daful <- subset(daf, daf$mask != 'none')
daful$mask <- factor(daful$mask, levels=c('upper', 'lower'))
# happy faces
dahxu <- subset(dah, dah$mask != 'lower')
dahxu$mask <- factor(dahxu$mask, levels=c('none', 'upper'))
dahxl <- subset(dah, dah$mask != 'upper')
dahxl$mask <- factor(dahxl$mask, levels=c('none', 'lower'))
dahul <- subset(dah, dah$mask != 'none')
dahul$mask <- factor(dahul$mask, levels=c('upper', 'lower'))
# sad faces
dasxu <- subset(das, das$mask != 'lower')
dasxu$mask <- factor(dasxu$mask, levels=c('none', 'upper'))
dasxl <- subset(das, das$mask != 'upper')
dasxl$mask <- factor(dasxl$mask, levels=c('none', 'lower'))
dasul <- subset(das, das$mask != 'none')
dasul$mask <- factor(dasul$mask, levels=c('upper', 'lower'))
# surprised faces
darxu <- subset(dar, dar$mask != 'lower')
darxu$mask <- factor(darxu$mask, levels=c('none', 'upper'))
darxl <- subset(dar, dar$mask != 'upper')
darxl$mask <- factor(darxl$mask, levels=c('none', 'lower'))
darul <- subset(dar, dar$mask != 'none')
darul$mask <- factor(darul$mask, levels=c('upper', 'lower'))


####---- RTs by emotion ratings * masks: lower mask vs. no mask ----####
## lower-masked disgusted faces
lmer_dadxl_logrt_rateAngry_lowerMask <- with(dadxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dadxl_logrt_rateAngry_lowerMask) # interaction **
# rated angry (incorrect)
dadxlRateAng <- subset(dadxl, dadxl$rateEmotion=='incorrect')
lmer_dadxlRateAng_logrt_lowerMask <- with(dadxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dadxlRateAng_logrt_lowerMask) # faster rating disgusted faces 'angry' with lower mask
# rated not angry (correct)
dadxlRateNotAng <- subset(dadxl, dadxl$rateEmotion=='correct')
lmer_dadxlRateNotAng_logrt_lowerMask <- with(dadxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dadxlRateNotAng_logrt_lowerMask) # ns

## lower-masked fearful faces
lmer_dafxl_logrt_rateAngry_lowerMask <- with(dafxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dafxl_logrt_rateAngry_lowerMask) # interaction ns
# rated angry (incorrect)
dafxlRateAng <- subset(dafxl, dafxl$rateEmotion=='incorrect')
lmer_dafxlRateAng_logrt_lowerMask <- with(dafxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dafxlRateAng_logrt_lowerMask) # ns
# rated not angry (correct)
dafxlRateNotAng <- subset(dafxl, dafxl$rateEmotion=='correct')
lmer_dafxlRateNotAng_logrt_lowerMask <- with(dafxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dafxlRateNotAng_logrt_lowerMask) # ns

## lower-masked surprised faces
lmer_darxl_logrt_rateAngry_lowerMask <- with(darxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_darxl_logrt_rateAngry_lowerMask) # interaction ns
# rated angry (incorrect)
darxlRateAng <- subset(darxl, darxl$rateEmotion=='incorrect')
lmer_darxlRateAng_logrt_lowerMask <- with(darxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_darxlRateAng_logrt_lowerMask) # ns
# rated not angry (correct)
darxlRateNotAng <- subset(darxl, darxl$rateEmotion=='correct')
lmer_darxlRateNotAng_logrt_lowerMask <- with(darxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_darxlRateNotAng_logrt_lowerMask) # slower rating surprised faces 'not angry' with lower mask


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## disgusted faces
lmer_dadul_logrt_rateAngry_mask <- with(dadul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dadul_logrt_rateAngry_mask) # interaction *
# rated angry (incorrect)
darulRateAng <- subset(dadul, dadul$rateEmotion=='incorrect')
lmer_dadulRateAng_logrt_mask <- with(darulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dadulRateAng_logrt_mask) # ns
# rated not angry (correct)
darulRateNotAng <- subset(dadul, dadul$rateEmotion=='correct')
lmer_dadulRateNotAng_logrt_mask <- with(darulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dadulRateNotAng_logrt_mask) # slower rating disgusted faces 'not angry' with lower mask vs. upper mask **
 
## happy faces
lmer_dahul_logrt_rateAngry_mask <- with(dahul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dahul_logrt_rateAngry_mask) # ns
# rated angry
dahulRateAng <- subset(dahul, dahul$rateEmotion=='incorrect')
lmer_dahulRateAng_logrt_mask <- with(dahulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dahulRateAng_logrt_mask) # ns
# rated not angry
dahulRateNotAng <- subset(dahul, dahul$rateEmotion=='correct')
lmer_dahulRateNotAng_logrt_mask <- with(dahulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dahulRateNotAng_logrt_mask) # slower rating happy faces 'not angry' with lower vs. upper mask **

## surprised faces
lmer_darul_logrt_rateAngry_mask <- with(darul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_darul_logrt_rateAngry_mask) # ns
# rated angry (incorrect)
darulRateAng <- subset(darul, darul$rateEmotion=='incorrect')
lmer_darulRateAng_logrt_mask <- with(darulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_darulRateAng_logrt_mask) # ns
# rated not angry (correct)
darulRateNotAng <- subset(darul, darul$rateEmotion=='correct')
lmer_darulRateNotAng_logrt_mask <- with(darulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_darulRateNotAng_logrt_mask) # slower rating surprised faces 'not angry' with lower vs. upper mask *


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask ----####
## lower-masked disgusted faces
# interaction
confint.merMod(lmer_dadxl_logrt_rateAngry_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.0948659 -0.01727344

# rated disgusted (incorrect): lower mask vs. no mask
confint.merMod(lmer_dadxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.01439217 0.06879178 

# rated not disgusted (correct): lower mask vs. no mask
confint.merMod(lmer_dadxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04963511 0.005417594 


## lower-masked fearful faces
# interaction
confint.merMod(lmer_dafxl_logrt_rateAngry_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.06283142 0.05214169

# rated angry (incorrect): lower mask vs. no mask
confint.merMod(lmer_dafxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.06813728 0.07319566  

# rated not angry (correct): lower mask vs. no mask
confint.merMod(lmer_dafxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.02749512 0.01017225


## lower-masked surprised faces
# interaction
confint.merMod(lmer_darxl_logrt_rateAngry_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.07460483 0.0577241

# rated angry (incorrect): lower mask vs. no mask
confint.merMod(lmer_darxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1122722 0.04836442 

# rated not angry (correct): lower mask vs. no mask
confint.merMod(lmer_darxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04351544 -0.009416122 


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## masked disgusted faces
# interaction
confint.merMod(lmer_dadul_logrt_rateAngry_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.08808187 -0.007922732

# rated angry (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dadulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.01819699 0.04105021

# rated not angry (correct): lower mask vs. upper mask
confint.merMod(lmer_dadulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.07305972 -0.01882875


## masked happy faces
# interaction
confint.merMod(lmer_dahul_logrt_rateAngry_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.05133621 0.1053192

# rated angry (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dahulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.2345237 0.1409242

# rated not angry (correct): lower mask vs. upper mask
confint.merMod(lmer_dahulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03371453 -0.006127829


## masked surprised faces
# interaction
confint.merMod(lmer_darul_logrt_rateAngry_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.05406165 0.09654517

# rated angry (incorrect): lower mask vs. upper mask
confint.merMod(lmer_darulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1359165 0.04533106

# rated not angry (correct): lower mask vs. upper mask
confint.merMod(lmer_darulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0384604 -0.002368244


####---- plot RTs by angry ratings and masks: lower mask vs.no mask ----####
# plot RTs by angry ratings and lower-masked disgusted faces
cat_plot(lmer_dadxl_logrt_rateAngry_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Oranges") +
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)

# plot RTs by angry ratings and lower-masked surprised faces
cat_plot(lmer_darxl_logrt_rateAngry_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Purples") +
  coord_cartesian(ylim=c(-3.00,-2.85)) +
  theme_minimal(base_size=24)


####---- plot RTs by angry ratings and masks: lower mask vs. upper mask ----####
# plot RTs by angry ratings and masked disgusted faces
cat_plot(lmer_dadul_logrt_rateAngry_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Oranges") +
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)

# plot RTs by angry ratings and masked happy faces
cat_plot(lmer_dahul_logrt_rateAngry_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95,
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)",
         legend.main = "rating", color.class = "Greens") +
  coord_cartesian(ylim=c(-2.95,-2.75)) +
  theme_minimal(base_size=24)

# plot RTs by angry ratings and masked surprised faces
cat_plot(lmer_darul_logrt_rateAngry_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Purples") +
  coord_cartesian(ylim=c(-3.00,-2.80)) +
  theme_minimal(base_size=24)
