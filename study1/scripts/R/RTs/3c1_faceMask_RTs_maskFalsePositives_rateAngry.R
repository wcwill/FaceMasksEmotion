# faceMask RT analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating anger
# note: need to have run '1_faceMask_proc.R' script first
# 9/22/20
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
summary(lmer_dadxl_logrt_rateAngry_lowerMask) # interaction ns
# rated angry (incorrect)
dadxlRateAng <- subset(dadxl, dadxl$rateEmotion=='incorrect')
lmer_dadxlRateAng_logrt_lowerMask <- with(dadxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dadxlRateAng_logrt_lowerMask) # ns
# rated not angry (correct)
dadxlRateNotAng <- subset(dadxl, dadxl$rateEmotion=='correct')
lmer_dadxlRateNotAng_logrt_lowerMask <- with(dadxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dadxlRateNotAng_logrt_lowerMask) # ns

## lower-masked happy faces
lmer_dahxl_logrt_rateAngry_lowerMask <- with(dahxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dahxl_logrt_rateAngry_lowerMask) # interaction **
# rated angry (incorrect)
dahxlRateAng <- subset(dahxl, dahxl$rateEmotion=='incorrect')
lmer_dahxlRateAng_logrt_lowerMask <- with(dahxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dahxlRateAng_logrt_lowerMask) # slower rating happy faces 'angry' with lower mask
# rated not angry (correct)
dahxlRateNotAng <- subset(dahxl, dahxl$rateEmotion=='correct')
lmer_dahxlRateNotAng_logrt_lowerMask <- with(dahxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dahxlRateNotAng_logrt_lowerMask) # ns

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


####---- RTs by emotion ratings * masks: upper mask vs. no mask ----####
## upper-masked disgusted faces
lmer_dadxu_logrt_rateAngry_upperMask <- with(dadxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dadxu_logrt_rateAngry_upperMask) # interaction ***
# rated angry (incorrect)
dadxuRateAng <- subset(dadxu, dadxu$rateEmotion=='incorrect')
lmer_dadxuRateAng_logrt_upperMask <- with(dadxuRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dadxuRateAng_logrt_upperMask) # ns
# rated not angry (correct)
dadxuRateNotAng <- subset(dadxu, dadxu$rateEmotion=='correct')
lmer_dadxuRateNotAng_logrt_upperMask <- with(dadxuRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dadxuRateNotAng_logrt_upperMask) # faster rating disgusted faces 'not angry' with upper mask

## upper-masked sad faces
lmer_dasxu_logrt_rateAngry_upperMask <- with(dasxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dasxu_logrt_rateAngry_upperMask) # interaction ns
# rated angry (incorrect)
dasxuRateAng <- subset(dasxu, dasxu$rateEmotion=='incorrect')
lmer_dasxuRateAng_logrt_upperMask <- with(dasxuRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dasxuRateAng_logrt_upperMask) # ns
# rated not angry (correct)
dasxuRateNotAng <- subset(dasxu, dasxu$rateEmotion=='correct')
lmer_dasxuRateNotAng_logrt_upperMask <- with(dasxuRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dasxuRateNotAng_logrt_upperMask) # ns


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## disgust faces
lmer_dadul_logrt_rateAngry_mask <- with(dadul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dadul_logrt_rateAngry_mask) # interaction *
# rated angry (incorrect)
dadulRateAng <- subset(dadul, dadul$rateEmotion=='incorrect')
lmer_dadulRateAng_logrt_mask <- with(dadulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dadulRateAng_logrt_mask) # ns
# rated not angry (correct)
dadulRateNotAng <- subset(dadul, dadul$rateEmotion=='correct')
lmer_dadulRateNotAng_logrt_mask <- with(dadulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dadulRateNotAng_logrt_mask) # slower rating disgust face 'not angry' with lower mask vs. upper

## fearful faces
lmer_daful_logrt_rateAngry_mask <- with(daful, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_daful_logrt_rateAngry_mask) # interaction ns
# rated angry (incorrect)
dafulRateAng <- subset(daful, daful$rateEmotion=='incorrect')
lmer_dafulRateAng_logrt_mask <- with(dafulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dafulRateAng_logrt_mask) # ns
# rated not angry (correct)
dafulRateNotAng <- subset(daful, daful$rateEmotion=='correct')
lmer_dafulRateNotAng_logrt_mask <- with(dafulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dafulRateNotAng_logrt_mask) # ns

## happy faces
lmer_dahul_logrt_rateAngry_mask <- with(dahul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dahul_logrt_rateAngry_mask) # interaction ns
# rated angry (incorrect)
dahulRateAng <- subset(dahul, dahul$rateEmotion=='incorrect')
lmer_dahulRateAng_logrt_mask <- with(dahulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dahulRateAng_logrt_mask) # ns
# rated not angry (correct)
dahulRateNotAng <- subset(dahul, dahul$rateEmotion=='correct')
lmer_dahulRateNotAng_logrt_mask <- with(dahulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dahulRateNotAng_logrt_mask) # slower rating happy face 'not angry' with lower mask vs. upper


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask ----####
## lower-masked disgusted faces
# interaction
confint.merMod(lmer_dadxl_logrt_rateAngry_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.01575992 0.05678083

# rated angry (incorrect): lower mask vs. no mask
confint.merMod(lmer_dadxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.01625062 0.02353421

# rated not angry (correct): lower mask vs. no mask
confint.merMod(lmer_dadxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.01647782 0.05014416


## lower-masked happy faces
# interaction
confint.merMod(lmer_dahxl_logrt_rateAngry_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.03289179 0.1499426

# rated angry (incorrect): lower mask vs. no mask
confint.merMod(lmer_dahxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.207752 -0.0212181

# rated not angry (correct): lower mask vs. no mask
confint.merMod(lmer_dahxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.02272689 0.00639995


## lower-masked surprised faces
# interaction
confint.merMod(lmer_darxl_logrt_rateAngry_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.0658743 0.02156183

# rated angry (incorrect): lower mask vs. no mask
confint.merMod(lmer_darxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.06627305 0.05346453

# rated not angry (correct): lower mask vs. no mask
confint.merMod(lmer_darxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0463279 -0.01544154


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: upper mask vs. no mask ----####
## upper-masked disgusted faces
# interaction
confint.merMod(lmer_dadxu_logrt_rateAngry_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper 0.033323 0.09398676

# rated angry (incorrect): upper mask vs. no mask
confint.merMod(lmer_dadxuRateAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.03761851 0.00802753

# rated not angry (correct): upper mask vs. no mask
confint.merMod(lmer_dadxuRateNotAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper 0.02673762 0.07237726


## upper-masked sad faces
# interaction
confint.merMod(lmer_dasxu_logrt_rateAngry_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.04857485 0.02893679

# rated angry (incorrect): upper mask vs. no mask
confint.merMod(lmer_dasxuRateAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.03103341 0.05113652

# rated not angry (correct): upper mask vs. no mask
confint.merMod(lmer_dasxuRateNotAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.01986366 0.01279527


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## masked disgusted faces
# interaction
confint.merMod(lmer_dadul_logrt_rateAngry_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.07729091 -0.00798758

# rated angry (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dadulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.007189253 0.03733146

# rated not angry (correct): lower mask vs. upper mask
confint.merMod(lmer_dadulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05926105 -0.006311333


## masked fearful faces
# interaction
confint.merMod(lmer_daful_logrt_rateAngry_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.05304448 0.02556703

# rated angry (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dafulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04003981 0.05937619

# rated not angry (correct): lower mask vs. upper mask
confint.merMod(lmer_dafulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.02032214 0.01224305


## masked happy faces
# interaction
confint.merMod(lmer_dahul_logrt_rateAngry_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.02481696 0.09023503

# rated angry (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dahulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1510754 0.02614821

# rated not angry (correct): lower mask vs. upper mask
confint.merMod(lmer_dahulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03785656 -0.009655338


####---- plot RTs by emotion ratings * masks: lower mask vs. no mask ----####
# plot RTs by angry ratings and lower-masked happy faces
cat_plot(lmer_dahxl_logrt_rateAngry_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greens") + 
  coord_cartesian(ylim=c(-2.95,-2.70)) +
  theme_minimal(base_size=24) 

# plot RTs by angry ratings and lower-masked surprised faces
cat_plot(lmer_darxl_logrt_rateAngry_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Purples") + 
  coord_cartesian(ylim=c(-3.00,-2.85)) +
  theme_minimal(base_size=24) 


####---- plot RTs by emotion ratings * masks: upper mask vs. no mask ----####
# plot RTs by angry ratings and upper-masked disgust faces
cat_plot(lmer_dadxu_logrt_rateAngry_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Oranges") + 
  coord_cartesian(ylim=c(-3.00,-2.85)) +
  theme_minimal(base_size=24)


####---- plot RTs by emotion ratings * masks: lower mask vs. upper mask ----####
# plot RTs by angry ratings and masked disgust faces
cat_plot(lmer_dadul_logrt_rateAngry_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Oranges") + 
  coord_cartesian(ylim=c(-3.00,-2.85)) +
  theme_minimal(base_size=24)

# plot RTs by angry ratings and masked happy faces
cat_plot(lmer_dahul_logrt_rateAngry_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greens") + 
  coord_cartesian(ylim=c(-2.95,-2.70)) +
  theme_minimal(base_size=24)
