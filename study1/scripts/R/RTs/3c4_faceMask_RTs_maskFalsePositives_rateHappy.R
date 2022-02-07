# faceMask RT analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating happiness
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
dh <- subset(d, emotionRating=='happy')

# recode face ratings
dh$rateEmotion[dh$rateEmotion == 1] <- 'correct'
dh$rateEmotion[dh$rateEmotion == 0] <- 'incorrect'
dh$rateEmotion <- as.factor(dh$rateEmotion)
dh$rateEmotion <- relevel(dh$rateEmotion, 'incorrect')

# further subset by facial expression
dha <- subset(dh, expression=='angry')
dhf <- subset(dh, expression=='fearful')
dhd <- subset(dh, expression=='disgusted')
dhs <- subset(dh, expression=='sad')
dhr <- subset(dh, expression=='surprised')

# further subset for masks of interest and re-level
# angry faces
dhaxu <- subset(dha, dha$mask != 'lower')
dhaxu$mask <- factor(dhaxu$mask, levels=c('none', 'upper'))
dhaxl <- subset(dha, dha$mask != 'upper')
dhaxl$mask <- factor(dhaxl$mask, levels=c('none', 'lower'))
dhaul <- subset(dha, dha$mask != 'none')
dhaul$mask <- factor(dhaul$mask, levels=c('upper', 'lower'))
# disgusted faces
dhdxu <- subset(dhd, dhd$mask != 'lower')
dhdxu$mask <- factor(dhdxu$mask, levels=c('none', 'upper'))
dhdxl <- subset(dhd, dhd$mask != 'upper')
dhdxl$mask <- factor(dhdxl$mask, levels=c('none', 'lower'))
dhdul <- subset(dhd, dhd$mask != 'none')
dhdul$mask <- factor(dhdul$mask, levels=c('upper', 'lower'))
# fearful faces
dhfxu <- subset(dhf, dhf$mask != 'lower')
dhfxu$mask <- factor(dhfxu$mask, levels=c('none', 'upper'))
dhfxl <- subset(dhf, dhf$mask != 'upper')
dhfxl$mask <- factor(dhfxl$mask, levels=c('none', 'lower'))
dhful <- subset(dhf, dhf$mask != 'none')
dhful$mask <- factor(dhful$mask, levels=c('upper', 'lower'))
# sad faces
dhsxu <- subset(dhs, dhs$mask != 'lower')
dhsxu$mask <- factor(dhsxu$mask, levels=c('none', 'upper'))
dhsxl <- subset(dhs, dhs$mask != 'upper')
dhsxl$mask <- factor(dhsxl$mask, levels=c('none', 'lower'))
dhsul <- subset(dhs, dhs$mask != 'none')
dhsul$mask <- factor(dhsul$mask, levels=c('upper', 'lower'))
# surprised faces
dhrxu <- subset(dhr, dhr$mask != 'lower')
dhrxu$mask <- factor(dhrxu$mask, levels=c('none', 'upper'))
dhrxl <- subset(dhr, dhr$mask != 'upper')
dhrxl$mask <- factor(dhrxl$mask, levels=c('none', 'lower'))
dhrul <- subset(dhr, dhr$mask != 'none')
dhrul$mask <- factor(dhrul$mask, levels=c('upper', 'lower'))


####---- RTs by emotion ratings * masks: lower mask vs. no mask ----####
## lower-masked angry faces
lmer_dhaxl_logrt_rateHappy_lowerMask <- with(dhaxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dhaxl_logrt_rateHappy_lowerMask) # interaction ns
# rated happy (incorrect)
dhaxlRateHappy <- subset(dhaxl, dhaxl$rateEmotion=='incorrect')
lmer_dhaxlRateHappy_logrt_lowerMask <- with(dhaxlRateHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhaxlRateHappy_logrt_lowerMask) # ns
# rated not happy (correct)
dhaxlRateNotHappy <- subset(dhaxl, dhaxl$rateEmotion=='correct')
lmer_dhaxlRateNotHappy_logrt_lowerMask <- with(dhaxlRateNotHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhaxlRateNotHappy_logrt_lowerMask) # slower rating angry faces 'not happy' with lower mask

## lower-masked disgusted faces
lmer_dhdxl_logrt_rateHappy_lowerMask <- with(dhdxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dhdxl_logrt_rateHappy_lowerMask) # interaction ns
# rated happy (incorrect)
dhdxlRateHappy <- subset(dhdxl, dhdxl$rateEmotion=='incorrect')
lmer_dhdxlRateHappy_logrt_lowerMask <- with(dhdxlRateHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhdxlRateHappy_logrt_lowerMask) # ns
# rated not happy (correct)
dhdxlRateNotHappy <- subset(dhdxl, dhdxl$rateEmotion=='correct')
lmer_dhdxlRateNotHappy_logrt_lowerMask <- with(dhdxlRateNotHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhdxlRateNotHappy_logrt_lowerMask) # slower rating disgusted faces 'not happy' with lower mask

## lower-masked sad faces
lmer_dhsxl_logrt_rateHappy_lowerMask <- with(dhsxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dhsxl_logrt_rateHappy_lowerMask) # interaction ns
# rated happy (incorrect)
dhsxlRateHappy <- subset(dhsxl, dhsxl$rateEmotion=='incorrect')
lmer_dhsxlRateHappy_logrt_lowerMask <- with(dhsxlRateHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhsxlRateHappy_logrt_lowerMask) # ns
# rated not happy (correct)
dhsxlRateNotHappy <- subset(dhsxl, dhsxl$rateEmotion=='correct')
lmer_dhsxlRateNotHappy_logrt_lowerMask <- with(dhsxlRateNotHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhsxlRateNotHappy_logrt_lowerMask) # slower rating sad faces 'not happy' with lower mask

## lower-masked surprised faces
lmer_dhrxl_logrt_rateHappy_lowerMask <- with(dhrxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dhrxl_logrt_rateHappy_lowerMask) # interaction **
# rated happy (incorrect)
dhrxlRateHappy <- subset(dhrxl, dhrxl$rateEmotion=='incorrect')
lmer_dhrxlRateHappy_logrt_lowerMask <- with(dhrxlRateHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhrxlRateHappy_logrt_lowerMask) # ns
# rated not happy (correct)
dhrxlRateNotHappy <- subset(dhrxl, dhrxl$rateEmotion=='correct')
lmer_dhrxlRateNotHappy_logrt_lowerMask <- with(dhrxlRateNotHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhrxlRateNotHappy_logrt_lowerMask) # slower rating surprised faces 'not happy' with lower mask


####---- RTs by emotion ratings * masks: upper mask vs. no mask ----####
## upper-masked angry faces
lmer_dhaxu_logrt_rateHappy_upperMask <- with(dhaxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dhaxu_logrt_rateHappy_upperMask) # interaction ns
# rated happy (incorrect)
dhaxuRateHappy <- subset(dhaxu, dhaxu$rateEmotion=='incorrect')
lmer_dhaxuRateHappy_logrt_upperMask <- with(dhaxuRateHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhaxuRateHappy_logrt_upperMask) # ns
# rated not happy (correct)
dhaxuRateNotHappy <- subset(dhaxu, dhaxu$rateEmotion=='correct')
lmer_dhaxuRateNotHappy_logrt_upperMask <- with(dhaxuRateNotHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhaxuRateNotHappy_logrt_upperMask) # ns


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## disgust faces
lmer_dhdul_logrt_rateHappy_mask <- with(dhdul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dhdul_logrt_rateHappy_mask) # interaction ***
# rated happy (incorrect)
dhdulRateHappy <- subset(dhdul, dhdul$rateEmotion=='incorrect')
lmer_dhdulRateHappy_logrt_mask <- with(dhdulRateHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhdulRateHappy_logrt_mask) # slower to rate disgust faces as 'happy' with lower mask vs. upper mask
# rated not happy (correct)
dhdulRateNotHappy <- subset(dhdul, dhdul$rateEmotion=='correct')
lmer_dhdulRateNotHappy_logrt_mask <- with(dhdulRateNotHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhdulRateNotHappy_logrt_mask) # faster to rate disgust faces as 'not happy' with upper mask vs. lower mask

## surprised faces
lmer_dhrul_logrt_rateHappy_mask <- with(dhrul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dhrul_logrt_rateHappy_mask) # interaction ns
# rated happy (incorrect)
dhrulRateHappy <- subset(dhrul, dhrul$rateEmotion=='incorrect')
lmer_dhrulRateHappy_logrt_mask <- with(dhrulRateHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhrulRateHappy_logrt_mask) # ns
# rated not happy (correct)
dhrulRateNotHappy <- subset(dhrul, dhrul$rateEmotion=='correct')
lmer_dhrulRateNotHappy_logrt_mask <- with(dhrulRateNotHappy, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhrulRateNotHappy_logrt_mask) # faster to rate surprised faces 'happy' with upper mask vs. lower mask


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask ----####
## lower-masked angry faces
# interaction
confint.merMod(lmer_dhaxl_logrt_rateHappy_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.03082503 0.1024569

# rated happy (incorrect): lower mask vs. no mask
confint.merMod(lmer_dhaxlRateHappy_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1968259 0.05972353

# rated not happy (correct): lower mask vs. no mask
confint.merMod(lmer_dhaxlRateNotHappy_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03115092 -0.002739989

## lower-masked disgust faces
# interaction
confint.merMod(lmer_dhdxl_logrt_rateHappy_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.00414656 0.1179475

# rated happy (incorrect): lower mask vs. no mask
confint.merMod(lmer_dhdxlRateHappy_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.216993 -0.008987465

# rated not happy (correct): lower mask vs. no mask
confint.merMod(lmer_dhdxlRateNotHappy_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04160574 -0.01408025


## lower-masked sad faces
# interaction
confint.merMod(lmer_dhsxl_logrt_rateHappy_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.1061636 0.003519326

# rated happy (incorrect): lower mask vs. no mask
confint.merMod(lmer_dhsxlRateHappy_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1160501 0.1123335

# rated not happy (correct): lower mask vs. no mask
confint.merMod(lmer_dhsxlRateNotHappy_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04529894 -0.02193801


## lower-masked surprised faces
# interaction
confint.merMod(lmer_dhrxl_logrt_rateHappy_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.1383184 -0.02929315

# rated happy (incorrect): lower mask vs. no mask
confint.merMod(lmer_dhrxlRateHappy_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03346045 0.161117

# rated not happy (correct): lower mask vs. no mask
confint.merMod(lmer_dhrxlRateNotHappy_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03365573 -0.003020962


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: upper mask vs. no mask ----####
## upper-masked angry faces
# interaction
confint.merMod(lmer_dhaxu_logrt_rateHappy_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.02096356 0.1036457

# rated happy (incorrect): upper mask vs. no mask
confint.merMod(lmer_dhaxuRateHappy_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# masklower -0.1923791 0.06763634

# rated not happy (correct): upper mask vs. no mask
confint.merMod(lmer_dhaxuRateNotHappy_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# masklower -0.02092355 0.00604345


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## masked disgusted faces
# interaction
confint.merMod(lmer_dhdul_logrt_rateHappy_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.05721386 0.1569404

# rated happy (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dhdulRateHappy_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.2166451 -0.02776967

# rated not happy (correct): lower mask vs. upper mask
confint.merMod(lmer_dhdulRateNotHappy_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03535226 -0.009231732


## masked surprised faces
# interaction
confint.merMod(lmer_dhrul_logrt_rateHappy_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.08608274 0.02144056

# rated happy (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dhrulRateHappy_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1125781 0.08109491

# rated not happy (correct): lower mask vs. upper mask
confint.merMod(lmer_dhrulRateNotHappy_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03785812 -0.01088319


####---- plot RTs by emotion ratings * masks: lower mask vs. no mask ----####
# plot RTs by happy ratings and lower-masked angry faces
cat_plot(lmer_dhaxl_logrt_rateHappy_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Reds") + 
  coord_cartesian(ylim=c(-2.90,-2.70)) +
  theme_minimal(base_size=24)

# plot RTs by happy ratings and lower-masked disgusted faces
cat_plot(lmer_dhdxl_logrt_rateHappy_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Oranges") +
  coord_cartesian(ylim=c(-2.95,-2.75)) +
  theme_minimal(base_size=24)

# plot RTs by happy ratings and lower-masked sad faces
cat_plot(lmer_dhsxl_logrt_rateHappy_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Greys") + 
  coord_cartesian(ylim=c(-2.95,-2.80)) +
  theme_minimal(base_size=24)

# plot RTs by happy ratings and lower-masked surprised faces
cat_plot(lmer_dhrxl_logrt_rateHappy_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Purples") + 
  coord_cartesian(ylim=c(-3.00,-2.80)) +
  theme_minimal(base_size=24)


####---- plot RTs by emotion ratings * masks: lower mask vs. upper mask ----####
# plot RTs by happy ratings and masked disgust faces
cat_plot(lmer_dhdul_logrt_rateHappy_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Oranges") + 
  coord_cartesian(ylim=c(-2.95,-2.70)) +
  theme_minimal(base_size=24)

# plot RTs by happy ratings and masked surprised faces
cat_plot(lmer_dhrul_logrt_rateHappy_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Purples") + 
  coord_cartesian(ylim=c(-2.95,-2.80)) +
  theme_minimal(base_size=24)
