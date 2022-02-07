# faceMask study 2 RT analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating disgust
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
dd <- subset(d, emotionRating=='disgusted')

# recode face ratings
dd$rateEmotion[dd$rateEmotion == 1] <- 'correct'
dd$rateEmotion[dd$rateEmotion == 0] <- 'incorrect'
dd$rateEmotion <- as.factor(dd$rateEmotion)
dd$rateEmotion <- relevel(dd$rateEmotion, 'incorrect')

# further subset by facial expression
dda <- subset(dd, expression=='angry')
ddf <- subset(dd, expression=='fearful')
ddh <- subset(dd, expression=='happy')
dds <- subset(dd, expression=='sad')
ddr <- subset(dd, expression=='surprised')

# further subset for masks of interest and re-level
# angry faces
ddaxu <- subset(dda, dda$mask != 'lower')
ddaxu$mask <- factor(ddaxu$mask, levels=c('none', 'upper'))
ddaxl <- subset(dda, dda$mask != 'upper')
ddaxl$mask <- factor(ddaxl$mask, levels=c('none', 'lower'))
ddaul <- subset(dda, dda$mask != 'none')
ddaul$mask <- factor(ddaul$mask, levels=c('upper', 'lower'))
# fearful faces
ddfxu <- subset(ddf, ddf$mask != 'lower')
ddfxu$mask <- factor(ddfxu$mask, levels=c('none', 'upper'))
ddfxl <- subset(ddf, ddf$mask != 'upper')
ddfxl$mask <- factor(ddfxl$mask, levels=c('none', 'lower'))
ddful <- subset(ddf, ddf$mask != 'none')
ddful$mask <- factor(ddful$mask, levels=c('upper', 'lower'))


####---- RTs by emotion ratings * masks: lower mask vs. no mask ----####
## lower-masked fearful faces
lmer_ddfxl_logrt_ratedisgusted_lowerMask <- with(ddfxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_ddfxl_logrt_ratedisgusted_lowerMask) # interaction *
# rated disgusted (incorrect)
ddfxlRateAng <- subset(ddfxl, ddfxl$rateEmotion=='incorrect')
lmer_ddfxlRateAng_logrt_lowerMask <- with(ddfxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddfxlRateAng_logrt_lowerMask) # ns
# rated not disgusted (correct)
ddfxlRateNotAng <- subset(ddfxl, ddfxl$rateEmotion=='correct')
lmer_ddfxlRateNotAng_logrt_lowerMask <- with(ddfxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddfxlRateNotAng_logrt_lowerMask) # faster rating fearful faces 'not disgusted' with lower mask ***


####---- RTs by emotion ratings * masks: upper mask vs. no mask ----####
## upper-masked angry faces
lmer_ddaxu_logrt_ratedisgusted_upperMask <- with(ddaxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_ddaxu_logrt_ratedisgusted_upperMask) # interaction ns
# rated disgusted (incorrect)
ddaxuRateAng <- subset(ddaxu, ddaxu$rateEmotion=='incorrect')
lmer_ddaxuRateAng_logrt_upperMask <- with(ddaxuRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddaxuRateAng_logrt_upperMask) # ns
# rated not disgusted (correct)
ddaxuRateNotAng <- subset(ddaxu, ddaxu$rateEmotion=='correct')
lmer_ddaxuRateNotAng_logrt_upperMask <- with(ddaxuRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddaxuRateNotAng_logrt_upperMask) # faster rating angry faces 'not disgusted' with upper mask ***

## upper-masked fearful faces
lmer_ddfxu_logrt_ratedisgusted_upperMask <- with(ddfxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_ddfxu_logrt_ratedisgusted_upperMask) # interaction ns
# rated disgusted (incorrect)
ddfxuRateAng <- subset(ddfxu, ddfxu$rateEmotion=='incorrect')
lmer_ddfxuRateAng_logrt_upperMask <- with(ddfxuRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddfxuRateAng_logrt_upperMask) # ns
# rated not disgusted (correct)
ddfxuRateNotAng <- subset(ddfxu, ddfxu$rateEmotion=='correct')
lmer_ddfxuRateNotAng_logrt_upperMask <- with(ddfxuRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddfxuRateNotAng_logrt_upperMask) # ns


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## angry faces
lmer_ddaul_logrt_ratedisgusted_mask <- with(ddaul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_ddaul_logrt_ratedisgusted_mask) # interaction ns
# rated disgusted (incorrect)
ddrulRateAng <- subset(ddaul, ddaul$rateEmotion=='incorrect')
lmer_ddaulRateAng_logrt_mask <- with(ddrulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddaulRateAng_logrt_mask) # ns
# rated not disgusted (correct)
ddrulRateNotAng <- subset(ddaul, ddaul$rateEmotion=='correct')
lmer_ddaulRateNotAng_logrt_mask <- with(ddrulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddaulRateNotAng_logrt_mask) # slower rating angry faces 'not disgusted' with lower vs. upper mask ***
 
## fearful faces
lmer_ddful_logrt_ratedisgusted_mask <- with(ddful, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_ddful_logrt_ratedisgusted_mask) # interaction ns
# rated disgusted
ddfulRateAng <- subset(ddful, ddful$rateEmotion=='incorrect')
lmer_ddfulRateAng_logrt_mask <- with(ddfulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddfulRateAng_logrt_mask) # ns
# rated not disgusted
ddfulRateNotAng <- subset(ddful, ddful$rateEmotion=='correct')
lmer_ddfulRateNotAng_logrt_mask <- with(ddfulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddfulRateNotAng_logrt_mask) # faster rating fearful faces 'not disgusted' with lower vs. upper mask ***


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask ----####
## lower-masked fearful faces
# interaction
confint.merMod(lmer_ddfxl_logrt_ratedisgusted_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.0009089778 0.08682173

# rated disgusted (incorrect): lower mask vs. no mask
confint.merMod(lmer_ddfxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0313916 0.04012131 

# rated not disgusted (correct): lower mask vs. no mask
confint.merMod(lmer_ddfxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.02611232 0.07128187


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: upper mask vs. no mask ----####
## lower-masked angry faces
# interaction
confint.merMod(lmer_ddaxu_logrt_ratedisgusted_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.001244725 0.07688297

# rated disgusted (incorrect): lower mask vs. no mask
confint.merMod(lmer_ddaxuRateAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.03747983 0.0416659

# rated not disgusted (correct): lower mask vs. no mask
confint.merMod(lmer_ddaxuRateNotAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper 0.0267455 0.06061057

## masked fearful faces
# interaction
confint.merMod(lmer_ddfxu_logrt_ratedisgusted_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.02485935 0.05560497

# rated disgusted (incorrect): lower mask vs. no mask
confint.merMod(lmer_ddfxuRateAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.03375352 0.02843298

# rated not disgusted (correct): lower mask vs. no mask
confint.merMod(lmer_ddfxuRateNotAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.01548531 0.03298412


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## masked angry faces
# interaction
confint.merMod(lmer_ddaul_logrt_ratedisgusted_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.05653829 0.02022022

# rated disgusted (incorrect): lower mask vs. upper mask
confint.merMod(lmer_ddaulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04577746 0.02297959

# rated not disgusted (correct): lower mask vs. upper mask
confint.merMod(lmer_ddaulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04949685 -0.01590362

## masked fearful faces
# interaction
confint.merMod(lmer_ddful_logrt_ratedisgusted_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.0007520102 0.07936641

# rated disgusted (incorrect): lower mask vs. upper mask
confint.merMod(lmer_ddfulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03058674 0.0384507

# rated not disgusted (correct): lower mask vs. upper mask
confint.merMod(lmer_ddfulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.02279967 0.06522479


####---- plot RTs by disgusted ratings and masks: lower mask vs.no mask ----####
# plot RTs by disgusted ratings and lower-masked fearful faces
cat_plot(lmer_ddfxl_logrt_ratedisgusted_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Blues") +
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)


####---- plot RTs by disgusted ratings and masks: upper mask vs. no mask ----####
# plot RTs by disgusted ratings and upper-masked angry faces
cat_plot(lmer_ddaxu_logrt_ratedisgusted_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Reds") +
  coord_cartesian(ylim=c(-3.00,-2.85)) +
  theme_minimal(base_size=24)


####---- plot RTs by disgusted ratings and masks: lower mask vs. upper mask ----####
# plot RTs by disgusted ratings and masked angry faces
cat_plot(lmer_ddaul_logrt_ratedisgusted_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Reds") +
  coord_cartesian(ylim=c(-3.05,-2.85)) +
  theme_minimal(base_size=24)

# plot RTs by disgusted ratings and masked fearful faces
cat_plot(lmer_ddful_logrt_ratedisgusted_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95,
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)",
         legend.main = "rating", color.class = "Blues") +
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)
