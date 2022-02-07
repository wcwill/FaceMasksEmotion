# faceMask study 2 RT analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating happiness
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
dh <- subset(d, emotionRating=='happy')

# recode face ratings
dh$rateEmotion[dh$rateEmotion == 1] <- 'correct'
dh$rateEmotion[dh$rateEmotion == 0] <- 'incorrect'
dh$rateEmotion <- as.factor(dh$rateEmotion)
dh$rateEmotion <- relevel(dh$rateEmotion, 'incorrect')

# further subset by facial expression
dhs <- subset(dh, expression=='sad')

# further subset for masks of interest and re-level
# sad faces
dhsxu <- subset(dhs, dhs$mask != 'lower')
dhsxu$mask <- factor(dhsxu$mask, levels=c('none', 'upper'))
dhsxl <- subset(dhs, dhs$mask != 'upper')
dhsxl$mask <- factor(dhsxl$mask, levels=c('none', 'lower'))
dhsul <- subset(dhs, dhs$mask != 'none')
dhsul$mask <- factor(dhsul$mask, levels=c('upper', 'lower'))


####---- RTs by emotion ratings * masks: lower mask vs. no mask ----####
## lower-masked sad faces
lmer_dhsxl_logrt_ratehappy_lowerMask <- with(dhsxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dhsxl_logrt_ratehappy_lowerMask) # interaction ns
# rated happy (incorrect)
dhsxlRateAng <- subset(dhsxl, dhsxl$rateEmotion=='incorrect')
lmer_dhsxlRateAng_logrt_lowerMask <- with(dhsxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhsxlRateAng_logrt_lowerMask) # slower rating sad faces 'happy' with lower mask *
# rated not happy (correct)
dhsxlRateNotAng <- subset(dhsxl, dhsxl$rateEmotion=='correct')
lmer_dhsxlRateNotAng_logrt_lowerMask <- with(dhsxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhsxlRateNotAng_logrt_lowerMask) # slower rating sad faces 'not happy' with lower mask ***


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## sad faces
lmer_dhsul_logrt_ratehappy_mask <- with(dhsul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dhsul_logrt_ratehappy_mask) # interaction ns
# rated happy (incorrect)
dhrulRateAng <- subset(dhsul, dhsul$rateEmotion=='incorrect')
lmer_dhsulRateAng_logrt_mask <- with(dhrulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhsulRateAng_logrt_mask) # ns
# rated not happy (correct)
dhrulRateNotAng <- subset(dhsul, dhsul$rateEmotion=='correct')
lmer_dhsulRateNotAng_logrt_mask <- with(dhrulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dhsulRateNotAng_logrt_mask) # slower rating sad faces 'not happy' with lower vs. upper mask ***


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask ----####
## lower-masked sad faces
# interaction
confint.merMod(lmer_dhsxl_logrt_ratehappy_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.02761627 0.1232105 

# rated happy (incorrect): lower mask vs. no mask
confint.merMod(lmer_dhsxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1521008 -0.01874338  

# rated not happy (correct): lower mask vs. no mask
confint.merMod(lmer_dhsxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0491602 -0.01388646 


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## masked sad faces
# interaction
confint.merMod(lmer_dhsul_logrt_ratehappy_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.07855922 0.0865046

# rated happy (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dhsulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1487641 0.08390352

# rated not happy (correct): lower mask vs. upper mask
confint.merMod(lmer_dhsulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04498839 -0.01051753


####---- plot RTs by happy ratings and masks: lower mask vs. no mask ----####
# plot RTs by happy ratings and lower-masked sad faces
cat_plot(lmer_dhsxl_logrt_ratehappy_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greys") +
  coord_cartesian(ylim=c(-3.00,-2.75)) +
  theme_minimal(base_size=24)


####---- plot RTs by happy ratings and masks: lower mask vs. upper mask ----####
# plot RTs by happy ratings and masked sad faces
cat_plot(lmer_dhsul_logrt_ratehappy_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95,
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)",
         legend.main = "rating", color.class = "Greys") +
  coord_cartesian(ylim=c(-3.00,-2.80)) +
  theme_minimal(base_size=24)
