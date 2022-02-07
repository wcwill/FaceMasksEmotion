# faceMask RT analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating sadness
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
ds <- subset(d, emotionRating=='sad')

# recode face ratings
ds$rateEmotion[ds$rateEmotion == 1] <- 'correct'
ds$rateEmotion[ds$rateEmotion == 0] <- 'incorrect'
ds$rateEmotion <- as.factor(ds$rateEmotion)
ds$rateEmotion <- relevel(ds$rateEmotion, 'incorrect')

# further subset by facial expression
dsa <- subset(ds, expression=='angry')
dsd <- subset(ds, expression=='disgusted')
dsf <- subset(ds, expression=='fearful')
dsh <- subset(ds, expression=='happy')
dsr <- subset(ds, expression=='surprised')

# further subset for masks of interest and re-level
# angry faces
dsaxu <- subset(dsa, dsa$mask != 'lower')
dsaxu$mask <- factor(dsaxu$mask, levels=c('none', 'upper'))
dsaxl <- subset(dsa, dsa$mask != 'upper')
dsaxl$mask <- factor(dsaxl$mask, levels=c('none', 'lower'))
dsaul <- subset(dsa, dsa$mask != 'none')
dsaul$mask <- factor(dsaul$mask, levels=c('upper', 'lower'))
# disgusted faces
dsdxu <- subset(dsd, dsd$mask != 'lower')
dsdxu$mask <- factor(dsdxu$mask, levels=c('none', 'upper'))
dsdxl <- subset(dsd, dsd$mask != 'upper')
dsdxl$mask <- factor(dsdxl$mask, levels=c('none', 'lower'))
dsdul <- subset(dsd, dsd$mask != 'none')
dsdul$mask <- factor(dsdul$mask, levels=c('upper', 'lower'))
# fearful faces
dsfxu <- subset(dsf, dsf$mask != 'lower')
dsfxu$mask <- factor(dsfxu$mask, levels=c('none', 'upper'))
dsfxl <- subset(dsf, dsf$mask != 'upper')
dsfxl$mask <- factor(dsfxl$mask, levels=c('none', 'lower'))
dsful <- subset(dsf, dsf$mask != 'none')
dsful$mask <- factor(dsful$mask, levels=c('upper', 'lower'))
# happy faces
dshxu <- subset(dsh, dsh$mask != 'lower')
dshxu$mask <- factor(dshxu$mask, levels=c('none', 'upper'))
dshxl <- subset(dsh, dsh$mask != 'upper')
dshxl$mask <- factor(dshxl$mask, levels=c('none', 'lower'))
dshul <- subset(dsh, dsh$mask != 'none')
dshul$mask <- factor(dshul$mask, levels=c('upper', 'lower'))
# surprised faces
dsrxu <- subset(dsr, dsr$mask != 'lower')
dsrxu$mask <- factor(dsrxu$mask, levels=c('none', 'upper'))
dsrxl <- subset(dsr, dsr$mask != 'upper')
dsrxl$mask <- factor(dsrxl$mask, levels=c('none', 'lower'))
dsrul <- subset(dsr, dsr$mask != 'none')
dsrul$mask <- factor(dsrul$mask, levels=c('upper', 'lower'))


####---- RTs by emotion ratings * masks: lower mask vs. no mask ----####
## lower-masked angry faces
lmer_dsaxl_logrt_rateSad_lowerMask <- with(dsaxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsaxl_logrt_rateSad_lowerMask) # interaction **
# rated sad (incorrect)
dsaxlRateSad <- subset(dsaxl, dsaxl$rateEmotion=='incorrect')
lmer_dsaxlRateSad_logrt_lowerMask <- with(dsaxlRateSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsaxlRateSad_logrt_lowerMask) # ns
# rated not sad (correct)
dsaxlRateNotSad <- subset(dsaxl, dsaxl$rateEmotion=='correct')
lmer_dsaxlRateNotSad_logrt_lowerMask <- with(dsaxlRateNotSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsaxlRateNotSad_logrt_lowerMask) # ns

## lower-masked fearful faces
lmer_dsfxl_logrt_rateSad_lowerMask <- with(dsfxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsfxl_logrt_rateSad_lowerMask) # interaction *
# rated sad (incorrect)
dsfxlRateSad <- subset(dsfxl, dsfxl$rateEmotion=='incorrect')
lmer_dsfxlRateSad_logrt_lowerMask <- with(dsfxlRateSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsfxlRateSad_logrt_lowerMask) # ns
# rated not sad (correct)
dsfxlRateNotSad <- subset(dsfxl, dsfxl$rateEmotion=='correct')
lmer_dsfxlRateNotSad_logrt_lowerMask <- with(dsfxlRateNotSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsfxlRateNotSad_logrt_lowerMask) # faster rating fearful faces 'not sad' with lower mask

## lower-masked happy faces
lmer_dshxl_logrt_rateSad_lowerMask <- with(dshxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dshxl_logrt_rateSad_lowerMask) # interaction ns
# rated sad (incorrect)
dshxlRateSad <- subset(dshxl, dshxl$rateEmotion=='incorrect')
lmer_dshxlRateSad_logrt_lowerMask <- with(dshxlRateSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dshxlRateSad_logrt_lowerMask) # ns
# rated not sad (correct)
dshxlRateNotSad <- subset(dshxl, dshxl$rateEmotion=='correct')
lmer_dshxlRateNotSad_logrt_lowerMask <- with(dshxlRateNotSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dshxlRateNotSad_logrt_lowerMask) # ns


####---- RTs by emotion ratings * masks: upper mask vs. no mask ----####
## upper-masked angry faces
lmer_dsaxu_logrt_rateSad_upperMask <- with(dsaxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsaxu_logrt_rateSad_upperMask) # interaction ns
# rated sad (incorrect)
dsaxuRateSad <- subset(dsaxu, dsaxu$rateEmotion=='incorrect')
lmer_dsaxuRateSad_logrt_upperMask <- with(dsaxuRateSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsaxuRateSad_logrt_upperMask) # ns
# rated not sad (correct)
dsaxuRateNotSad <- subset(dsaxu, dsaxu$rateEmotion=='correct')
lmer_dsaxuRateNotSad_logrt_upperMask <- with(dsaxuRateNotSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsaxuRateNotSad_logrt_upperMask) # ns

## upper-masked fearful faces
lmer_dsfxu_logrt_rateSad_upperMask <- with(dsfxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsfxu_logrt_rateSad_upperMask) # interaction ns
# rated sad (incorrect)
dsfxuRateSad <- subset(dsfxu, dsfxu$rateEmotion=='incorrect')
lmer_dsfxuRateSad_logrt_upperMask <- with(dsfxuRateSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsfxuRateSad_logrt_upperMask) # ns
# rated not sad (correct)
dsfxuRateNotSad <- subset(dsfxu, dsfxu$rateEmotion=='correct')
lmer_dsfxuRateNotSad_logrt_upperMask <- with(dsfxuRateNotSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsfxuRateNotSad_logrt_upperMask) # ns


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## masked angry faces
lmer_dsaul_logrt_rateSad_mask <- with(dsaul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsaul_logrt_rateSad_mask) # interaction **
# rated sad (incorrect)
dsaulRateSad <- subset(dsaul, dsaul$rateEmotion=='incorrect')
lmer_dsaulRateSad_logrt_mask <- with(dsaulRateSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsaulRateSad_logrt_mask) # ns
# rated not sad (correct)
dsaulRateNotSad <- subset(dsaul, dsaul$rateEmotion=='correct')
lmer_dsaulRateNotSad_logrt_mask <- with(dsaulRateNotSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsaulRateNotSad_logrt_mask) # faster rating angry faces 'not sad' with lower mask vs. upper mask

## masked fearful faces
lmer_dsful_logrt_rateSad_mask <- with(dsful, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsful_logrt_rateSad_mask) # interaction ns
# rated sad (incorrect)
dsfulRateSad <- subset(dsful, dsful$rateEmotion=='incorrect')
lmer_dsfulRateSad_logrt_mask <- with(dsfulRateSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsfulRateSad_logrt_mask) # ns
# rated not sad (correct)
dsfulRateNotSad <- subset(dsful, dsful$rateEmotion=='correct')
lmer_dsfulRateNotSad_logrt_mask <- with(dsfulRateNotSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsfulRateNotSad_logrt_mask) # faster rating fearful faces 'not sad' with lower mask vs. upper mask

## masked happy faces
lmer_dshul_logrt_rateSad_mask <- with(dshul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dshul_logrt_rateSad_mask) # interaction ns
# rated sad (incorrect)
dshulRateSad <- subset(dshul, dshul$rateEmotion=='incorrect')
lmer_dshulRateSad_logrt_mask <- with(dshulRateSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dshulRateSad_logrt_mask) # ns
# rated not sad (correct)
dshulRateNotSad <- subset(dshul, dshul$rateEmotion=='correct')
lmer_dshulRateNotSad_logrt_mask <- with(dshulRateNotSad, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dshulRateNotSad_logrt_mask) # faster rating happy faces 'not sad' with upper mask vs. lower mask


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask ----####
## lower-masked angry faces
# interaction
confint.merMod(lmer_dsaxl_logrt_rateSad_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.01190165 0.08534024

# rated sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dsaxlRateSad_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.06368042 0.00193022

# rated not sad (correct): lower mask vs. no mask
confint.merMod(lmer_dsaxlRateNotSad_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.001003889 0.04175277


## lower-masked fearful faces
# interaction
confint.merMod(lmer_dsfxl_logrt_rateSad_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.008558913 0.08313732

# rated sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dsfxlRateSad_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0512839 0.01876695

# rated not sad (correct): lower mask vs. no mask
confint.merMod(lmer_dsfxlRateNotSad_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.01252065 0.04843417


## lower-masked happy faces
# interaction
confint.merMod(lmer_dshxl_logrt_rateSad_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.07990394 0.02567759

# rated sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dshxlRateSad_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1043772 0.08470492

# rated not sad (correct): lower mask vs. no mask
confint.merMod(lmer_dshxlRateNotSad_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.02425554 0.003252572


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: upper mask vs. no mask ----####
## upper-masked angry faces
# interaction
confint.merMod(lmer_dsaxu_logrt_rateSad_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.05686662 0.00733816

# rated sad (incorrect): upper mask vs. no mask
confint.merMod(lmer_dsaxuRateSad_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.01417291 0.03288899

# rated not sad (correct): upper mask vs. no mask
confint.merMod(lmer_dsaxuRateNotSad_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.0343616 0.005749949


## upper-masked fearful faces
# interaction
confint.merMod(lmer_dsfxu_logrt_rateSad_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.02647691 0.04819635

# rated sad (incorrect): upper mask vs. no mask
confint.merMod(lmer_dsfxuRateSad_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.03632336 0.02255026

# rated not sad (correct): upper mask vs. no mask
confint.merMod(lmer_dsfxuRateNotSad_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.02492731 0.0128404


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## masked angry faces
# interaction
confint.merMod(lmer_dsaul_logrt_rateSad_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.01748618 0.09343583

# rated sad (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dsaulRateSad_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05970828 0.002251047

# rated not sad (correct): lower mask vs. upper mask
confint.merMod(lmer_dsaulRateNotSad_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.006046096 0.05273782


## masked fearful faces
# interaction
confint.merMod(lmer_dsful_logrt_rateSad_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.01381672 0.05896482

# rated sad (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dsfulRateSad_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.02817276 0.05090829

# rated not sad (correct): lower mask vs. upper mask
confint.merMod(lmer_dsfulRateNotSad_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.01452416 0.0484701


## masked happy faces
# interaction
confint.merMod(lmer_dshul_logrt_rateSad_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.08471862 0.03146022

# rated sad (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dshulRateSad_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0944083 0.09941281

# rated not sad (correct): lower mask vs. upper mask
confint.merMod(lmer_dshulRateNotSad_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03155271 -0.001810772


####---- plot RTs by emotion ratings * masks: lower mask vs. no mask ----####
# plot sad rating RTs by sad ratings and lower-masked angry faces
cat_plot(lmer_dsaxl_logrt_rateSad_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Reds") + 
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)

# plot sad rating RTs by sad ratings and lower-masked fearful faces
cat_plot(lmer_dsfxl_logrt_rateSad_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Blues") + 
  coord_cartesian(ylim=c(-3.00,-2.85)) +
  theme_minimal(base_size=24)


####---- plot RTs by emotion ratings * masks: lower mask vs. upper mask ----####
# plot sad rating RTs by sad ratings and masked angry faces
cat_plot(lmer_dsaul_logrt_rateSad_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Reds") + 
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)

# plot sad rating RTs by sad ratings and masked fearful faces
cat_plot(lmer_dsful_logrt_rateSad_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Blues") + 
  coord_cartesian(ylim=c(-3.00,-2.85)) +
  theme_minimal(base_size=24)

# plot sad rating RTs by sad ratings and masked happy faces
cat_plot(lmer_dshul_logrt_rateSad_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Greens") + 
  coord_cartesian(ylim=c(-2.95,-2.75)) +
  theme_minimal(base_size=24)
