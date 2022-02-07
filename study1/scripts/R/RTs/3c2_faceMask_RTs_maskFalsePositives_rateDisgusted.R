# faceMask RT analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating disgust
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
# happy faces
ddhxu <- subset(ddh, ddh$mask != 'lower')
ddhxu$mask <- factor(ddhxu$mask, levels=c('none', 'upper'))
ddhxl <- subset(ddh, ddh$mask != 'upper')
ddhxl$mask <- factor(ddhxl$mask, levels=c('none', 'lower'))
ddhul <- subset(ddh, ddh$mask != 'none')
ddhul$mask <- factor(ddhul$mask, levels=c('upper', 'lower'))
# sad faces
ddsxu <- subset(dds, dds$mask != 'lower')
ddsxu$mask <- factor(ddsxu$mask, levels=c('none', 'upper'))
ddsxl <- subset(dds, dds$mask != 'upper')
ddsxl$mask <- factor(ddsxl$mask, levels=c('none', 'lower'))
ddsul <- subset(dds, dds$mask != 'none')
ddsul$mask <- factor(ddsul$mask, levels=c('upper', 'lower'))
# surprised faces
ddrxu <- subset(ddr, ddr$mask != 'lower')
ddrxu$mask <- factor(ddrxu$mask, levels=c('none', 'upper'))
ddrxl <- subset(ddr, ddr$mask != 'upper')
ddrxl$mask <- factor(ddrxl$mask, levels=c('none', 'lower'))
ddrul <- subset(ddr, ddr$mask != 'none')
ddrul$mask <- factor(ddrul$mask, levels=c('upper', 'lower'))


####---- RTs by emotion ratings * masks: lower mask vs. no mask ----####
## lower-masked angry faces
lmer_ddaxl_logrt_rateDisgust_lowerMask <- with(ddaxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_ddaxl_logrt_rateDisgust_lowerMask) # interaction ns
# rated disgusted
ddaxlRateDis <- subset(ddaxl, ddaxl$rateEmotion=='incorrect')
lmer_ddaxlRateDis_logrt_lowerMask <- with(ddaxlRateDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddaxlRateDis_logrt_lowerMask) # ns
# rated not disgusted
ddaxlRateNotDis <- subset(ddaxl, ddaxl$rateEmotion=='correct')
lmer_ddaxlRateNotDis_logrt_lowerMask <- with(ddaxlRateNotDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddaxlRateNotDis_logrt_lowerMask) # faster rating angry faces 'not disgusted' with lower mask

## lower-masked fearful faces
lmer_ddfxl_logrt_rateDisgust_lowerMask <- with(ddfxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_ddfxl_logrt_rateDisgust_lowerMask) # interaction **
# rated disgusted
ddfxlRateDis <- subset(ddfxl, ddfxl$rateEmotion=='incorrect')
lmer_ddfxlRateDis_logrt_lowerMask <- with(ddfxlRateDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddfxlRateDis_logrt_lowerMask) # ns
# rated not disgusted
ddfxlRateNotDis <- subset(ddfxl, ddfxl$rateEmotion=='correct')
lmer_ddfxlRateNotDis_logrt_lowerMask <- with(ddfxlRateNotDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddfxlRateNotDis_logrt_lowerMask) # faster rating fearful faces 'not disgusted' with lower mask


####---- RTs by emotion ratings * masks: upper mask vs. no mask ----####
## upper-masked angry faces
lmer_ddaxu_logrt_rateDisgust_upperMask <- with(ddaxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_ddaxu_logrt_rateDisgust_upperMask) # interaction ns
# rated disgusted
ddaxuRateDis <- subset(ddaxu, ddaxu$rateEmotion=='incorrect')
lmer_ddaxuRateDis_logrt_upperMask <- with(ddaxuRateDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddaxuRateDis_logrt_upperMask) # ns
# rated not disgusted
ddaxuRateNotDis <- subset(ddaxu, ddaxu$rateEmotion=='correct')
lmer_ddaxuRateNotDis_logrt_upperMask <- with(ddaxuRateNotDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddaxuRateNotDis_logrt_upperMask) # faster rating angry faces 'not disgusted' with upper mask

## upper-masked fearful faces
lmer_ddfxu_logrt_rateDisgust_upperMask <- with(ddfxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_ddfxu_logrt_rateDisgust_upperMask) # interaction ns
# rated disgusted
ddfxuRateDis <- subset(ddfxu, ddfxu$rateEmotion=='incorrect')
lmer_ddfxuRateDis_logrt_upperMask <- with(ddfxuRateDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddfxuRateDis_logrt_upperMask) # ns
# rated not disgusted
ddfxuRateNotDis <- subset(ddfxu, ddfxu$rateEmotion=='correct')
lmer_ddfxuRateNotDis_logrt_upperMask <- with(ddfxuRateNotDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddfxuRateNotDis_logrt_upperMask) # ns


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## angry faces
lmer_ddaul_logrt_rateDisgust_mask <- with(ddaul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_ddaul_logrt_rateDisgust_mask) # ns
# rated disgusted
ddaulRateDis <- subset(ddaul, ddaul$rateEmotion=='incorrect')
lmer_ddaulRateDis_logrt_mask <- with(ddaulRateDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddaulRateDis_logrt_mask) # ns
# rated not disgusted
ddaulRateNotDis <- subset(ddaul, ddaul$rateEmotion=='correct')
lmer_ddaulRateNotDis_logrt_mask <- with(ddaulRateNotDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddaulRateNotDis_logrt_mask) # faster to rate angry faces 'not disgusted' with upper mask vs. lower mask

## fearful faces
lmer_ddful_logrt_rateDisgust_mask <- with(ddful, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_ddful_logrt_rateDisgust_mask) # interaction **
# rated disgusted
ddfulRateDis <- subset(ddful, ddful$rateEmotion=='incorrect')
lmer_ddfulRateDis_logrt_mask <- with(ddfulRateDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddfulRateDis_logrt_mask) # faster to rate fearful faces 'disgusted' with upper mask vs. lower
# rated not disgusted
ddfulRateNotDis <- subset(ddful, ddful$rateEmotion=='correct')
lmer_ddfulRateNotDis_logrt_mask <- with(ddfulRateNotDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddfulRateNotDis_logrt_mask) # faster to rate fearful faces 'not disgusted' with lower mask vs. upper mask

## surprise faces
lmer_ddrul_logrt_rateDisgust_mask <- with(ddrul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_ddrul_logrt_rateDisgust_mask) # interaction ns
# rated disgusted
ddrulRateDis <- subset(ddrul, ddrul$rateEmotion=='incorrect')
lmer_ddrulRateDis_logrt_mask <- with(ddrulRateDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddrulRateDis_logrt_mask) # ns
# rated not disgusted
ddrulRateNotDis <- subset(ddrul, ddrul$rateEmotion=='correct')
lmer_ddrulRateNotDis_logrt_mask <- with(ddrulRateNotDis, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_ddrulRateNotDis_logrt_mask) # ns


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask ----####
## lower-masked angry faces
# interaction
confint.merMod(lmer_ddaxl_logrt_rateDisgust_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.006250714 0.06246952

# rated disgusted (incorrect): lower mask vs. no mask
confint.merMod(lmer_ddaxlRateDis_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03361072 0.02279974

# rated not disgusted (correct): lower mask vs. no mask
confint.merMod(lmer_ddaxlRateNotDis_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.00103269 0.03834462


## lower-masked fearful faces
# interaction
confint.merMod(lmer_ddfxl_logrt_rateDisgust_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.02002069 0.09664294

# rated disgusted (incorrect): lower mask vs. no mask
confint.merMod(lmer_ddfxlRateDis_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04308084 0.02687685

# rated not disgusted (correct): lower mask vs. no mask
confint.merMod(lmer_ddfxlRateNotDis_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.02812068 0.06306292


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: upper mask vs. no mask ----####
## upper-masked angry faces
# interaction
confint.merMod(lmer_ddaxu_logrt_rateDisgust_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper-0.003219632 0.07030159

# rated disgusted (incorrect): upper mask vs. no mask
confint.merMod(lmer_ddaxuRateDis_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.02776472 0.04627841

# rated not disgusted (correct): upper mask vs. no mask
confint.merMod(lmer_ddaxuRateNotDis_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper 0.03345934 0.06883461


## upper-masked fearful faces
# interaction
confint.merMod(lmer_ddfxu_logrt_rateDisgust_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.05430733 0.01247002

# rated disgusted (incorrect): upper mask vs. no mask
confint.merMod(lmer_ddfxuRateDis_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.000132963 0.04991494

# rated not disgusted (correct): upper mask vs. no mask
confint.merMod(lmer_ddfxuRateNotDis_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.0236299 0.02335104


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## masked angry faces
# interaction
confint.merMod(lmer_ddaul_logrt_rateDisgust_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.0562487 0.0152179

# rated disgusted (incorrect): lower mask vs. upper mask
confint.merMod(lmer_ddaulRateDis_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05226565 0.02331826

# rated not disgusted (correct): lower mask vs. upper mask
confint.merMod(lmer_ddaulRateNotDis_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05178736 -0.01893273


## masked fearful faces
# interaction
confint.merMod(lmer_ddful_logrt_rateDisgust_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.01963052 0.09454688

# rated disgusted (incorrect): lower mask vs. upper mask
confint.merMod(lmer_ddfulRateDis_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.06781173 -0.003981306

# rated not disgusted (correct): lower mask vs. upper mask
confint.merMod(lmer_ddfulRateNotDis_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.01109993 0.0535802


## masked surprised faces
# interaction
confint.merMod(lmer_ddrul_logrt_rateDisgust_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.02921528 0.04960599

# rated disgusted (incorrect): lower mask vs. upper mask
confint.merMod(lmer_ddrulRateDis_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04229713 0.05187423

# rated not disgusted (correct): lower mask vs. upper mask
confint.merMod(lmer_ddrulRateNotDis_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.004838316 0.026988


####---- plot RTs by emotion ratings * masks: lower mask vs. no mask ----####
# plot RTs by disgust ratings and lower-masked angry faces
cat_plot(lmer_ddaxl_logrt_rateDisgust_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Reds") + 
  theme_minimal(base_size=24)

# plot RTs by disgust ratings and lower-masked fear faces
cat_plot(lmer_ddfxl_logrt_rateDisgust_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Blues") + 
  theme_minimal(base_size=24)


####---- plot RTs by emotion ratings * masks: upper mask vs. no mask ----####
# plot RTs by disgust ratings and upper-masked angry faces
cat_plot(lmer_ddaxu_logrt_rateDisgust_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Reds") + 
  theme_minimal(base_size=24)


####---- plot RTs by emotion ratings * masks: lower mask vs. upper mask ----####
# plot RTs by disgust ratings and masked angry faces
cat_plot(lmer_ddaul_logrt_rateDisgust_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Reds") + 
  coord_cartesian(ylim=c(-3.05,-2.85)) +
  theme_minimal(base_size=24)

# plot RTs by disgust ratings and masked fearful faces
cat_plot(lmer_ddful_logrt_rateDisgust_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Blues") + 
  coord_cartesian(ylim=c(-3.05,-2.85)) +
  theme_minimal(base_size=24)
