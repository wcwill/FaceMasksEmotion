# faceMask study 2 RT analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating sadness
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
ds <- subset(d, emotionRating=='sad')

# recode face ratings
ds$rateEmotion[ds$rateEmotion == 1] <- 'correct'
ds$rateEmotion[ds$rateEmotion == 0] <- 'incorrect'
ds$rateEmotion <- as.factor(ds$rateEmotion)
ds$rateEmotion <- relevel(ds$rateEmotion, 'incorrect')

# further subset by facial expression
dsa <- subset(ds, expression=='angry')
dsd <- subset(ds, expression=='disgusted')
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
lmer_dsaxl_logrt_ratesad_lowerMask <- with(dsaxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsaxl_logrt_ratesad_lowerMask) # interaction **
# rated sad (incorrect)
dsaxlRateAng <- subset(dsaxl, dsaxl$rateEmotion=='incorrect')
lmer_dsaxlRateAng_logrt_lowerMask <- with(dsaxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsaxlRateAng_logrt_lowerMask) # ns
# rated not sad (correct)
dsaxlRateNotAng <- subset(dsaxl, dsaxl$rateEmotion=='correct')
lmer_dsaxlRateNotAng_logrt_lowerMask <- with(dsaxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsaxlRateNotAng_logrt_lowerMask) # faster rating angry faces 'not sad' with lower mask ***

## lower-masked disgusted faces
lmer_dsdxl_logrt_ratesad_lowerMask <- with(dsdxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsdxl_logrt_ratesad_lowerMask) # interaction ns
# rated sad (incorrect)
dsdxlRateAng <- subset(dsdxl, dsdxl$rateEmotion=='incorrect')
lmer_dsdxlRateAng_logrt_lowerMask <- with(dsdxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsdxlRateAng_logrt_lowerMask) # ns
# rated not sad (correct)
dsdxlRateNotAng <- subset(dsdxl, dsdxl$rateEmotion=='correct')
lmer_dsdxlRateNotAng_logrt_lowerMask <- with(dsdxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsdxlRateNotAng_logrt_lowerMask) # ns

## lower-masked happy faces
lmer_dshxl_logrt_ratesad_lowerMask <- with(dshxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dshxl_logrt_ratesad_lowerMask) # interaction ns
# rated sad (incorrect)
dshxlRateAng <- subset(dshxl, dshxl$rateEmotion=='incorrect')
lmer_dshxlRateAng_logrt_lowerMask <- with(dshxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dshxlRateAng_logrt_lowerMask) # ns
# rated not sad (correct)
dshxlRateNotAng <- subset(dshxl, dshxl$rateEmotion=='correct')
lmer_dshxlRateNotAng_logrt_lowerMask <- with(dshxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dshxlRateNotAng_logrt_lowerMask) # slower rating happy faces 'not sad' with lower mask ***

## lower-masked surprised faces
lmer_dsrxl_logrt_ratesad_lowerMask <- with(dsrxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsrxl_logrt_ratesad_lowerMask) # interaction ns
# rated sad (incorrect)
dsrxlRateAng <- subset(dsrxl, dsrxl$rateEmotion=='incorrect')
lmer_dsrxlRateAng_logrt_lowerMask <- with(dsrxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsrxlRateAng_logrt_lowerMask) # ns
# rated not sad (correct)
dsrxlRateNotAng <- subset(dsrxl, dsrxl$rateEmotion=='correct')
lmer_dsrxlRateNotAng_logrt_lowerMask <- with(dsrxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsrxlRateNotAng_logrt_lowerMask) # slower rating surprised faces 'not sad' with lower mask **



####---- RTs by emotion ratings * masks: upper mask vs. no mask ----####
## upper-masked angry faces
lmer_dsaxu_logrt_ratesad_upperMask <- with(dsaxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsaxu_logrt_ratesad_upperMask) # interaction ns
# rated sad (incorrect)
dsaxuRateAng <- subset(dsaxu, dsaxu$rateEmotion=='incorrect')
lmer_dsaxuRateAng_logrt_upperMask <- with(dsaxuRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsaxuRateAng_logrt_upperMask) # ns
# rated not sad (correct)
dsaxuRateNotAng <- subset(dsaxu, dsaxu$rateEmotion=='correct')
lmer_dsaxuRateNotAng_logrt_upperMask <- with(dsaxuRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsaxuRateNotAng_logrt_upperMask) # ns


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## angry faces
lmer_dsaul_logrt_ratesad_lowerMask <- with(dsaul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsaul_logrt_ratesad_lowerMask) # interaction **
# rated sad (incorrect)
dsaulRateAng <- subset(dsaul, dsaul$rateEmotion=='incorrect')
lmer_dsaulRateAng_logrt_lowerMask <- with(dsaulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsaulRateAng_logrt_lowerMask) # ns
# rated not sad (correct)
dsaulRateNotAng <- subset(dsaul, dsaul$rateEmotion=='correct')
lmer_dsaulRateNotAng_logrt_lowerMask <- with(dsaulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsaulRateNotAng_logrt_lowerMask) # faster rating angry faces 'not sad' with lower vs. upper mask ***

## disgusted faces
lmer_dsdul_logrt_ratesad_lowerMask <- with(dsdul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsdul_logrt_ratesad_lowerMask) # interaction *
# rated sad (incorrect)
dsdulRateAng <- subset(dsdul, dsdul$rateEmotion=='incorrect')
lmer_dsdulRateAng_logrt_lowerMask <- with(dsdulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsdulRateAng_logrt_lowerMask) # ns
# rated not sad (correct)
dsdulRateNotAng <- subset(dsdul, dsdul$rateEmotion=='correct')
lmer_dsdulRateNotAng_logrt_lowerMask <- with(dsdulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsdulRateNotAng_logrt_lowerMask) # slower rating disgusted faces 'not sad' with lower vs. upper mask *

## happy faces
lmer_dshul_logrt_ratesad_lowerMask <- with(dshul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dshul_logrt_ratesad_lowerMask) # interaction ns
# rated sad (incorrect)
dshulRateAng <- subset(dshul, dshul$rateEmotion=='incorrect')
lmer_dshulRateAng_logrt_lowerMask <- with(dshulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dshulRateAng_logrt_lowerMask) # ns
# rated not sad (correct)
dshulRateNotAng <- subset(dshul, dshul$rateEmotion=='correct')
lmer_dshulRateNotAng_logrt_lowerMask <- with(dshulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dshulRateNotAng_logrt_lowerMask) # slower rating happy faces 'not sad' with lower vs. upper mask ***

## surprised faces
lmer_dsrul_logrt_ratesad_lowerMask <- with(dsrul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dsrul_logrt_ratesad_lowerMask) # interaction ns
# rated sad (incorrect)
dsrulRateAng <- subset(dsrul, dsrul$rateEmotion=='incorrect')
lmer_dsrulRateAng_logrt_lowerMask <- with(dsrulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsrulRateAng_logrt_lowerMask) # ns
# rated not sad (correct)
dsrulRateNotAng <- subset(dsrul, dsrul$rateEmotion=='correct')
lmer_dsrulRateNotAng_logrt_lowerMask <- with(dsrulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dsrulRateNotAng_logrt_lowerMask) # slower rating surprised faces 'not sad' with lower vs. upper mask **


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask  ----####
## lower-masked angry faces
# interaction
confint.merMod(lmer_dsaxl_logrt_ratesad_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.01871099 0.104129

# rated sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dsaxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04521737 0.03284851 

# rated not sad (correct): lower mask vs. no mask
confint.merMod(lmer_dsaxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.02963451 0.06766247


## lower-masked disgusted faces
# interaction
confint.merMod(lmer_dsdxl_logrt_ratesad_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.03969606 0.05919075

# rated sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dsdxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04749638 0.05545828 

# rated not sad (correct): lower mask vs. no mask
confint.merMod(lmer_dsdxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0170206 0.01785839


## lower-masked happy faces
# interaction
confint.merMod(lmer_dshxl_logrt_ratesad_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.1615961 0.02292278

# rated sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dshxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1333872 0.2383516 

# rated not sad (correct): lower mask vs. no mask
confint.merMod(lmer_dshxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04347248 -0.01114936 


## lower-masked surprised faces
# interaction
confint.merMod(lmer_dsrxl_logrt_ratesad_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.1049823 0.01690726

# rated sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dsrxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05301236 0.1169694 

# rated not sad (correct): lower mask vs. no mask
confint.merMod(lmer_dsrxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04895169 -0.01201673 


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: upper mask vs. no mask ----####
## upper-masked angry faces
# interaction
confint.merMod(lmer_dsaxu_logrt_ratesad_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.0343411 0.04633999

# rated sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dsaxuRateAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.03470887 0.02329258 

# rated not sad (correct): lower mask vs. no mask
confint.merMod(lmer_dsaxuRateNotAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.02974487 0.0175955 


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## angry faces
# interaction
confint.merMod(lmer_dsaul_logrt_ratesad_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower 0.01364459 0.09825146

# rated sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dsaulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03539739 0.04735353 

# rated not sad (correct): lower mask vs. no mask
confint.merMod(lmer_dsaulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.03318574 0.07394962


## disgusted faces
# interaction
confint.merMod(lmer_dsdul_logrt_ratesad_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.1048597 0.008193914

# rated sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dsdulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.02112717 0.1125495 

# rated not sad (correct): lower mask vs. no mask
confint.merMod(lmer_dsdulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03739058 -0.002468981


## happy faces
# interaction
confint.merMod(lmer_dshul_logrt_ratesad_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.1319035 0.04100823

# rated sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dshulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.2004199 0.1893804 

# rated not sad (correct): lower mask vs. no mask
confint.merMod(lmer_dshulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04586312 -0.01419371 


## surprised faces
# interaction
confint.merMod(lmer_dsrul_logrt_ratesad_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.07683142 0.0426653

# rated sad (incorrect): lower mask vs. no mask
confint.merMod(lmer_dsrulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.07940289 0.07956125 

# rated not sad (correct): lower mask vs. no mask
confint.merMod(lmer_dsrulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0500967 -0.01248566 


####---- plot RTs by sad ratings and masks: lower mask vs. no mask ----####
# plot RTs by sad ratings and lower-masked angry faces
cat_plot(lmer_dsaxl_logrt_ratesad_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Reds") +
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)

# plot RTs by sad ratings and lower-masked happy faces
cat_plot(lmer_dshxl_logrt_ratesad_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greens") +
  coord_cartesian(ylim=c(-3.00,-2.80)) +
  theme_minimal(base_size=24)

# plot RTs by sad ratings and lower-masked surprised faces
cat_plot(lmer_dsrxl_logrt_ratesad_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Purples") +
  coord_cartesian(ylim=c(-3.00,-2.85)) +
  theme_minimal(base_size=24)


####---- plot RTs by sad ratings and masks: lower mask vs. upper mask ----####
# plot RTs by sad ratings and masked angry faces
cat_plot(lmer_dsaul_logrt_ratesad_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Reds") +
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)

# plot RTs by sad ratings and masked disgusted faces
cat_plot(lmer_dsdul_logrt_ratesad_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Oranges") +
  coord_cartesian(ylim=c(-3.05,-2.85)) +
  theme_minimal(base_size=24)

# plot RTs by sad ratings and masked happy faces
cat_plot(lmer_dshul_logrt_ratesad_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greens") +
  coord_cartesian(ylim=c(-2.95,-2.80)) +
  theme_minimal(base_size=24)

# plot RTs by sad ratings and masked surprised faces
cat_plot(lmer_dsrul_logrt_ratesad_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Purples") +
  coord_cartesian(ylim=c(-3.00,-2.85)) +
  theme_minimal(base_size=24)
