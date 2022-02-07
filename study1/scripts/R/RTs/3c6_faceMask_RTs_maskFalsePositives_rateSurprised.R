# faceMask RT analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating surprise
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
drh <- subset(dr, expression=='happy')
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
# happy faces
drhxu <- subset(drh, drh$mask != 'lower')
drhxu$mask <- factor(drhxu$mask, levels=c('none', 'upper'))
drhxl <- subset(drh, drh$mask != 'upper')
drhxl$mask <- factor(drhxl$mask, levels=c('none', 'lower'))
drhul <- subset(drh, drh$mask != 'none')
drhul$mask <- factor(drhul$mask, levels=c('upper', 'lower'))
# sad faces
drsxu <- subset(drs, drs$mask != 'lower')
drsxu$mask <- factor(drsxu$mask, levels=c('none', 'upper'))
drsxl <- subset(drs, drs$mask != 'upper')
drsxl$mask <- factor(drsxl$mask, levels=c('none', 'lower'))
drsul <- subset(drs, drs$mask != 'none')
drsul$mask <- factor(drsul$mask, levels=c('upper', 'lower'))


####---- RTs by emotion ratings * masks: lower mask vs. no mask ----####
## lower-masked fearful faces
lmer_drfxl_logrt_rateSurprised_lowerMask <- with(drfxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drfxl_logrt_rateSurprised_lowerMask) # interaction ns
# rated surprised (incorrect)
drfxlRateSur <- subset(drfxl, drfxl$rateEmotion=='incorrect')
lmer_drfxlRateSur_logrt_lowerMask <- with(drfxlRateSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drfxlRateSur_logrt_lowerMask) # faster rating fearful faces 'surprised' with lower mask
# rated not surprised (correct)
drfxlRateNotSur <- subset(drfxl, drfxl$rateEmotion=='correct')
lmer_drfxlRateNotSur_logrt_lowerMask <- with(drfxlRateNotSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drfxlRateNotSur_logrt_lowerMask) # faster rating fearful faces 'not surprised' with lower mask

## lower-masked sad faces
lmer_drsxl_logrt_rateSurprised_lowerMask <- with(drsxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drsxl_logrt_rateSurprised_lowerMask) # interaction ns
# rated surprised (incorrect)
drsxlRateSur <- subset(drsxl, drsxl$rateEmotion=='incorrect')
lmer_drsxlRateSur_logrt_lowerMask <- with(drsxlRateSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drsxlRateSur_logrt_lowerMask) # ns
# rated not surprised (correct)
drsxlRateNotSur <- subset(drsxl, drsxl$rateEmotion=='correct')
lmer_drsxlRateNotSur_logrt_lowerMask <- with(drsxlRateNotSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drsxlRateNotSur_logrt_lowerMask) # slower rating sad faces 'not surprised' with lower mask


####---- RTs by emotion ratings * masks: upper mask vs. no mask ----####
## upper-masked angry faces
lmer_draxu_logrt_rateSurprised_upperMask <- with(draxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_draxu_logrt_rateSurprised_upperMask) # interaction ns
# rated surprised (incorrect)
draxuRateSur <- subset(draxu, draxu$rateEmotion=='incorrect')
lmer_draxuRateSur_logrt_upperMask <- with(draxuRateSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_draxuRateSur_logrt_upperMask) # ns
# rated not surprised (correct)
draxuRateNotSur <- subset(draxu, draxu$rateEmotion=='correct')
lmer_draxuRateNotSur_logrt_upperMask <- with(draxuRateNotSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_draxuRateNotSur_logrt_upperMask) # ns

## upper-masked fearful faces
lmer_drfxu_logrt_rateSurprised_upperMask <- with(drfxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drfxu_logrt_rateSurprised_upperMask) # interaction ***
# rated surprised (incorrect)
drfxuRateSur <- subset(drfxu, drfxu$rateEmotion=='incorrect')
lmer_drfxuRateSur_logrt_upperMask <- with(drfxuRateSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drfxuRateSur_logrt_upperMask) # ns
# rated not surprised (correct)
drfxuRateNotSur <- subset(drfxu, drfxu$rateEmotion=='correct')
lmer_drfxuRateNotSur_logrt_upperMask <- with(drfxuRateNotSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drfxuRateNotSur_logrt_upperMask) # faster rating fearful faces 'not surprised' with upper mask


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## masked angry faces
lmer_draul_logrt_rateSurprised_mask <- with(draul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_draul_logrt_rateSurprised_mask) # interaction ns
# rated surprised (incorrect)
draulRateSur <- subset(draul, draul$rateEmotion=='incorrect')
lmer_draulRateSur_logrt_mask <- with(draulRateSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_draulRateSur_logrt_mask) # ns
# rated not surprised (correct)
draulRateNotSur <- subset(draul, draul$rateEmotion=='correct')
lmer_draulRateNotSur_logrt_mask <- with(draulRateNotSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_draulRateNotSur_logrt_mask) # faster rating angry faces 'not surprised' with upper mask vs. lower mask

## masked disgusted faces
lmer_drdul_logrt_rateSurprised_mask <- with(drdul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drdul_logrt_rateSurprised_mask) # interaction *
# rated surprised (incorrect)
drdulRateSur <- subset(drdul, drdul$rateEmotion=='incorrect')
lmer_drdulRateSur_logrt_mask <- with(drdulRateSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drdulRateSur_logrt_mask) # ns
# rated not surprised (correct)
drdulRateNotSur <- subset(drdul, drdul$rateEmotion=='correct')
lmer_drdulRateNotSur_logrt_mask <- with(drdulRateNotSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drdulRateNotSur_logrt_mask) # faster rating disgusted faces 'not surprised' with upper mask vs. lower mask

## masked fearful faces
lmer_drful_logrt_rateSurprised_mask <- with(drful, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drful_logrt_rateSurprised_mask) # interaction ***
# rated surprised (incorrect)
drfulRateSur <- subset(drful, drful$rateEmotion=='incorrect')
lmer_drfulRateSur_logrt_mask <- with(drfulRateSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drfulRateSur_logrt_mask) # faster rating fearful faces 'surprised' with lower mask vs. upper mask
# rated not surprised (correct)
drfulRateNotSur <- subset(drful, drful$rateEmotion=='correct')
lmer_drfulRateNotSur_logrt_mask <- with(drfulRateNotSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drfulRateNotSur_logrt_mask) # faster rating fearful faces 'not surprised' with upper mask vs. lower mask

## masked happy faces
lmer_drhul_logrt_rateSurprised_mask <- with(drhul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drhul_logrt_rateSurprised_mask) # interaction ns
# rated surprised (incorrect)
drhulRateSur <- subset(drhul, drhul$rateEmotion=='incorrect')
lmer_drhulRateSur_logrt_mask <- with(drhulRateSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drhulRateSur_logrt_mask) # ns
# rated not surprised (correct)
drhulRateNotSur <- subset(drhul, drhul$rateEmotion=='correct')
lmer_drhulRateNotSur_logrt_mask <- with(drhulRateNotSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drhulRateNotSur_logrt_mask) # faster rating happy faces 'not surprised' with upper mask vs. lower mask

## masked sad faces
lmer_drsul_logrt_rateSurprised_mask <- with(drsul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_drsul_logrt_rateSurprised_mask) # interaction ns
# rated surprised (incorrect)
drsulRateSur <- subset(drsul, drsul$rateEmotion=='incorrect')
lmer_drsulRateSur_logrt_mask <- with(drsulRateSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drsulRateSur_logrt_mask) # ns
# rated not surprised (correct)
drsulRateNotSur <- subset(drsul, drsul$rateEmotion=='correct')
lmer_drsulRateNotSur_logrt_mask <- with(drsulRateNotSur, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_drsulRateNotSur_logrt_mask) # faster rating sad faces 'not surprised' with upper mask vs. lower mask


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask ----####
## lower-masked fearful faces
# interaction
confint.merMod(lmer_drfxl_logrt_rateSurprised_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.05910499 0.01306788

# rated surprised (incorrect): lower mask vs. no mask
confint.merMod(lmer_drfxlRateSur_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.03149859 0.0763854

# rated not surprised (correct): lower mask vs. no mask
confint.merMod(lmer_drfxlRateNotSur_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.005600442 0.06107658


## lower-masked sad faces
# interaction
confint.merMod(lmer_drsxl_logrt_rateSurprised_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.08672694 0.02153237

# rated surprised (incorrect): lower mask vs. no mask
confint.merMod(lmer_drsxlRateSur_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04553788 0.09079446

# rated not surprised (correct): lower mask vs. no mask
confint.merMod(lmer_drsxlRateNotSur_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03925102 -0.00822482


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: upper mask vs. no mask ----####
## upper-masked angry faces
# interaction
confint.merMod(lmer_draxu_logrt_rateSurprised_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.05626202 0.0509919

# rated surprised (incorrect): upper mask vs. no mask
confint.merMod(lmer_draxuRateSur_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.07860867 0.09489301

# rated not surprised (correct): upper mask vs. no mask
confint.merMod(lmer_draxuRateNotSur_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.01346107 0.01837987


## upper-masked fearful faces
# interaction
confint.merMod(lmer_drfxu_logrt_rateSurprised_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper 0.05251702 0.1376754

# rated surprised (incorrect): upper mask vs. no mask
confint.merMod(lmer_drfxuRateSur_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.07621835 0.002450469

# rated not surprised (correct): upper mask vs. no mask
confint.merMod(lmer_drfxuRateNotSur_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper 0.04743399 0.08590289


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## masked angry faces
# interaction
confint.merMod(lmer_draul_logrt_rateSurprised_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.06545988 0.03705556

# rated surprised (incorrect): lower mask vs. upper mask
confint.merMod(lmer_draulRateSur_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1061448 0.06958022

# rated not surprised (correct): lower mask vs. upper mask
confint.merMod(lmer_draulRateNotSur_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03623608 -0.006590196


## masked disgusted faces
# interaction
confint.merMod(lmer_drdul_logrt_rateSurprised_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.09831302 -0.002611572

# rated surprised (incorrect): lower mask vs. upper mask
confint.merMod(lmer_drdulRateSur_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05178093 0.1066032

# rated not surprised (correct): lower mask vs. upper mask
confint.merMod(lmer_drdulRateNotSur_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03008209 -0.001867007


## masked fearful faces
# interaction
confint.merMod(lmer_drful_logrt_rateSurprised_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.1410762 -0.06346343

# rated surprised (incorrect): lower mask vs. upper mask
confint.merMod(lmer_drfulRateSur_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.05633525 0.1282475

# rated not surprised (correct): lower mask vs. upper mask
confint.merMod(lmer_drfulRateNotSur_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05550814 -0.01000622


## masked happy faces
# interaction
confint.merMod(lmer_drhul_logrt_rateSurprised_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.09021462 0.002091758

# rated surprised (incorrect): lower mask vs. upper mask
confint.merMod(lmer_drhulRateSur_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04520484 0.04652422

# rated not surprised (correct): lower mask vs. upper mask
confint.merMod(lmer_drhulRateNotSur_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.058484 -0.02239264


## masked sad faces
# interaction
confint.merMod(lmer_drsul_logrt_rateSurprised_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.08654677 0.01026327

# rated surprised (incorrect): lower mask vs. upper mask
confint.merMod(lmer_drsulRateSur_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.08673659 0.03752811

# rated not surprised (correct): lower mask vs. upper mask
confint.merMod(lmer_drsulRateNotSur_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.07490886 -0.04389571


####---- plot RTs by emotion ratings * masks: lower mask vs. no mask ----####
# plot RTs by surprised ratings and lower-masked fearful faces
cat_plot(lmer_drfxl_logrt_rateSurprised_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Blues") + 
  coord_cartesian(ylim=c(-3.05,-2.85)) +
  theme_minimal(base_size=24)

# plot RTs by surprised ratings and lower-masked sad faces
cat_plot(lmer_drsxl_logrt_rateSurprised_lowerMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Greys") + 
  coord_cartesian(ylim=c(-2.95,-2.80)) +
  theme_minimal(base_size=24)


####---- plot RTs by emotion ratings * masks: upper mask vs. no mask ----####
# plot RTs by surprised ratings and upper-masked fearful faces
cat_plot(lmer_drfxu_logrt_rateSurprised_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Blues") + 
  coord_cartesian(ylim=c(-3.05,-2.85)) +
  theme_minimal(base_size=24)


####---- plot RTs by emotion ratings * masks: lower mask vs. upper mask ----####
# plot RTs by surprised ratings and masked angry faces
cat_plot(lmer_draul_logrt_rateSurprised_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Reds") + 
  coord_cartesian(ylim=c(-3.00,-2.80)) +
  theme_minimal(base_size=24)

# plot RTs by surprised ratings and masked disgusted faces
cat_plot(lmer_drdul_logrt_rateSurprised_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Oranges") + 
  coord_cartesian(ylim=c(-3.00,-2.85)) +
  theme_minimal(base_size=24)

# plot RTs by surprised ratings and masked fearful faces
cat_plot(lmer_drful_logrt_rateSurprised_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Blues") + 
  coord_cartesian(ylim=c(-3.05,-2.85)) +
  theme_minimal(base_size=24)

# plot RTs by surprised ratings and masked happy faces
cat_plot(lmer_drhul_logrt_rateSurprised_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Greens") + 
  coord_cartesian(ylim=c(-2.95,-2.80)) +
  theme_minimal(base_size=24)

# plot RTs by surprised ratings and masked sad faces
cat_plot(lmer_drsul_logrt_rateSurprised_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Greys") + 
  coord_cartesian(ylim=c(-2.95,-2.80)) +
  theme_minimal(base_size=24)
