# faceMask RT analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating fear
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
df <- subset(d, emotionRating=='fearful')

# recode face ratings
df$rateEmotion[df$rateEmotion == 1] <- 'correct'
df$rateEmotion[df$rateEmotion == 0] <- 'incorrect'
df$rateEmotion <- as.factor(df$rateEmotion)
df$rateEmotion <- relevel(df$rateEmotion, 'incorrect')

# further subset by facial expression
dfa <- subset(df, expression=='angry')
dfd <- subset(df, expression=='disgusted')
dfh <- subset(df, expression=='happy')
dfs <- subset(df, expression=='sad')
dfr <- subset(df, expression=='surprised')

### further subset for masks of interest and re-level
# angry faces
dfaxu <- subset(dfa, dfa$mask != 'lower')
dfaxu$mask <- factor(dfaxu$mask, levels=c('none', 'upper'))
dfaxl <- subset(dfa, dfa$mask != 'upper')
dfaxl$mask <- factor(dfaxl$mask, levels=c('none', 'lower'))
dfaul <- subset(dfa, dfa$mask != 'none')
dfaul$mask <- factor(dfaul$mask, levels=c('upper', 'lower'))
# disgusted faces
dfdxu <- subset(dfd, dfd$mask != 'lower')
dfdxu$mask <- factor(dfdxu$mask, levels=c('none', 'upper'))
dfdxl <- subset(dfd, dfd$mask != 'upper')
dfdxl$mask <- factor(dfdxl$mask, levels=c('none', 'lower'))
dfdul <- subset(dfd, dfd$mask != 'none')
dfdul$mask <- factor(dfdul$mask, levels=c('upper', 'lower'))
# happy faces
dfhxu <- subset(dfh, dfh$mask != 'lower')
dfhxu$mask <- factor(dfhxu$mask, levels=c('none', 'upper'))
dfhxl <- subset(dfh, dfh$mask != 'upper')
dfhxl$mask <- factor(dfhxl$mask, levels=c('none', 'lower'))
dfhul <- subset(dfh, dfh$mask != 'none')
dfhul$mask <- factor(dfhul$mask, levels=c('upper', 'lower'))
# sad faces
dfsxu <- subset(dfs, dfs$mask != 'lower')
dfsxu$mask <- factor(dfsxu$mask, levels=c('none', 'upper'))
dfsxl <- subset(dfs, dfs$mask != 'upper')
dfsxl$mask <- factor(dfsxl$mask, levels=c('none', 'lower'))
dfsul <- subset(dfs, dfs$mask != 'none')
dfsul$mask <- factor(dfsul$mask, levels=c('upper', 'lower'))
# surprised faces
dfrxu <- subset(dfr, dfr$mask != 'lower')
dfrxu$mask <- factor(dfrxu$mask, levels=c('none', 'upper'))
dfrxl <- subset(dfr, dfr$mask != 'upper')
dfrxl$mask <- factor(dfrxl$mask, levels=c('none', 'lower'))
dfrul <- subset(dfr, dfr$mask != 'none')
dfrul$mask <- factor(dfrul$mask, levels=c('upper', 'lower'))


####---- RTs by emotion ratings * masks: lower mask vs. no mask ----####
## lower-masked angry faces
lmer_dfaxl_logrt_rateFearful_lowerMask <- with(dfaxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfaxl_logrt_rateFearful_lowerMask) # interaction ns
# rated fearful
dfaxlRateFear <- subset(dfaxl, dfaxl$rateEmotion=='incorrect')
lmer_dfaxlRateFear_logrt_lowerMask <- with(dfaxlRateFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfaxlRateFear_logrt_lowerMask) # ns
# rated not fearful
dfaxlRateNotFear <- subset(dfaxl, dfaxl$rateEmotion=='correct')
lmer_dfaxlRateNotFear_logrt_lowerMask <- with(dfaxlRateNotFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfaxlRateNotFear_logrt_lowerMask) # ns

## lower-masked sad faces
lmer_dfsxl_logrt_rateFearful_lowerMask <- with(dfsxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfsxl_logrt_rateFearful_lowerMask) # interaction ns
# rated fearful
dfsxlRateFear <- subset(dfsxl, dfsxl$rateEmotion=='incorrect')
lmer_dfsxlRateFear_logrt_lowerMask <- with(dfsxlRateFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfsxlRateFear_logrt_lowerMask) # ns
# rated not fearful
dfsxlRateNotFear <- subset(dfsxl, dfsxl$rateEmotion=='correct')
lmer_dfsxlRateNotFear_logrt_lowerMask <- with(dfsxlRateNotFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfsxlRateNotFear_logrt_lowerMask) # ns


####---- RTs by emotion ratings * masks: upper mask vs. no mask ----####
## upper-masked angry faces
lmer_dfaxu_logrt_rateFearful_upperMask <- with(dfaxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfaxu_logrt_rateFearful_upperMask) # interaction ns
# rated fearful
dfaxuRateFear <- subset(dfaxu, dfaxu$rateEmotion=='incorrect')
lmer_dfaxuRateFear_logrt_upperMask <- with(dfaxuRateFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfaxuRateFear_logrt_upperMask) # faster rating angry faces 'fearful' with upper mask
# rated not fearful
dfaxuRateNotFear <- subset(dfaxu, dfaxu$rateEmotion=='correct')
lmer_dfaxuRateNotFear_logrt_upperMask <- with(dfaxuRateNotFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfaxuRateNotFear_logrt_upperMask) # faster rating angry faces 'not fearful' with upper mask

## upper-masked sad faces
lmer_dfsxu_logrt_rateFearful_upperMask <- with(dfsxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfsxu_logrt_rateFearful_upperMask) # interaction ns
# rated fearful
dfsxuRateFear <- subset(dfsxu, dfsxu$rateEmotion=='incorrect')
lmer_dfsxuRateFear_logrt_upperMask <- with(dfsxuRateFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfsxuRateFear_logrt_upperMask) # ns
# rated not fearful
dfsxuRateNotFear <- subset(dfsxu, dfsxu$rateEmotion=='correct')
lmer_dfsxuRateNotFear_logrt_upperMask <- with(dfsxuRateNotFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfsxuRateNotFear_logrt_upperMask) # faster rating sad faces 'not fearful' with upper mask

## upper-masked surprised faces
lmer_dfrxu_logrt_rateFearful_upperMask <- with(dfrxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfrxu_logrt_rateFearful_upperMask) # interaction **
# rated fearful
dfrxuRateFear <- subset(dfrxu, dfrxu$rateEmotion=='incorrect')
lmer_dfrxuRateFear_logrt_upperMask <- with(dfrxuRateFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfrxuRateFear_logrt_upperMask) # ns
# rated not fearful
dfrxuRateNotFear <- subset(dfrxu, dfrxu$rateEmotion=='correct')
lmer_dfrxuRateNotFear_logrt_upperMask <- with(dfrxuRateNotFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfrxuRateNotFear_logrt_upperMask) # faster rating surprised faces 'not fearful' with upper mask


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## angry faces
lmer_dfaul_logrt_rateFearful_mask <- with(dfaul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfaul_logrt_rateFearful_mask) # interaction ns
# rated fearful (incorrect)
dfaulRateFear <- subset(dfaul, dfaul$rateEmotion=='incorrect')
lmer_dfaulRateFear_logrt_mask <- with(dfaulRateFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfaulRateFear_logrt_mask) # ns
# rated not fearful (correct)
dfaulRateNotFear <- subset(dfaul, dfaul$rateEmotion=='correct')
lmer_dfaulRateNotFear_logrt_mask <- with(dfaulRateNotFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfaulRateNotFear_logrt_mask) # faster rating angry faces 'not fearful' with upper mask vs. lower mask

## happy faces
lmer_dfhul_logrt_rateFearful_mask <- with(dfhul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfhul_logrt_rateFearful_mask) # interaction ns
# rated fearful (incorrect)
dfhulRateFear <- subset(dfhul, dfhul$rateEmotion=='incorrect')
lmer_dfhulRateFear_logrt_mask <- with(dfhulRateFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfhulRateFear_logrt_mask) # ns
# rated not fearful (correct)
dfhulRateNotFear <- subset(dfhul, dfhul$rateEmotion=='correct')
lmer_dfhulRateNotFear_logrt_mask <- with(dfhulRateNotFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfhulRateNotFear_logrt_mask) # faster rating happy faces 'not fearful' with upper mask vs. lower mask

## sad faces
lmer_dfsul_logrt_rateFearful_mask <- with(dfsul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfsul_logrt_rateFearful_mask) # interaction ns
# rated fearful (incorrect)
dfsulRateFear <- subset(dfsul, dfsul$rateEmotion=='incorrect')
lmer_dfsulRateFear_logrt_mask <- with(dfsulRateFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfsulRateFear_logrt_mask) # ns
# rated not fearful (correct)
dfsulRateNotFear <- subset(dfsul, dfsul$rateEmotion=='correct')
lmer_dfsulRateNotFear_logrt_mask <- with(dfsulRateNotFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfsulRateNotFear_logrt_mask) # faster rating sad faces 'not fearful' with upper mask vs. lower mask

## surprised faces
lmer_dfrul_logrt_rateFearful_mask <- with(dfrul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfrul_logrt_rateFearful_mask) # interaction ns
# rated fearful (incorrect)
dfrulRateFear <- subset(dfrul, dfrul$rateEmotion=='incorrect')
lmer_dfrulRateFear_logrt_mask <- with(dfrulRateFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfrulRateFear_logrt_mask) # ns
# rated not fearful (correct)
dfrulRateNotFear <- subset(dfrul, dfrul$rateEmotion=='correct')
lmer_dfrulRateNotFear_logrt_mask <- with(dfrulRateNotFear, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfrulRateNotFear_logrt_mask) # ns


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask ----####
## lower-masked angry faces
# interaction
confint.merMod(lmer_dfaxl_logrt_rateFearful_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.07087408 0.003839071

# rated fearful (incorrect): lower mask vs. no mask
confint.merMod(lmer_dfaxlRateFear_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.01542222 0.06216271

# rated not fearful (correct): lower mask vs. no mask
confint.merMod(lmer_dfaxlRateNotFear_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0247189 0.01018646


## lower-masked sad faces
# interaction
confint.merMod(lmer_dfsxl_logrt_rateFearful_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.04009798 0.03360469

# rated fearful (incorrect): lower mask vs. no mask
confint.merMod(lmer_dfsxlRateFear_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03380521 0.04446866

# rated not fearful (correct): lower mask vs. no mask
confint.merMod(lmer_dfsxlRateNotFear_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.02122353 0.0153835


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: upper mask vs. no mask ----####
## upper-masked angry faces
# interaction
confint.merMod(lmer_dfaxu_logrt_rateFearful_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.04015984 0.03632854

# rated fearful (incorrect): upper mask vs. no mask
confint.merMod(lmer_dfaxuRateFear_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper 0.005969013 0.08374396

# rated not fearful (correct): upper mask vs. no mask
confint.merMod(lmer_dfaxuRateNotFear_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper 0.02338939 0.05562087


## upper-masked sad faces
# interaction
confint.merMod(lmer_dfsxu_logrt_rateFearful_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.01278651 0.06363411

# rated fearful (incorrect): upper mask vs. no mask
confint.merMod(lmer_dfsxuRateFear_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.04076207 0.04109392

# rated not fearful (correct): upper mask vs. no mask
confint.merMod(lmer_dfsxuRateNotFear_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper 0.007780576 0.04356623


## upper-masked surprised faces
# interaction
confint.merMod(lmer_dfrxu_logrt_rateFearful_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper 0.0155573 0.08564488

# rated fearful (incorrect): upper mask vs. no mask
confint.merMod(lmer_dfrxuRateFear_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.03753648 0.009840028

# rated not fearful (correct): upper mask vs. no mask
confint.merMod(lmer_dfrxuRateNotFear_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper 0.01220249 0.06349225


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## masked angry faces
# interaction
confint.merMod(lmer_dfaul_logrt_rateFearful_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.06444498 0.01704105

# rated fearful (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dfaulRateFear_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.06654773 0.01729685

# rated not fearful (correct): lower mask vs. upper mask
confint.merMod(lmer_dfaulRateNotFear_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05986538 -0.02682164


## masked happy faces
# interaction
confint.merMod(lmer_dfhul_logrt_rateFearful_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.09096951 0.01717972

# rated fearful (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dfhulRateFear_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.08919483 0.07849487

# rated not fearful (correct): lower mask vs. upper mask
confint.merMod(lmer_dfhulRateNotFear_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03306707 -0.002489839


## masked sad faces
# interaction
confint.merMod(lmer_dfsul_logrt_rateFearful_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.05160608 0.01964895

# rated fearful (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dfsulRateFear_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05175015 0.03048391

# rated not fearful (correct): lower mask vs. upper mask
confint.merMod(lmer_dfsulRateNotFear_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.04483297 -0.008998787


## masked surprised faces
# interaction
confint.merMod(lmer_dfrul_logrt_rateFearful_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.03341805 0.04214814

# rated fearful (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dfrulRateFear_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.01798987 0.03347614

# rated not fearful (correct): lower mask vs. upper mask
confint.merMod(lmer_dfrulRateNotFear_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.01513513 0.03831126


####---- plot RTs by emotion ratings * masks: upper mask vs. no mask ----####
# plot RTs by fear ratings and upper-masked angry faces
cat_plot(lmer_dfaxu_logrt_rateFearful_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Reds") + 
  coord_cartesian(ylim=c(-3.05,-2.85)) +
  theme_minimal(base_size=24)

# plot RTs by fear ratings and upper-masked sad faces
cat_plot(lmer_dfsxu_logrt_rateFearful_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Greys") + 
  coord_cartesian(ylim=c(-3.00,-2.90)) +
  theme_minimal(base_size=24)

# plot RTs by fear ratings and upper-masked surprised faces
cat_plot(lmer_dfrxu_logrt_rateFearful_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Purples") + 
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)


####---- plot RTs by emotion ratings * masks: lower mask vs. upper mask ----####
# plot RTs by fear ratings and masked angry faces
cat_plot(lmer_dfaul_logrt_rateFearful_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Reds") + 
  coord_cartesian(ylim=c(-3.00,-2.85)) + 
  theme_minimal(base_size=24)

# plot RTs by fear ratings and masked happy faces
cat_plot(lmer_dfhul_logrt_rateFearful_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Greens") + 
  coord_cartesian(ylim=c(-2.95,-2.80)) + 
  theme_minimal(base_size=24)

# plot RTs by fear ratings and masked sad faces
cat_plot(lmer_dfsul_logrt_rateFearful_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean log(RT)", 
         legend.main = "rating", color.class = "Greys") + 
  coord_cartesian(ylim=c(-3.05,-2.90)) + 
  theme_minimal(base_size=24)
