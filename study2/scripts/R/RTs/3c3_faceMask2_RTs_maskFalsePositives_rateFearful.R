# faceMask study 2 RT analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating fear
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
df <- subset(d, emotionRating=='fearful')

# recode face ratings
df$rateEmotion[df$rateEmotion == 1] <- 'correct'
df$rateEmotion[df$rateEmotion == 0] <- 'incorrect'
df$rateEmotion <- as.factor(df$rateEmotion)
df$rateEmotion <- relevel(df$rateEmotion, 'incorrect')

# further subset by facial expression
dfa <- subset(df, expression=='angry')
dfs <- subset(df, expression=='sad')
dfr <- subset(df, expression=='surprised')

# further subset for masks of interest and re-level
# angry faces
dfaxu <- subset(dfa, dfa$mask != 'lower')
dfaxu$mask <- factor(dfaxu$mask, levels=c('none', 'upper'))
dfaxl <- subset(dfa, dfa$mask != 'upper')
dfaxl$mask <- factor(dfaxl$mask, levels=c('none', 'lower'))
dfaul <- subset(dfa, dfa$mask != 'none')
dfaul$mask <- factor(dfaul$mask, levels=c('upper', 'lower'))
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
lmer_dfaxl_logrt_ratefearful_lowerMask <- with(dfaxl, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfaxl_logrt_ratefearful_lowerMask) # interaction ns
# rated fearful (incorrect)
dfaxlRateAng <- subset(dfaxl, dfaxl$rateEmotion=='incorrect')
lmer_dfaxlRateAng_logrt_lowerMask <- with(dfaxlRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfaxlRateAng_logrt_lowerMask) # ns
# rated not fearful (correct)
dfaxlRateNotAng <- subset(dfaxl, dfaxl$rateEmotion=='correct')
lmer_dfaxlRateNotAng_logrt_lowerMask <- with(dfaxlRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfaxlRateNotAng_logrt_lowerMask) # ns


####---- RTs by emotion ratings * masks: upper mask vs. no mask ----####
## upper-masked sad faces
lmer_dfsxu_logrt_ratefearful_upperMask <- with(dfsxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfsxu_logrt_ratefearful_upperMask) # interaction ns
# rated fearful (incorrect)
dfsxuRateAng <- subset(dfsxu, dfsxu$rateEmotion=='incorrect')
lmer_dfsxuRateAng_logrt_upperMask <- with(dfsxuRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfsxuRateAng_logrt_upperMask) # ns
# rated not fearful (correct)
dfsxuRateNotAng <- subset(dfsxu, dfsxu$rateEmotion=='correct')
lmer_dfsxuRateNotAng_logrt_upperMask <- with(dfsxuRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfsxuRateNotAng_logrt_upperMask) # faster rating sad faces 'not fearful' with upper mask **

## upper-masked surprised faces
lmer_dfrxu_logrt_ratefearful_upperMask <- with(dfrxu, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfrxu_logrt_ratefearful_upperMask) # interaction **
# rated fearful (incorrect)
dfrxuRateAng <- subset(dfrxu, dfrxu$rateEmotion=='incorrect')
lmer_dfrxuRateAng_logrt_upperMask <- with(dfrxuRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfrxuRateAng_logrt_upperMask) # slower rating surprised faces 'fearful' with upper mask *
# rated not fearful (correct)
dfrxuRateNotAng <- subset(dfrxu, dfrxu$rateEmotion=='correct')
lmer_dfrxuRateNotAng_logrt_upperMask <- with(dfrxuRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfrxuRateNotAng_logrt_upperMask) # faster rating surprised faces 'not fearful' with upper mask *


####---- RTs by emotion ratings * masks: lower mask vs. upper mask ----####
## angry faces
lmer_dfaul_logrt_ratefearful_mask <- with(dfaul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfaul_logrt_ratefearful_mask) # interaction ns
# rated fearful (incorrect)
dfaulRateAng <- subset(dfaul, dfaul$rateEmotion=='incorrect')
lmer_dfaulRateAng_logrt_mask <- with(dfaulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfaulRateAng_logrt_mask) # ns
# rated not fearful (correct)
dfaulRateNotAng <- subset(dfaul, dfaul$rateEmotion=='correct')
lmer_dfaulRateNotAng_logrt_mask <- with(dfaulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfaulRateNotAng_logrt_mask) # slower rating angry faces 'not fearful' with lower vs. upper mask ***
 
## sad faces
lmer_dfsul_logrt_ratefearful_mask <- with(dfsul, lmer(logrt ~ rateEmotion * mask + (1 | subject)))
summary(lmer_dfsul_logrt_ratefearful_mask) # interaction ns
# rated fearful
dfsulRateAng <- subset(dfsul, dfsul$rateEmotion=='incorrect')
lmer_dfsulRateAng_logrt_mask <- with(dfsulRateAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfsulRateAng_logrt_mask) # ns
# rated not fearful
dfsulRateNotAng <- subset(dfsul, dfsul$rateEmotion=='correct')
lmer_dfsulRateNotAng_logrt_mask <- with(dfsulRateNotAng, lmer(logrt ~ mask + (1 | subject)))
summary(lmer_dfsulRateNotAng_logrt_mask) # slower rating sad faces 'not fearful' with lower vs. upper mask *


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. no mask ----####
## lower-masked angry faces
# interaction
confint.merMod(lmer_dfaxl_logrt_ratefearful_lowerMask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.08176043 0.01531808

# rated fearful (incorrect): lower mask vs. no mask
confint.merMod(lmer_dfaxlRateAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.01489886 0.0764995  

# rated not fearful (correct): lower mask vs. no mask
confint.merMod(lmer_dfaxlRateNotAng_logrt_lowerMask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.02897632 0.006113089 


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: upper mask vs. no mask ----####
## upper-masked sad faces
# interaction
confint.merMod(lmer_dfsxu_logrt_ratefearful_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper -0.04024828 0.05230157

# rated fearful (incorrect): lower mask vs. no mask
confint.merMod(lmer_dfsxuRateAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.02401696 0.08057408 

# rated not fearful (correct): lower mask vs. no mask
confint.merMod(lmer_dfsxuRateNotAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper 0.009861116 0.04181499 

## upper-masked surprised faces
# interaction
confint.merMod(lmer_dfrxu_logrt_ratefearful_upperMask, parm=c('rateEmotioncorrect:maskupper'), method='boot', nsim=1000)
# rateEmotioncorrect:maskupper 0.02003366 0.09806474

# rated fearful (incorrect): lower mask vs. no mask
confint.merMod(lmer_dfrxuRateAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper -0.0470042 -0.00163853 

# rated not fearful (correct): lower mask vs. no mask
confint.merMod(lmer_dfrxuRateNotAng_logrt_upperMask, parm=c('maskupper'), method='boot', nsim=1000)
# maskupper 0.005952609 0.06728531 


####---- estimate 95% confidence intervals for RTs by emotion ratings and masks: lower mask vs. upper mask ----####
## masked angry faces
# interaction
confint.merMod(lmer_dfaul_logrt_ratefearful_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.1059113 -0.0003664258

# rated fearful (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dfaulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.02175338 0.07575489

# rated not fearful (correct): lower mask vs. upper mask
confint.merMod(lmer_dfaulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.0494141 -0.01650109

## masked sad faces
# interaction
confint.merMod(lmer_dfsul_logrt_ratefearful_mask, parm=c('rateEmotioncorrect:masklower'), method='boot', nsim=1000)
# rateEmotioncorrect:masklower -0.07136063 0.02035965

# rated fearful (incorrect): lower mask vs. upper mask
confint.merMod(lmer_dfsulRateAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05865287 0.0435058

# rated not fearful (correct): lower mask vs. upper mask
confint.merMod(lmer_dfsulRateNotAng_logrt_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.03983399 -0.003689602


####---- plot RTs by fearful ratings and masks: upper mask vs.no mask ----####
# plot RTs by fearful ratings and upper-masked sad faces
cat_plot(lmer_dfsxu_logrt_ratefearful_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Greys") +
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)

# plot RTs by fearful ratings and upper-masked surprised faces
cat_plot(lmer_dfrxu_logrt_ratefearful_upperMask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Purples") +
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)


####---- plot RTs by fearful ratings and masks: lower mask vs. upper mask ----####
# plot RTs by fearful ratings and masked angry faces
cat_plot(lmer_dfaul_logrt_ratefearful_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95, 
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)", 
         legend.main = "rating", color.class = "Reds") +
  coord_cartesian(ylim=c(-3.05,-2.85)) +
  theme_minimal(base_size=24)

# plot RTs by fearful ratings and masked sad faces
cat_plot(lmer_dfsul_logrt_ratefearful_mask, pred = mask, modx = rateEmotion, interval = TRUE, int.width = .95,
         dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "mask", y.label = "Mean -log(RT)",
         legend.main = "rating", color.class = "Greys") +
  coord_cartesian(ylim=c(-3.05,-2.90)) +
  theme_minimal(base_size=24)
