# faceMask face ratings analysis script: mask analyses of false negatives (failing to ID correct emotion)
# note: need to have run '1_faceMask_proc.R' script first
# 7/31/20
library(lme4)
library(lmerTest)
library(ggplot2)
library(RColorBrewer)

####---- setup ----####
# subset RTs > 100 ms (see Luce, 1986; Whelan, 2008)
d <- subset(d, rt >= 100)

# subset all congruent trials for main effect analyses
dxx <- subset(d, emotionRating==expression)

# subset by emotion rating type and expression
daa <- subset(dxx, emotionRating=='angry' & expression=='angry')
ddd <- subset(dxx, emotionRating=='disgusted' & expression=='disgusted')
dff <- subset(dxx, emotionRating=='fearful' & expression=='fearful')
dhh <- subset(dxx, emotionRating=='happy' & expression=='happy')
dss <- subset(dxx, emotionRating=='sad' & expression=='sad')
drr <- subset(dxx, emotionRating=='surprised' & expression=='surprised')

# further subset for lower vs. upper mask contrasts
dxxul <- subset(dxx, dxx$mask != 'none')
dxxul$mask <- factor(dxxul$mask, levels=c('upper', 'lower'))
daaul <- subset(daa, daa$mask != 'none')
daaul$mask <- factor(daaul$mask, levels=c('upper', 'lower'))
dddul <- subset(ddd, ddd$mask != 'none')
dddul$mask <- factor(dddul$mask, levels=c('upper', 'lower'))
dfful <- subset(dff, dff$mask != 'none')
dfful$mask <- factor(dfful$mask, levels=c('upper', 'lower'))
dhhul <- subset(dhh, dhh$mask != 'none')
dhhul$mask <- factor(dhhul$mask, levels=c('upper', 'lower'))
dssul <- subset(dss, dss$mask != 'none')
dssul$mask <- factor(dssul$mask, levels=c('upper', 'lower'))
drrul <- subset(drr, drr$mask != 'none')
drrul$mask <- factor(drrul$mask, levels=c('upper', 'lower'))


####---- emotion ratings by masks: IDing correct emotions ----####
## all ratings (congruent faces)
glmer_dxx_rateEmotion_mask <- with(dxx, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dxx_rateEmotion_mask) # less likely to rate faces correctly with either mask vs. no mask
glmer_dxxul_rateEmotion_mask <- with(dxxul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dxxul_rateEmotion_mask) # no difference between upper/lower masks rating faces correctly

## rating anger (angry faces)
glmer_daa_rateEmotion_mask <- with(daa, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_daa_rateEmotion_mask) # less likely to rate faces 'angry' with upper mask vs. no mask
glmer_daaul_rateEmotion_mask <- with(daaul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_daaul_rateEmotion_mask) # more likely to rate faces 'angry' with lower mask vs. upper mask

## rating disgust (disgusted faces)
glmer_ddd_rateEmotion_mask <- with(ddd, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_ddd_rateEmotion_mask) # less likely to rate faces 'disgusted' with lower mask vs. no mask
glmer_dddul_rateEmotion_mask <- with(dddul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dddul_rateEmotion_mask) # less likely to rate faces 'disgusted' with lower mask vs. upper mask

## rating fear (fearful faces)
glmer_dff_rateEmotion_mask <- with(dff, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dff_rateEmotion_mask) # less likely to rate faces 'fearful' with either mask (stronger for upper) vs. no mask
glmer_dfful_rateEmotion_mask <- with(dfful, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dfful_rateEmotion_mask) # more likely to rate faces 'fearful' with lower mask vs. upper mask

## rating happy (happy faces)
glmer_dhh_rateEmotion_mask <- with(dhh, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhh_rateEmotion_mask) # less likely to rate faces 'happy' with lower mask vs. no mask
glmer_dhhul_rateEmotion_mask <- with(dhhul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhhul_rateEmotion_mask) # less likely to rate faces 'happy' with lower mask vs. upper mask

## rating sadness (sad faces)
glmer_dss_rateEmotion_mask <- with(dss, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dss_rateEmotion_mask) # less likely to rate faces 'sad' with either mask (stronger for lower) vs. no mask
glmer_dssul_rateEmotion_mask <- with(dssul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dssul_rateEmotion_mask) # less likely to rate faces 'sad' with lower mask vs. upper mask

## rating surprise (surprised faces)
glmer_drr_rateEmotion_mask <- with(drr, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drr_rateEmotion_mask) # less likely to rate faces 'surprised' with either mask (equally strong) vs. no mask
glmer_drrul_rateEmotion_mask <- with(drrul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drrul_rateEmotion_mask) # no difference between upper/lower masks rating faces surprised 


####---- estimate 95% confidence intervals for emotion ratings by masks: IDing correct emotions ----####
## all ratings (congruent faces)
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dxx_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.9204417 -0.6037899
# maskupper -0.8912149 -0.5638000

# lower mask vs. upper mask
confint.merMod(glmer_dxxul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.2056201 0.134978


## rating anger (angry faces)
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_daa_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.2791304  0.4441265
# maskupper -1.9282072 -1.1777409

# lower mask vs. upper mask
confint.merMod(glmer_daaul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 1.29135 1.954419


## rating disgust (disgusted faces)
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_ddd_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.8249142 -1.35346416
# maskupper -0.4364114  0.06139805

# lower mask vs. upper mask
confint.merMod(glmer_dddul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.658978 -1.115473


## rating fear (fearful faces)
# lower mask vs. no mask and upper mask vs. no mask
confint.merMod(glmer_dff_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.8825027 -0.372671
# maskupper -1.7282268 -1.213327

# lower mask vs. upper mask
confint.merMod(glmer_dfful_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.588848 1.072936


## rating happy (happy faces)
# lower mask vs. no mask and upper mask vs. no mask
confint.merMod(glmer_dhh_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.0735990 -0.4567624
# maskupper -0.5413818  0.1008516

# lower mask vs. upper mask
confint.merMod(glmer_dhhul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.849821 -0.2345896


## rating sadness (sad faces)
# lower mask vs. no mask and upper mask vs. no mask
confint.merMod(glmer_dss_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.5565712 -0.91914940
# maskupper -0.7411944 -0.08747714

# lower mask vs. upper mask
confint.merMod(glmer_dssul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.101214 -0.4978957


## rating surprise (surprised faces)
# lower mask vs. no mask and upper mask vs. no mask
confint.merMod(glmer_drr_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.415271 -0.7822882
# maskupper -1.199125 -0.5329627

# lower mask vs. upper mask
confint.merMod(glmer_drrul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.530703 0.09241731


####---- plot emotion ratings by masks: IDing correct emotions ----####
## plot all ratings (congruent faces) by mask
dxx_rateEmotion_mask <- ddply(dxx, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dxx_rateEmotion_mask$mask <- factor(dxx_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(9, "Greys")[c(2,8,2)]
ggplot(dxx_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot angry ratings (angry faces) by mask
daa_rateEmotion_mask <- ddply(daa, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
daa_rateEmotion_mask$mask <- factor(daa_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Reds")[c(2,3,2)]
ggplot(daa_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot disgust ratings (disgusted faces) by mask
ddd_rateEmotion_mask <- ddply(ddd, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
ddd_rateEmotion_mask$mask <- factor(ddd_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Oranges")[c(2,3,2)]
ggplot(ddd_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot fearful ratings (fearful faces) by mask
dff_rateEmotion_mask <- ddply(dff, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dff_rateEmotion_mask$mask <- factor(dff_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Blues")[c(2,3,2)]
ggplot(dff_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot happy ratings (happy faces) by mask
dhh_rateEmotion_mask <- ddply(dhh, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dhh_rateEmotion_mask$mask <- factor(dhh_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Greens")[c(2,3,2)]
ggplot(dhh_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot sad ratings (sad faces) by mask
dss_rateEmotion_mask <- ddply(dss, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dss_rateEmotion_mask$mask <- factor(dss_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Greys")[c(2,3,2)]
ggplot(dss_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot surprised ratings (surprised faces) by mask
drr_rateEmotion_mask <- ddply(drr, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
drr_rateEmotion_mask$mask <- factor(drr_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Purples")[c(2,3,2)]
ggplot(drr_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
