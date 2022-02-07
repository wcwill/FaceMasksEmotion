# faceMask face ratings analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating happiness
# note: need to have run '1_faceMask_proc.R' script first
# 1/23/21
library(lme4)
library(lmerTest)
library(ggplot2)
library(RColorBrewer)

####---- setup ----####
# subset RTs > 100 ms (see Luce, 1986; Whelan, 2008)
d <- subset(d, rt >= 100)

# flip ratings
d$rateEmotion <- 1 - d$rateEmotion

# subset by emotion rating type
dh <- subset(d, emotionRating=='happy')

# further subset by facial expression
dha <- subset(dh, expression=='angry')
dhd <- subset(dh, expression=='disgusted')
dhf <- subset(dh, expression=='fearful')
dhs <- subset(dh, expression=='sad')
dhr <- subset(dh, expression=='surprised')

# further subset for lower vs. upper mask contrasts
dhaul <- subset(dha, dha$mask != 'none')
dhaul$mask <- factor(dhaul$mask, levels=c('upper', 'lower'))
dhdul <- subset(dhd, dhd$mask != 'none')
dhdul$mask <- factor(dhdul$mask, levels=c('upper', 'lower'))
dhful <- subset(dhf, dhf$mask != 'none')
dhful$mask <- factor(dhful$mask, levels=c('upper', 'lower'))
dhsul <- subset(dhs, dhs$mask != 'none')
dhsul$mask <- factor(dhsul$mask, levels=c('upper', 'lower'))
dhrul <- subset(dhr, dhr$mask != 'none')
dhrul$mask <- factor(dhrul$mask, levels=c('upper', 'lower'))


####---- emotion ratings by masks: IDing incorrect emotions ----###
# angry faces
glmer_dha_rateEmotion_mask <- with(dha, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dha_rateEmotion_mask) # less likely to rate angry faces correctly with either mask
glmer_dhaul_rateEmotion_mask <- with(dhaul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhaul_rateEmotion_mask) # ns

# disgusted faces
glmer_dhd_rateEmotion_mask <- with(dhd, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhd_rateEmotion_mask) # less likely to rate disgusted faces correctly with lower mask
glmer_dhdul_rateEmotion_mask <- with(dhdul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhdul_rateEmotion_mask) # more likely to rate disgusted faces correctly with upper mask vs. lower mask

# fearful faces
glmer_dhf_rateEmotion_mask <- with(dhf, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhf_rateEmotion_mask) # ns
glmer_dhful_rateEmotion_mask <- with(dhful, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhful_rateEmotion_mask) # ns

# sad faces
glmer_dhs_rateEmotion_mask <- with(dhs, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_dhs_rateEmotion_mask) # less likely to rate sad faces correctly with lower mask
glmer_dhsul_rateEmotion_mask <- with(dhsul, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_dhsul_rateEmotion_mask) # ns

# surprised faces
glmer_dhr_rateEmotion_mask <- with(dhr, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhr_rateEmotion_mask) # less likely to rate surprised faces correctly with lower mask
glmer_dhrul_rateEmotion_mask <- with(dhrul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhrul_rateEmotion_mask) # more likely to rate happy faces correctly with upper mask vs. lower mask


####---- estimate 95% confidence intervals for emotion ratings by masks: IDing incorrect emotions ----####
## angry faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dha_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.349749 -0.2314221
# maskupper -1.513714 -0.3151559

# lower mask vs. upper mask
confint.merMod(glmer_dhaul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.4822175 0.7463944


## disgust faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dhd_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.6078107 -0.3979831
# maskupper -0.7513993  0.4151836

# lower mask vs. upper mask
confint.merMod(glmer_dhdul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.603375 -0.1893045


## fearful faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dhf_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.9844332 0.2502912
# maskupper -0.5560956 0.6919541

# lower mask vs. upper mask
confint.merMod(glmer_dhful_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.238858 0.1676959


## sad faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dhs_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.484532 -0.445851787
# maskupper -1.085311  0.001582976

# lower mask vs. upper mask
confint.merMod(glmer_dhsul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.9932912 0.02703807


## surprised faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dhr_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.0342732 -0.0775699
# maskupper -0.5356733  0.4771546

# lower mask vs. upper mask
confint.merMod(glmer_dhrul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.072048 -0.06358947


####---- plot emotion ratings by masks: IDing incorrect emotions ----####
# plot happy ratings (angry faces) by mask
dha_rateEmotion_mask <- ddply(dha, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dha_rateEmotion_mask$mask <- factor(dha_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Reds")[c(2,3,2)]
ggplot(dha_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot happy ratings (disgusted faces) by mask
dhd_rateEmotion_mask <- ddply(dhd, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dhd_rateEmotion_mask$mask <- factor(dhd_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Oranges")[c(2,3,2)]
ggplot(dhd_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot happy ratings (sad faces) by mask
dhs_rateEmotion_mask <- ddply(dhs, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dhs_rateEmotion_mask$mask <- factor(dhs_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Greys")[c(2,3,2)]
ggplot(dhs_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot happy ratings (surprised faces) by mask
dhr_rateEmotion_mask <- ddply(dhr, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dhr_rateEmotion_mask$mask <- factor(dhr_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Purples")[c(2,3,2)]
ggplot(dhr_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
