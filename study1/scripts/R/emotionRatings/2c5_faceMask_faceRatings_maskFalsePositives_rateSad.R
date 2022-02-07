# faceMask face ratings analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating sadness
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
ds <- subset(d, emotionRating=='sad')

# further subset by facial expression
dsa <- subset(ds, expression=='angry')
dsd <- subset(ds, expression=='disgusted')
dsf <- subset(ds, expression=='fearful')
dsh <- subset(ds, expression=='happy')
dsr <- subset(ds, expression=='surprised')

# further subset for lower vs. upper mask contrasts
dsaul <- subset(dsa, dsa$mask != 'none')
dsaul$mask <- factor(dsaul$mask, levels=c('upper', 'lower'))
dsdul <- subset(dsd, dsd$mask != 'none')
dsdul$mask <- factor(dsdul$mask, levels=c('upper', 'lower'))
dsful <- subset(dsf, dsf$mask != 'none')
dsful$mask <- factor(dsful$mask, levels=c('upper', 'lower'))
dshul <- subset(dsh, dsh$mask != 'none')
dshul$mask <- factor(dshul$mask, levels=c('upper', 'lower'))
dsrul <- subset(dsr, dsr$mask != 'none')
dsrul$mask <- factor(dsrul$mask, levels=c('upper', 'lower'))


####---- emotion ratings by masks: IDing incorrect emotions ----###
# angry faces
glmer_dsa_rateEmotion_mask <- with(dsa, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsa_rateEmotion_mask) # more likely to rate angry faces correctly with lower mask, less likely to rate angry faces correctly with upper mask
glmer_dsaul_rateEmotion_mask <- with(dsaul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsaul_rateEmotion_mask) # more likely to rate angry faces correctly with lower mask vs. upper mask

# disgusted faces
glmer_dsd_rateEmotion_mask <- with(dsd, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsd_rateEmotion_mask) # ns
glmer_dsdul_rateEmotion_mask <- with(dsdul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsdul_rateEmotion_mask) # ns

# fearful faces
glmer_dsf_rateEmotion_mask <- with(dsf, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsf_rateEmotion_mask) # more likely to rate fearful faces correctly with lower mask, less likely to rate fearful faces correctly with upper mask
glmer_dsful_rateEmotion_mask <- with(dsful, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsful_rateEmotion_mask) # more likely to rate fearful faces correctly with lower mask vs. upper mask

# happy faces
glmer_dsh_rateEmotion_mask <- with(dsh, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_dsh_rateEmotion_mask) # less likely to rate happy faces correctly with lower mask
glmer_dshul_rateEmotion_mask <- with(dshul, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_dshul_rateEmotion_mask) # more likely to rate happy faces correctly with upper mask vs. lower mask

# surprised faces
glmer_dsr_rateEmotion_mask <- with(dsr, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsr_rateEmotion_mask) # ns
glmer_dsrul_rateEmotion_mask <- with(dsrul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsrul_rateEmotion_mask) # ns


####---- estimate 95% confidence intervals for emotion ratings by masks: IDing incorrect emotions ----####
## angry faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dsa_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower 0.2427287  1.1020448
# maskupper -1.0340895 -0.246334

# lower mask vs. upper mask
confint.merMod(glmer_dsaul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.8998083 1.659113


## disgust faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dsd_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.1323688 0.5319743
# maskupper -0.3944342 0.2669797

# lower mask vs. upper mask
confint.merMod(glmer_dsdul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.05075126 0.6450699


## fearful faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dsf_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower 0.2736221  1.1128785
# maskupper -0.9918161 -0.1700501

# lower mask vs. upper mask
confint.merMod(glmer_dsful_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.817966 1.5598


## happy faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dsh_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.0915206 -0.1448073
# maskupper -0.4891718  0.4232489

# lower mask vs. upper mask
confint.merMod(glmer_dshul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower  -1.109674 -0.09233839


## surprised faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dsr_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.4780256 0.2001132
# maskupper -0.3616097 0.3584040

# lower mask vs. upper mask
confint.merMod(glmer_dsrul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.4325883 0.1719011


####---- plot emotion ratings by masks: IDing incorrect emotions ----####
# plot sad ratings (angry faces) by mask
dsa_rateEmotion_mask <- ddply(dsa, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dsa_rateEmotion_mask$mask <- factor(dsa_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Reds")[c(2,3,2)]
ggplot(dsa_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot sad ratings (fearful faces) by mask
dsf_rateEmotion_mask <- ddply(dsf, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dsf_rateEmotion_mask$mask <- factor(dsf_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Blues")[c(2,3,2)]
ggplot(dsf_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot sad ratings (happy faces) by mask
dsh_rateEmotion_mask <- ddply(dsh, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dsh_rateEmotion_mask$mask <- factor(dsh_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Greens")[c(2,3,2)]
ggplot(dsh_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
