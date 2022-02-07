# faceMask study 2 emotion ratings analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating sadness
# note: need to have run '1_faceMask2_proc.R' script first
# 10/28/20
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


####---- emotion ratings by masks: IDing incorrect emotions ----####
# angry faces
glmer_dsa_rateEmotion_mask <- with(dsa, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsa_rateEmotion_mask) # less likely to rate angry faces correctly with upper mask * & more likely with lower mask
glmer_dsaul_rateEmotion_mask <- with(dsaul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsaul_rateEmotion_mask) # more likely to rate angry faces correctly with lower vs. upper mask ***

# disgusted faces
glmer_dsd_rateEmotion_mask <- with(dsd, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsd_rateEmotion_mask) # less likely to rate disgusted faces correctly with lower mask *
glmer_dsdul_rateEmotion_mask <- with(dsdul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsdul_rateEmotion_mask) # less likely to rate disgusted faces correctly with lower vs. upper mask **

# fearful faces
glmer_dsf_rateEmotion_mask <- with(dsf, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsf_rateEmotion_mask) # ns
glmer_dsful_rateEmotion_mask <- with(dsful, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsful_rateEmotion_mask) # ns

# happy faces
glmer_dsh_rateEmotion_mask <- with(dsh, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsh_rateEmotion_mask) # less likely to rate happy faces correctly with lower mask **
glmer_dshul_rateEmotion_mask <- with(dshul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dshul_rateEmotion_mask) # less likely to rate happy faces correctly with lower vs. upper mask ***

# surprised faces
glmer_dsr_rateEmotion_mask <- with(dsr, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dsr_rateEmotion_mask) # less likely to rate surprised faces correctly with lower mask *
glmer_dsrul_rateEmotion_mask <- with(dsrul, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_dsrul_rateEmotion_mask) # less likely to rate surprised faces correctly with lower vs. upper mask ***


####---- estimate 95% confidence intervals for emotion ratings by masks: IDing incorrect emotions ----####
## angry faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dsa_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower  0.09721299  1.2972329
# maskupper -1.29939273 -0.1270628

# lower mask vs. upper mask
confint.merMod(glmer_dsaul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.7482217 2.012525


## disgusted faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dsd_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.5787246 -0.08429733
# maskupper -0.5014213 0.92481509

# lower mask vs. upper mask
confint.merMod(glmer_dsdul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.687316 -0.3250032 


## fearful faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dsf_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower  -0.9921582  0.46438979
# maskupper  -1.4191919 -0.01789969

# lower mask vs. upper mask
confint.merMod(glmer_dsful_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower  -0.3385405 1.148965


## happy faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dsh_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower  -2.2126106 -0.6111182
# maskupper  -0.4883512  1.0039839 

# lower mask vs. upper mask
confint.merMod(glmer_dshul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -3.02933 -1.040368


## surprised faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dsr_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower  -1.5290498 -0.1306305
# maskupper  -0.8024857  0.6426189

# lower mask vs. upper mask
confint.merMod(glmer_dsrul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.222323 -0.4129531


####---- plot emotion ratings by masks: IDing incorrect emotions ----####
# plot rating sad (angry faces) by mask
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

# plot rating sad (disgusted faces) by mask
dsd_rateEmotion_mask <- ddply(dsd, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dsd_rateEmotion_mask$mask <- factor(dsd_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Oranges")[c(2,3,2)]
ggplot(dsd_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot rating sad (fearful faces) by mask
ds_rateEmotion_mask <- ddply(ds, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
ds_rateEmotion_mask$mask <- factor(ds_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Blues")[c(2,3,2)]
ggplot(ds_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot rating sad (happy faces) by mask
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

# plot rating sad (surprised faces) by mask
dsr_rateEmotion_mask <- ddply(dsr, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dsr_rateEmotion_mask$mask <- factor(dsr_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Purples")[c(2,3,2)]
ggplot(dsr_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
