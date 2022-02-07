# faceMask study 2 emotion ratings analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating happiness
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


####---- emotion ratings by masks: IDing incorrect emotions ----####
# angry faces
glmer_dha_rateEmotion_mask <- with(dha, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dha_rateEmotion_mask) # ns
glmer_dhaul_rateEmotion_mask <- with(dhaul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhaul_rateEmotion_mask) # ns

# disgusted faces
glmer_dhd_rateEmotion_mask <- with(dhd, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhd_rateEmotion_mask) # ns
glmer_dhdul_rateEmotion_mask <- with(dhdul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhdul_rateEmotion_mask) # ns

# fearful faces
glmer_dhf_rateEmotion_mask <- with(dhf, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhf_rateEmotion_mask) # ns
glmer_dhful_rateEmotion_mask <- with(dhful, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhful_rateEmotion_mask) # ns

# sad faces
glmer_dhs_rateEmotion_mask <- with(dhs, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhs_rateEmotion_mask) #  less likely to rate sad faces correctly with lower mask *
glmer_dhsul_rateEmotion_mask <- with(dhsul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhsul_rateEmotion_mask) # less likely to rate sad faces correctly with lower vs. upper mask *

# surprised faces
glmer_dhr_rateEmotion_mask <- with(dhr, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhr_rateEmotion_mask) # ns
glmer_dhrul_rateEmotion_mask <- with(dhrul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhrul_rateEmotion_mask) # ns


####---- estimate 95% confidence intervals for emotion ratings by masks: IDing incorrect emotions ----####
## angry faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dha_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.1670843 0.9682932
# maskupper -0.8499118 1.1974559

# lower mask vs. upper mask
confint.merMod(glmer_dhaul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.963587 0.8350819


## disgusted faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dhd_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.867842 0.5856522
# maskupper -1.952890 0.5866623 

# lower mask vs. upper mask
confint.merMod(glmer_dhdul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.544768 1.571655  


## fearful faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dhf_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.416268 0.2834656
# maskupper -1.569609 0.1698962

# lower mask vs. upper mask
confint.merMod(glmer_dhful_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.6516104 1.055178


## sad faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dhs_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -2.3863563 -0.2695478
# maskupper -0.7173994  1.4175120 

# lower mask vs. upper mask
confint.merMod(glmer_dhsul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -3.416059 -0.5129997 


## surprised faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dhr_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.5784179 1.106841
# maskupper -0.5292197 1.077059

# lower mask vs. upper mask
confint.merMod(glmer_dhrul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.8252913 0.7399608 


####---- plot emotion ratings by masks: IDing incorrect emotions ----####
# plot rating happy (angry faces) by mask
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

# plot rating happy (disgusted faces) by mask
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

# plot rating happy (fearful faces) by mask
dhf_rateEmotion_mask <- ddply(dhf, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dhf_rateEmotion_mask$mask <- factor(dhf_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Blues")[c(2,3,2)]
ggplot(dhf_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot rating happy (sad faces) by mask
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

# plot rating happy (surprised faces) by mask
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
