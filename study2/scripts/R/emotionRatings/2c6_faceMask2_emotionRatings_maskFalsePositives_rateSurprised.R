# faceMask study 2 emotion ratings analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating surprise
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
dr <- subset(d, emotionRating=='surprised')

# further subset by facial expression
dra <- subset(dr, expression=='angry')
drd <- subset(dr, expression=='disgusted')
drf <- subset(dr, expression=='fearful')
drh <- subset(dr, expression=='happy')
drs <- subset(dr, expression=='sad')

# further subset for lower vs. upper mask contrasts
draul <- subset(dra, dra$mask != 'none')
draul$mask <- factor(draul$mask, levels=c('upper', 'lower'))
drdul <- subset(drd, drd$mask != 'none')
drdul$mask <- factor(drdul$mask, levels=c('upper', 'lower'))
drful <- subset(drf, drf$mask != 'none')
drful$mask <- factor(drful$mask, levels=c('upper', 'lower'))
drhul <- subset(drh, drh$mask != 'none')
drhul$mask <- factor(drhul$mask, levels=c('upper', 'lower'))
drsul <- subset(drs, drs$mask != 'none')
drsul$mask <- factor(drsul$mask, levels=c('upper', 'lower'))


####---- emotion ratings by masks: IDing incorrect emotions ----####
# angry faces
glmer_dra_rateEmotion_mask <- with(dra, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dra_rateEmotion_mask) # less likely to rate angry faces correctly with lower mask *
glmer_draul_rateEmotion_mask <- with(draul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_draul_rateEmotion_mask) # less likely to rate angry faces correctly with lower vs. upper mask ***

# disgusted faces
glmer_drd_rateEmotion_mask <- with(drd, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drd_rateEmotion_mask) # more likely to rate disgusted faces correctly with lower mask **
glmer_drdul_rateEmotion_mask <- with(drdul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drdul_rateEmotion_mask) # more likely to rate disgusted faces correctly with lower vs. upper mask *

# fearful faces
glmer_drf_rateEmotion_mask <- with(drf, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drf_rateEmotion_mask) # more likely to rate fearful faces correctly with upper mask *
glmer_drful_rateEmotion_mask <- with(drful, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drful_rateEmotion_mask) # less likely to rate fearful faces correctly with lower vs. upper mask **

# happy faces
glmer_drh_rateEmotion_mask <- with(drh, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drh_rateEmotion_mask) # ns
glmer_drhul_rateEmotion_mask <- with(drhul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drhul_rateEmotion_mask) # ns

# sad faces
glmer_drs_rateEmotion_mask <- with(drs, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drs_rateEmotion_mask) # more likely to rate sad faces correctly with upper mask *
glmer_drsul_rateEmotion_mask <- with(drsul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drsul_rateEmotion_mask) # less likely to rate sad faces correctly with lower vs. upper mask **


####---- estimate 95% confidence intervals for emotion ratings by masks: IDing incorrect emotions ----####
## angry faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dra_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -2.4227759 -0.1603038
# maskupper -0.0573151  2.3299940

# lower mask vs. upper mask
confint.merMod(glmer_draul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -4.377692 -1.144591


## disgusted faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_drd_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower  0.3770533 2.0025827
# maskupper -0.6509904 0.8859943 

# lower mask vs. upper mask
confint.merMod(glmer_drdul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.1555803 1.845972  


## fearful faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_drf_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.0233161 0.5254758
# maskupper  0.1288983 1.6392762

# lower mask vs. upper mask
confint.merMod(glmer_drful_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.877058 -0.3489936


## happy faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_drh_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.6523924 0.7355428
# maskupper -0.5608046 0.7677655

# lower mask vs. upper mask
confint.merMod(glmer_drhul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.6781904 0.5611523 


## sad faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_drs_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -2.0432472 0.5859273
# maskupper  0.1961318 2.9073099

# lower mask vs. upper mask
confint.merMod(glmer_drsul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -8.293677 -0.8520454 


####---- plot emotion ratings by masks: IDing incorrect emotions ----####
# plot rating surprised (angry faces) by mask
dra_rateEmotion_mask <- ddply(dra, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dra_rateEmotion_mask$mask <- factor(dra_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Reds")[c(2,3,2)]
ggplot(dra_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot rating surprised (disgusted faces) by mask
drd_rateEmotion_mask <- ddply(drd, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
drd_rateEmotion_mask$mask <- factor(drd_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Oranges")[c(2,3,2)]
ggplot(drd_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot rating surprised (fearful faces) by mask
dr_rateEmotion_mask <- ddply(dr, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dr_rateEmotion_mask$mask <- factor(dr_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Blues")[c(2,3,2)]
ggplot(dr_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot rating surprised (happy faces) by mask
drh_rateEmotion_mask <- ddply(drh, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
drh_rateEmotion_mask$mask <- factor(drh_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Greens")[c(2,3,2)]
ggplot(drh_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot rating surprised (sad faces) by mask
drs_rateEmotion_mask <- ddply(drs, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
drs_rateEmotion_mask$mask <- factor(drs_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Greys")[c(2,3,2)]
ggplot(drs_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
