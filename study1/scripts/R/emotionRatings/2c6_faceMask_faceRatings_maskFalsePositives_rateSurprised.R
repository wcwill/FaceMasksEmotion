# faceMask face ratings analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating surprise
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


####---- emotion ratings by masks: IDing incorrect emotions ----###
# angry faces
glmer_dra_rateEmotion_mask <- with(dra, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_dra_rateEmotion_mask) # more likely to rate angry faces correctly with upper mask
glmer_draul_rateEmotion_mask <- with(draul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_draul_rateEmotion_mask) # more likely to rate angry faces correctly with upper mask vs. lower mask

# disgusted faces
glmer_drd_rateEmotion_mask <- with(drd, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drd_rateEmotion_mask) # ns
glmer_drdul_rateEmotion_mask <- with(drdul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drdul_rateEmotion_mask) # more likely to rate disgust faces correctly with upper mask vs. lower mask

# fearful faces
glmer_drf_rateEmotion_mask <- with(drf, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drf_rateEmotion_mask) # more likely to rate fearful faces correctly with upper mask, less likely to rate fearful faces correctly with lower mask
glmer_drful_rateEmotion_mask <- with(drful, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drful_rateEmotion_mask) # more likely to rate fearful faces correctly with upper mask vs. lower mask

# happy faces
glmer_drh_rateEmotion_mask <- with(drh, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_drh_rateEmotion_mask) # ns
glmer_drhul_rateEmotion_mask <- with(drhul, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_drhul_rateEmotion_mask) # more likely to rate happy faces correctly with upper mask vs. lower mask

# sad faces
glmer_drs_rateEmotion_mask <- with(drs, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drs_rateEmotion_mask) # less likely to rate sad faces correctly with lower mask
glmer_drsul_rateEmotion_mask <- with(drsul, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_drsul_rateEmotion_mask) # more likely to rate sad faces correctly with upper mask vs. lower mask


####---- estimate 95% confidence intervals for emotion ratings by masks: IDing incorrect emotions ----####
## angry faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dra_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.59883223 0.2964217
# maskupper -0.02625316 0.8827758

# lower mask vs. upper mask
confint.merMod(glmer_draul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.066907 -0.140049


## disgust faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_drd_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.4817270 0.1914626
# maskupper -0.1428477 0.5648597

# lower mask vs. upper mask
confint.merMod(glmer_drdul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.7253143 0.02104836


## fearful faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_drf_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.737868 -1.015920
# maskupper 1.423396  2.126215

# lower mask vs. upper mask
confint.merMod(glmer_drful_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -3.474676 -2.67065


## happy faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_drh_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.57662381 0.0121493
# maskupper -0.07848288 0.5177226

# lower mask vs. upper mask
confint.merMod(glmer_drhul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.7670584 -0.2060242


## sad faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_drs_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.9961286 -0.2733884
# maskupper -0.3357253  0.4421852

# lower mask vs. upper mask
confint.merMod(glmer_drsul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.9480554 -0.3037708


####---- plot emotion ratings by masks: IDing incorrect emotions ----####
# plot surprised ratings (angry faces) by mask
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

# plot surprised ratings (disgusted faces) by mask
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

# plot surprised ratings (fearful faces) by mask
drf_rateEmotion_mask <- ddply(drf, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
drf_rateEmotion_mask$mask <- factor(drf_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Blues")[c(2,3,2)]
ggplot(drf_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot surprised ratings (happy faces) by mask
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

# plot surprised ratings (sad faces) by mask
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
