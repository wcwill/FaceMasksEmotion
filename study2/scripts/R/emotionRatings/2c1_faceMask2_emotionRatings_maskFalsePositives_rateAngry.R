# faceMask study 2 emotion ratings analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating anger
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
da <- subset(d, emotionRating=='angry')

# further subset by facial expression
dad <- subset(da, expression=='disgusted')
daf <- subset(da, expression=='fearful')
dah <- subset(da, expression=='happy')
das <- subset(da, expression=='sad')
dar <- subset(da, expression=='surprised')

# further subset for lower vs. upper mask contrasts
dadul <- subset(dad, dad$mask != 'none')
dadul$mask <- factor(dadul$mask, levels=c('upper', 'lower'))
daful <- subset(daf, daf$mask != 'none')
daful$mask <- factor(daful$mask, levels=c('upper', 'lower'))
dahul <- subset(dah, dah$mask != 'none')
dahul$mask <- factor(dahul$mask, levels=c('upper', 'lower'))
dasul <- subset(das, das$mask != 'none')
dasul$mask <- factor(dasul$mask, levels=c('upper', 'lower'))
darul <- subset(dar, dar$mask != 'none')
darul$mask <- factor(darul$mask, levels=c('upper', 'lower'))


####---- emotion ratings by masks: IDing incorrect emotions ----####
# disgust faces
glmer_dad_rateEmotion_mask <- with(dad, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dad_rateEmotion_mask) # less likely to rate disgusted faces correctly with lower mask
glmer_dadul_rateEmotion_mask <- with(dadul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dadul_rateEmotion_mask) # less likely to rate disgusted faces correctly with lower mask vs. upper mask

# fearful faces
glmer_daf_rateEmotion_mask <- with(daf, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_daf_rateEmotion_mask) # less likely to rate fearful faces correctly with lower mask
glmer_daful_rateEmotion_mask <- with(daful, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_daful_rateEmotion_mask) # ns

# happy faces
glmer_dah_rateEmotion_mask <- with(dah, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dah_rateEmotion_mask) # ns
glmer_dahul_rateEmotion_mask <- with(dahul, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_dahul_rateEmotion_mask) # less likely to rate disgusted faces correctly with lower mask vs. upper mask

# sad faces
glmer_das_rateEmotion_mask <- with(das, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_das_rateEmotion_mask) # ns
glmer_dasul_rateEmotion_mask <- with(dasul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dasul_rateEmotion_mask) # ns

# surprised faces
glmer_dar_rateEmotion_mask <- with(dar, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dar_rateEmotion_mask) # less likely to rate surprised faces correctly with lower mask
glmer_darul_rateEmotion_mask <- with(darul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_darul_rateEmotion_mask) # less likely to rate surprised faces correctly with lower mask vs. upper mask


####---- estimate 95% confidence intervals for emotion ratings by masks: IDing incorrect emotions ----####
## disgust faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dad_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -3.2656843 -1.581153
# maskupper -0.5027205 1.224473

# lower mask vs. upper mask
confint.merMod(glmer_dadul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -3.317542 -1.741358 


## fearful faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_daf_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower  -1.441339 -0.1406938 
# maskupper -1.086356 0.2703708  

# lower mask vs. upper mask
confint.merMod(glmer_daful_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.9631503 0.3278955 


## happy faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dah_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower  -2.079658 0.1284587
# maskupper -1.056505 1.1811719

# lower mask vs. upper mask
confint.merMod(glmer_dahul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -2.89122  -0.09514009 


## sad faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_das_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -1.168801 0.4515959 
# maskupper -1.154043 0.5162163  

# lower mask vs. upper mask
confint.merMod(glmer_dasul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.7955561 0.7697545 


## surprised faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dar_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower  -2.0062771 -0.4815119
# maskupper -0.4567522 1.0276621 

# lower mask vs. upper mask
confint.merMod(glmer_darul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -2.437262 -0.8724786


####---- plot emotion ratings by masks: IDing incorrect emotions ----####
# plot angry ratings (disgust faces) by mask
dad_rateEmotion_mask <- ddply(dad, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dad_rateEmotion_mask$mask <- factor(dad_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Oranges")[c(2,3,2)]
ggplot(dad_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot angry ratings (fearful faces) by mask
daf_rateEmotion_mask <- ddply(daf, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
daf_rateEmotion_mask$mask <- factor(daf_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Blues")[c(2,3,2)]
ggplot(daf_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot angry ratings (happy faces) by mask
dah_rateEmotion_mask <- ddply(dah, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dah_rateEmotion_mask$mask <- factor(dah_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Greens")[c(2,3,2)]
ggplot(dah_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot angry ratings (sad faces) by mask
das_rateEmotion_mask <- ddply(das, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
das_rateEmotion_mask$mask <- factor(das_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Greys")[c(2,3,2)]
ggplot(das_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot angry ratings (surprised faces) by mask
dar_rateEmotion_mask <- ddply(dar, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dar_rateEmotion_mask$mask <- factor(dar_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Purples")[c(2,3,2)]
ggplot(dar_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
