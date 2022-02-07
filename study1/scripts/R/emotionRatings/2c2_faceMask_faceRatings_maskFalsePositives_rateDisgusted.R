# faceMask face ratings analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating disgust
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
dd <- subset(d, emotionRating=='disgusted')

# further subset by facial expression
dda <- subset(dd, expression=='angry')
ddf <- subset(dd, expression=='fearful')
ddh <- subset(dd, expression=='happy')
dds <- subset(dd, expression=='sad')
ddr <- subset(dd, expression=='surprised')

# further subset for lower vs. upper mask contrasts
ddaul <- subset(dda, dda$mask != 'none')
ddaul$mask <- factor(ddaul$mask, levels=c('upper', 'lower'))
ddful <- subset(ddf, ddf$mask != 'none')
ddful$mask <- factor(ddful$mask, levels=c('upper', 'lower'))
ddhul <- subset(ddh, ddh$mask != 'none')
ddhul$mask <- factor(ddhul$mask, levels=c('upper', 'lower'))
ddsul <- subset(dds, dds$mask != 'none')
ddsul$mask <- factor(ddsul$mask, levels=c('upper', 'lower'))
ddrul <- subset(ddr, ddr$mask != 'none')
ddrul$mask <- factor(ddrul$mask, levels=c('upper', 'lower'))


####---- emotion ratings by masks: IDing incorrect emotions ----####
# angry faces
glmer_dda_rateEmotion_mask <- with(dda, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dda_rateEmotion_mask) # less likely to rate angry faces correctly with lower mask; more likely to rate angry faces correctly with upper mask
glmer_ddaul_rateEmotion_mask <- with(ddaul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_ddaul_rateEmotion_mask) # more likely to rate angry faces correctly with upper mask vs. lower mask

# fearful faces
glmer_ddf_rateEmotion_mask <- with(ddf, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_ddf_rateEmotion_mask) # more likely to rate fearful faces correctly with lower mask; less likely to rate fearful faces correctly with upper mask
glmer_ddful_rateEmotion_mask <- with(ddful, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_ddful_rateEmotion_mask) # more likely to rate fearful faces correctly with lower mask vs. upper mask

# happy faces
glmer_ddh_rateEmotion_mask <- with(ddh, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_ddh_rateEmotion_mask) # ns
glmer_ddhul_rateEmotion_mask <- with(ddhul, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_ddhul_rateEmotion_mask) # ns

# sad faces
glmer_dds_rateEmotion_mask <- with(dds, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dds_rateEmotion_mask) # ns
glmer_ddsul_rateEmotion_mask <- with(ddsul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_ddsul_rateEmotion_mask) # ns

# surprised faces
glmer_ddr_rateEmotion_mask <- with(ddr, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_ddr_rateEmotion_mask) # ns
glmer_ddrul_rateEmotion_mask <- with(ddrul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_ddrul_rateEmotion_mask) # more likely to rate surprised faces correctly with lower mask vs. upper mask


####---- estimate 95% confidence intervals for emotion ratings by masks: IDing incorrect emotions ----####
## angry faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dda_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.5122450 -0.0222790
# maskupper 0.4277822  0.9357835

# lower mask vs. upper mask
confint.merMod(glmer_ddaul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.165192 -0.6294565


## fearful faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_ddf_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower 1.0006556  1.6011857
# maskupper -0.8335044 -0.2678261

# lower mask vs. upper mask
confint.merMod(glmer_ddful_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 1.477313 2.09083


## happy faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_ddh_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.6863962 0.2042772
# maskupper -0.7092632 0.1773940

# lower mask vs. upper mask
confint.merMod(glmer_ddhul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.4697783 0.5103141


## sad faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dds_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.2484279 0.2396636
# maskupper -0.1342583 0.3836622

# lower mask vs. upper mask
confint.merMod(glmer_ddsul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.3835938 0.1291839


## surprised faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_ddr_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.02951276 0.5446838
# maskupper -0.38312042 0.1865985

# lower mask vs. upper mask
confint.merMod(glmer_ddrul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower 0.08380522 0.6568797


####---- plot emotion ratings by masks: IDing incorrect emotions ----####
# plot disgust ratings (angry faces) by mask
dda_rateEmotion_mask <- ddply(dda, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dda_rateEmotion_mask$mask <- factor(dda_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Reds")[c(2,3,2)]
ggplot(dda_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot disgust ratings (fearful faces) by mask
ddf_rateEmotion_mask <- ddply(ddf, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
ddf_rateEmotion_mask$mask <- factor(ddf_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Blues")[c(2,3,2)]
ggplot(ddf_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot disgust ratings (happy faces) by mask
ddh_rateEmotion_mask <- ddply(ddh, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
ddh_rateEmotion_mask$mask <- factor(ddh_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Greens")[c(2,3,2)]
ggplot(ddh_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot disgust ratings (sad faces) by mask
dds_rateEmotion_mask <- ddply(dds, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dds_rateEmotion_mask$mask <- factor(dds_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Greys")[c(2,3,2)]
ggplot(dds_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot disgust ratings (surprised faces) by mask
ddr_rateEmotion_mask <- ddply(ddr, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
ddr_rateEmotion_mask$mask <- factor(ddr_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Purples")[c(2,3,2)]
ggplot(ddr_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
