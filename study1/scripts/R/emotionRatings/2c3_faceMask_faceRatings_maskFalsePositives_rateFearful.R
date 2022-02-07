# faceMask face ratings analysis script: mask analyses of false positives (IDing incorrect emotion) -- rating fear
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
df <- subset(d, emotionRating=='fearful')

# further subset by facial expression
dfa <- subset(df, expression=='angry')
dfd <- subset(df, expression=='disgusted')
dfh <- subset(df, expression=='happy')
dfs <- subset(df, expression=='sad')
dfr <- subset(df, expression=='surprised')

# further subset for lower vs. upper mask contrasts
dfaul <- subset(dfa, dfa$mask != 'none')
dfaul$mask <- factor(dfaul$mask, levels=c('upper', 'lower'))
dfdul <- subset(dfd, dfd$mask != 'none')
dfdul$mask <- factor(dfdul$mask, levels=c('upper', 'lower'))
dfhul <- subset(dfh, dfh$mask != 'none')
dfhul$mask <- factor(dfhul$mask, levels=c('upper', 'lower'))
dfsul <- subset(dfs, dfs$mask != 'none')
dfsul$mask <- factor(dfsul$mask, levels=c('upper', 'lower'))
dfrul <- subset(dfr, dfr$mask != 'none')
dfrul$mask <- factor(dfrul$mask, levels=c('upper', 'lower'))


####---- emotion ratings by masks: IDing incorrect emotions ----####
# angry faces
glmer_dfa_rateEmotion_mask <- with(dfa, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dfa_rateEmotion_mask) # more likely to rate angry faces correctly with upper mask, less likely to rate angry faces correctly with lower mask
glmer_dfaul_rateEmotion_mask <- with(dfaul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dfaul_rateEmotion_mask) # more likely to rate angry faces correctly with upper mask vs lower mask

# disgust faces
glmer_dfd_rateEmotion_mask <- with(dfd, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dfd_rateEmotion_mask) # ns
glmer_dfdul_rateEmotion_mask <- with(dfdul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dfdul_rateEmotion_mask) # ns

# happy faces
glmer_dfh_rateEmotion_mask <- with(dfh, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_dfh_rateEmotion_mask) # ns
glmer_dfhul_rateEmotion_mask <- with(dfhul, glmer(rateEmotion ~ mask + (1 | subject), family='binomial'))
summary(glmer_dfhul_rateEmotion_mask) # more likely to rate happy faces correctly with upper mask vs. lower mask

# sad faces
glmer_dfs_rateEmotion_mask <- with(dfs, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dfs_rateEmotion_mask) # more likely to rate sad faces correctly with upper mask, less likely to rate sad faces correctly with lower mask
glmer_dfsul_rateEmotion_mask <- with(dfsul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dfsul_rateEmotion_mask) # more likely to rate sad faces correctly with upper mask vs. lower mask

# surprised faces
glmer_dfr_rateEmotion_mask <- with(dfr, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dfr_rateEmotion_mask) # more likely to rate surprised faces correctly with upper mask
glmer_dfrul_rateEmotion_mask <- with(dfrul, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dfrul_rateEmotion_mask) # more likely to rate surprised faces correctly with upper mask vs. lower mask


####---- estimate 95% confidence intervals for emotion ratings by masks: IDing incorrect emotions ----####
## angry faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dfa_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.64665267 -0.08297024
# maskupper 0.02557526  0.62742847

# lower mask vs. upper mask
confint.merMod(glmer_dfaul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.9076411 -0.3154886


## disgust faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dfd_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.0867374 0.4896733
# maskupper -0.1868094 0.3944542

# lower mask vs. upper mask
confint.merMod(glmer_dfdul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.1856662 0.367077


## happy faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dfh_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.7525761 0.170658
# maskupper -0.2979769 0.620729

# lower mask vs. upper mask
confint.merMod(glmer_dfhul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.9893848 -0.006193162


## sad faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dfs_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.79281302 -0.2657062
# maskupper 0.09769923  0.6366980

# lower mask vs. upper mask
confint.merMod(glmer_dfsul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -1.117772 -0.5865762


## surprised faces
# lower mask vs. no mask & upper mask vs. no mask
confint.merMod(glmer_dfr_rateEmotion_mask, parm=c('masklower', 'maskupper'), method='boot', nsim=1000)
# masklower -0.3126590 0.2527924
# maskupper 0.3768691 0.9747463

# lower mask vs. upper mask
confint.merMod(glmer_dfrul_rateEmotion_mask, parm=c('masklower'), method='boot', nsim=1000)
# masklower -0.9802144 -0.3849245


####---- plot emotion ratings by masks: IDing incorrect emotions ----####
# plot fearful ratings (angry faces) by mask
dfa_rateEmotion_mask <- ddply(dfa, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dfa_rateEmotion_mask$mask <- factor(dfa_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Reds")[c(2,3,2)]
ggplot(dfa_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot fearful ratings (happy faces) by mask
dfh_rateEmotion_mask <- ddply(dfh, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dfh_rateEmotion_mask$mask <- factor(dfh_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Greens")[c(2,3,2)]
ggplot(dfh_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot fearful ratings (sad faces) by mask
dfs_rateEmotion_mask <- ddply(dfs, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dfs_rateEmotion_mask$mask <- factor(dfs_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Greys")[c(2,3,2)]
ggplot(dfs_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

# plot fearful ratings (surprised faces) by mask
dfr_rateEmotion_mask <- ddply(dfr, "mask", summarise, m_rateEmotion = mean(rateEmotion), ci = 1.96*(sd(rateEmotion) / sqrt(length(rateEmotion))))
dfr_rateEmotion_mask$mask <- factor(dfr_rateEmotion_mask$mask, levels=c('lower', 'none', 'upper'))

my_palette <- brewer.pal(3, "Purples")[c(2,3,2)]
ggplot(dfr_rateEmotion_mask, aes(x=mask, y=m_rateEmotion, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=m_rateEmotion-ci, ymax=m_rateEmotion+ci), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("Mean % rated correctly") +
  geom_hline(yintercept = .5, linetype = "dashed") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")

