# faceMask face ratings analysis script: manipulation checks
# note: need to have run '1_faceMask_proc.R' script first
# 7/31/20
library(lme4)
library(lmerTest)
library(ggplot2)
library(RColorBrewer)

####---- setup ----####
# subset RTs > 100 ms (see Luce, 1986; Whelan, 2008)
d <- subset(d, rt >= 100)

# subset by emotion rating type
da <- subset(d, emotionRating=='angry')
dd <- subset(d, emotionRating=='disgusted')
df <- subset(d, emotionRating=='fearful')
dh <- subset(d, emotionRating=='happy')
ds <- subset(d, emotionRating=='sad')
dr <- subset(d, emotionRating=='surprised')


####---- emotion ratings by face emotion % ----####
## rating anger (angry faces)
glmer_da_rateEmotion_faceAngry <- with(da, glmer(rateEmotion ~ faceAngry + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_da_rateEmotion_faceAngry) # more likely to rate faces 'angry' as face anger % increases

## rating disgust (disgusted faces)
glmer_dd_rateEmotion_faceDisgusted <- with(dd, glmer(rateEmotion ~ faceDisgusted + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dd_rateEmotion_faceDisgusted) # more likely to rate faces 'disgusted' as face disgust % increases

## rating fear (fearful faces)
glmer_df_rateEmotion_faceFearful <- with(df, glmer(rateEmotion ~ faceFearful + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_df_rateEmotion_faceFearful) # more likely to rate faces 'fearful' as face fear % increases

## rating happy (happy faces)
glmer_dh_rateEmotion_faceHappy <- with(dh, glmer(rateEmotion ~ faceHappy + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dh_rateEmotion_faceHappy) # more likely to rate faces 'happy' as face happinness % increases

## rating sadness (sad faces)
glmer_ds_rateEmotion_faceSad <- with(ds, glmer(rateEmotion ~ faceSad + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_ds_rateEmotion_faceSad) # more likely to rate faces 'sad' as face sadness % increases

## rating surprise (surprised faces)
glmer_dr_rateEmotion_faceSurprised <- with(dr, glmer(rateEmotion ~ faceSurprised + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dr_rateEmotion_faceSurprised) # more likely to rate faces 'surprised' as face surprise % increases


####---- plot emotion ratings by face emotion % ----####
# plot angry ratings by face anger %
ggplot(da, aes(x=faceAngry, y=rateEmotion)) + 
  geom_point(alpha=0) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), colour = "#E31A1C", fill = "#FB9A99") +
  xlab("face anger %") +
  ylab("Probability rated angry") +
  geom_vline(xintercept = 50, linetype = "dashed") + 
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(0,100,50)) +
  theme_minimal(base_size=24)

# plot disgust ratings by face disgust %
ggplot(dd, aes(x=faceDisgusted, y=rateEmotion)) + 
  geom_point(alpha=0) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), colour = "#FF7F00", fill = "#FDBF6F") +
  xlab("face disgust %") +
  ylab("Probability rated disgusted") +
  geom_vline(xintercept = 50, linetype = "dashed") + 
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(0,100,50)) +
  theme_minimal(base_size=24)

# plot fearful ratings by face fear %
ggplot(df, aes(x=faceFearful, y=rateEmotion)) + 
  geom_point(alpha=0) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), colour = "#1F78B4", fill = "#A6CEE3") +
  xlab("face fear %") +
  ylab("Probability rated fearful") +
  geom_vline(xintercept = 50, linetype = "dashed") + 
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(0,100,50)) +
  theme_minimal(base_size=24)

# plot happy ratings by face happiness %
ggplot(dh, aes(x=faceHappy, y=rateEmotion)) + 
  geom_point(alpha=0) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), colour = "#33A02C", fill = "#B2DF8A") +
  xlab("face happiness %") +
  ylab("Probability rated happy") +
  geom_vline(xintercept = 50, linetype = "dashed") + 
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(0,100,50)) +
  theme_minimal(base_size=24)

# plot sad ratings by face sadness %
ggplot(ds, aes(x=faceSad, y=rateEmotion)) + 
  geom_point(alpha=0) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), colour = "#636363", fill = "#BDBDBD") +
  xlab("face sadness %") +
  ylab("Probability rated sad") +
  geom_vline(xintercept = 50, linetype = "dashed") + 
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(0,100,50)) +
  theme_minimal(base_size=24)

# plot surprised ratings by face surprise %
ggplot(dr, aes(x=faceSurprised, y=rateEmotion)) + 
  geom_point(alpha=0) + 
  stat_smooth(method="glm", method.args=list(family="binomial"), colour = "#6A3D9A", fill = "#CAB2D6") +
  xlab("face surprise %") +
  ylab("Probability rated surprised") +
  geom_vline(xintercept = 50, linetype = "dashed") + 
  geom_hline(yintercept = .5, linetype = "dashed") +
  scale_x_continuous(breaks=seq(0,100,50)) +
  theme_minimal(base_size=24)
