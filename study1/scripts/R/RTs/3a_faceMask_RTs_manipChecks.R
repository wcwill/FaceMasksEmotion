# faceMask RT analysis script: manipulation checks
# note: need to have run '1_faceMask_behav_proc.R' script first
# 7/31/20
library(lme4)
library(lmerTest)
library(ggplot2)
library(RColorBrewer)
library(moments)
library(interactions)

####---- setup ----####
# subset RTs > 100 ms (see Luce, 1986; Whelan, 2008)
d <- subset(d, rt >= 100)

# check RTs for skewness and log transform
hist(d$rt)
skewness(d$rt)
d$logrt <- log10(d$rt)

# subset by emotion rating type
da <- subset(d, emotionRating=='angry')
dd <- subset(d, emotionRating=='disgusted')
df <- subset(d, emotionRating=='fearful')
dh <- subset(d, emotionRating=='happy')
ds <- subset(d, emotionRating=='sad')
dr <- subset(d, emotionRating=='surprised')

# recode face ratings
da$rateEmotion[da$rateEmotion == 1] <- 'angry'
da$rateEmotion[da$rateEmotion == 0] <- 'not angry'
da$rateEmotion <- as.factor(da$rateEmotion)
da$rateEmotion <- relevel(da$rateEmotion, 'not angry')
dd$rateEmotion[dd$rateEmotion == 1] <- 'disgusted'
dd$rateEmotion[dd$rateEmotion == 0] <- 'not disgusted'
dd$rateEmotion <- as.factor(dd$rateEmotion)
dd$rateEmotion <- relevel(dd$rateEmotion, 'not disgusted')
df$rateEmotion[df$rateEmotion == 1] <- 'fearful'
df$rateEmotion[df$rateEmotion == 0] <- 'not fearful'
df$rateEmotion <- as.factor(df$rateEmotion)
df$rateEmotion <- relevel(df$rateEmotion, 'not fearful')
dh$rateEmotion[dh$rateEmotion == 1] <- 'happy'
dh$rateEmotion[dh$rateEmotion == 0] <- 'not happy'
dh$rateEmotion <- as.factor(dh$rateEmotion)
dh$rateEmotion <- relevel(dh$rateEmotion, 'not happy')
ds$rateEmotion[ds$rateEmotion == 1] <- 'sad'
ds$rateEmotion[ds$rateEmotion == 0] <- 'not sad'
ds$rateEmotion <- as.factor(ds$rateEmotion)
ds$rateEmotion <- relevel(ds$rateEmotion, 'not sad')
dr$rateEmotion[dr$rateEmotion == 1] <- 'surprised'
dr$rateEmotion[dr$rateEmotion == 0] <- 'not surprised'
dr$rateEmotion <- as.factor(dr$rateEmotion)
dr$rateEmotion <- relevel(dr$rateEmotion, 'not surprised')


####---- RTs by emotion ratings * facial emotion % ----####
## rating anger (angry faces)
lmer_da_logrt_faceAngry_rateAngry <- with(da, lmer(logrt ~ faceAngry * rateEmotion + (1 | subject) + (1 | face)))
summary(lmer_da_logrt_faceAngry_rateAngry)
# rated angry
da1 <- subset(da, rateEmotion == 'angry')
lmer_da1_logrt_faceAngry_rate1 <- with(da1, lmer(logrt ~ faceAngry + (1 | subject) + (1 | face)))
summary(lmer_da1_logrt_faceAngry_rate1)

## rating disgust (disgusted faces)
lmer_dd_logrt_faceDisgusted_rateDisgust <- with(dd, lmer(logrt ~ faceDisgusted * rateEmotion + (1 | subject) + (1 | face)))
summary(lmer_dd_logrt_faceDisgusted_rateDisgust)
# rated disgusted
dd1 <- subset(dd, rateEmotion == 'disgusted')
lmer_dd1_logrt_faceDisgusted_rate1 <- with(dd1, lmer(logrt ~ faceDisgusted + (1 | subject) + (1 | face)))
summary(lmer_dd1_logrt_faceDisgusted_rate1)

## rating fear (fearful faces)
lmer_df_logrt_faceFearful_rateFearful <- with(df, lmer(logrt ~ faceFearful * rateEmotion + (1 | subject) + (1 | face)))
summary(lmer_df_logrt_faceFearful_rateFearful)
# rated fearful
df1 <- subset(df, rateEmotion == 'fearful')
lmer_df1_logrt_faceFearful_rate1 <- with(df1, lmer(logrt ~ faceFearful + (1 | subject) + (1 | face)))
summary(lmer_df1_logrt_faceFearful_rate1)

## rating happy (happy faces)
lmer_dh_logrt_faceHappy_rateHappy <- with(dh, lmer(logrt ~ faceHappy * rateEmotion + (1 | subject) + (1 | face)))
summary(lmer_dh_logrt_faceHappy_rateHappy)
# rated happy
dh1 <- subset(dh, rateEmotion == 'happy')
lmer_dh1_logrt_faceHappy_rate1 <- with(dh1, lmer(logrt ~ faceHappy + (1 | subject) + (1 | face)))
summary(lmer_dh1_logrt_faceHappy_rate1)

## rating sadness (sad faces)
lmer_ds_logrt_faceSad_rateSad <- with(ds, lmer(logrt ~ faceSad * rateEmotion + (1 | subject) + (1 | face)))
summary(lmer_ds_logrt_faceSad_rateSad)
# rated sad
ds1 <- subset(ds, rateEmotion == 'sad')
lmer_ds1_logrt_faceSad_rate1 <- with(ds1, lmer(logrt ~ faceSad + (1 | subject) + (1 | face)))
summary(lmer_ds1_logrt_faceSad_rate1)

## rating surprise (surprised faces)
lmer_dr_logrt_faceSurprised_rateSurprise <- with(dr, lmer(logrt ~ faceSurprised * rateEmotion + (1 | subject) + (1 | face)))
summary(lmer_dr_logrt_faceSurprised_rateSurprise)
# rated surprised
dr1 <- subset(dr, rateEmotion == 'surprised')
lmer_dr1_logrt_faceSurprised_rate1 <- with(dr1, lmer(logrt ~ faceSurprised + (1 | subject) + (1 | face)))
summary(lmer_dr1_logrt_faceSurprised_rate1)


####---- plot RTs by emotion ratings and facial emotion % ----####
# plot angry rating RTs by face anger % and angry ratings
interact_plot(lmer_da_logrt_faceAngry_rateAngry, pred = faceAngry, modx = rateEmotion, interval = TRUE, int.width = .95, 
              dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "face anger %", y.label = "mean log(RT)", 
              legend.main = "rating", color.class = "Reds") + 
  scale_x_continuous(breaks=seq(0,100,50)) +
  theme_minimal(base_size=24)

# plot disgust rating RTs by face disgust % and disgust ratings
interact_plot(lmer_dd_logrt_faceDisgusted_rateDisgust, pred = faceDisgusted, modx = rateEmotion, interval = TRUE, int.width = .95, 
              dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "face disgust %", y.label = "mean log(RT)", 
              legend.main = "rating", color.class = "Oranges") + 
  scale_x_continuous(breaks=seq(0,100,50)) +
  theme_minimal(base_size=24)

# plot fearful rating RTs by face fear % and fearful ratings
interact_plot(lmer_df_logrt_faceFearful_rateFearful, pred = faceFearful, modx = rateEmotion, interval = TRUE, int.width = .95, 
              dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "face fear %", y.label = "mean log(RT)", 
              legend.main = "rating", color.class = "Blues") + 
  scale_x_continuous(breaks=seq(0,100,50)) +
  theme_minimal(base_size=24)

# plot happy rating RTs by face happiness % and happy ratings
interact_plot(lmer_dh_logrt_faceHappy_rateHappy, pred = faceHappy, modx = rateEmotion, interval = TRUE, int.width = .95, 
              dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "face happiness %", y.label = "mean log(RT)", 
              legend.main = "rating", color.class = "Greens") + 
  scale_x_continuous(breaks=seq(0,100,50)) +
  theme_minimal(base_size=24)

# plot sad rating RTs by face sadness % and sad ratings
interact_plot(lmer_ds_logrt_faceSad_rateSad, pred = faceSad, modx = rateEmotion, interval = TRUE, int.width = .95, 
              dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "face sadness %", y.label = "mean log(RT)", 
              legend.main = "rating", color.class = "Greys") + 
  scale_x_continuous(breaks=seq(0,100,50)) +
  theme_minimal(base_size=24)

# plot surprised rating RTs by face surprise % and surprised ratings
interact_plot(lmer_dr_logrt_faceSurprised_rateSurprise, pred = faceSurprised, modx = rateEmotion, interval = TRUE, int.width = .95, 
              dodge.width=.1, geom = "line", vary.lty = TRUE, x.label = "face surprise %", y.label = "mean log(RT)", 
              legend.main = "rating", color.class = "Purples") + 
  scale_x_continuous(breaks=seq(0,100,50)) +
  theme_minimal(base_size=24)
