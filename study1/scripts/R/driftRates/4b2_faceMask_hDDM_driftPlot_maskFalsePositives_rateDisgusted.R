# faceMask hDDM regression output plotting script: re-plot drift rate of false positives (IDing incorrect emotion) -- rating disgusted
# to be run after hDDM analysis in Python
# 12/9/20
library(ggplot2)
library(RColorBrewer)

####---- plot drift rate by masks: IDing incorrect emotions ----####
## plot disgusted drift (angry faces) by mask
setwd("")
dda <- read.csv('faceMask_hddm_faceRatings_maskFalsePositives_da_5000.csv', stringsAsFactors = FALSE)
rownames(dda) <- dda[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dda["v_C(mask)[T.lower]","mean"], dda["v_Intercept","mean"], dda["v_C(mask)[T.upper]","mean"])
cimin <- c(dda["v_C(mask)[T.lower]","X2.5q"], dda["v_Intercept","X2.5q"], dda["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dda["v_C(mask)[T.lower]","X97.5q"], dda["v_Intercept","X97.5q"], dda["v_C(mask)[T.upper]","X97.5q"])
dda_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dda_drift_mask$value[dda_drift_mask$mask != 'none'] <- (dda_drift_mask$value[dda_drift_mask$mask != 'none'] + dda_drift_mask$value[dda_drift_mask$mask == 'none'])
dda_drift_mask$cimin[dda_drift_mask$mask != 'none'] <- (dda_drift_mask$cimin[dda_drift_mask$mask != 'none'] + dda_drift_mask$value[dda_drift_mask$mask == 'none'])
dda_drift_mask$cimax[dda_drift_mask$mask != 'none'] <- (dda_drift_mask$cimax[dda_drift_mask$mask != 'none'] + dda_drift_mask$value[dda_drift_mask$mask == 'none'])

# flip coefficients
dda_drift_mask$value <- dda_drift_mask$value * -1
dda_drift_mask$cimin <- dda_drift_mask$cimin * -1
dda_drift_mask$cimax <- dda_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Reds")[c(2,3,2)]
ggplot(dda_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot disgusted drift (fearful faces) by mask
setwd("")
ddf <- read.csv('faceMask_hddm_faceRatings_maskFalsePositives_df_5000.csv', stringsAsFactors = FALSE)
rownames(ddf) <- ddf[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(ddf["v_C(mask)[T.lower]","mean"], ddf["v_Intercept","mean"], ddf["v_C(mask)[T.upper]","mean"])
cimin <- c(ddf["v_C(mask)[T.lower]","X2.5q"], ddf["v_Intercept","X2.5q"], ddf["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(ddf["v_C(mask)[T.lower]","X97.5q"], ddf["v_Intercept","X97.5q"], ddf["v_C(mask)[T.upper]","X97.5q"])
ddf_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
ddf_drift_mask$value[ddf_drift_mask$mask != 'none'] <- (ddf_drift_mask$value[ddf_drift_mask$mask != 'none'] + ddf_drift_mask$value[ddf_drift_mask$mask == 'none'])
ddf_drift_mask$cimin[ddf_drift_mask$mask != 'none'] <- (ddf_drift_mask$cimin[ddf_drift_mask$mask != 'none'] + ddf_drift_mask$value[ddf_drift_mask$mask == 'none'])
ddf_drift_mask$cimax[ddf_drift_mask$mask != 'none'] <- (ddf_drift_mask$cimax[ddf_drift_mask$mask != 'none'] + ddf_drift_mask$value[ddf_drift_mask$mask == 'none'])

# flip coefficients
ddf_drift_mask$value <- ddf_drift_mask$value * -1
ddf_drift_mask$cimin <- ddf_drift_mask$cimin * -1
ddf_drift_mask$cimax <- ddf_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Blues")[c(2,3,2)]
ggplot(ddf_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(-0.5,1)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
