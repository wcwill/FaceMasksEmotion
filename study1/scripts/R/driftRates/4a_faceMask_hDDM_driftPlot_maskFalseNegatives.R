# faceMask hDDM regression output plotting script: re-plot drift rate of false negatives (failing to ID correct emotion)
# to be run after hDDM analysis in Python
# 12/4/20
library(ggplot2)
library(RColorBrewer)

####---- plot drift rate by masks: IDing correct emotions ----####
## plot all ratings drift (congruent faces) by mask
setwd("")
dxx <- read.csv('faceMask_hddm_faceRatings_maskFalseNegatives_xx_5000.csv', stringsAsFactors = FALSE)
rownames(dxx) <- dxx[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dxx["v_C(mask)[T.lower]","mean"], dxx["v_Intercept","mean"], dxx["v_C(mask)[T.upper]","mean"])
cimin <- c(dxx["v_C(mask)[T.lower]","X2.5q"], dxx["v_Intercept","X2.5q"], dxx["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dxx["v_C(mask)[T.lower]","X97.5q"], dxx["v_Intercept","X97.5q"], dxx["v_C(mask)[T.upper]","X97.5q"])
dxx_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dxx_drift_mask$value[dxx_drift_mask$mask != 'none'] <- (dxx_drift_mask$value[dxx_drift_mask$mask != 'none'] + dxx_drift_mask$value[dxx_drift_mask$mask == 'none'])
dxx_drift_mask$cimin[dxx_drift_mask$mask != 'none'] <- (dxx_drift_mask$cimin[dxx_drift_mask$mask != 'none'] + dxx_drift_mask$value[dxx_drift_mask$mask == 'none'])
dxx_drift_mask$cimax[dxx_drift_mask$mask != 'none'] <- (dxx_drift_mask$cimax[dxx_drift_mask$mask != 'none'] + dxx_drift_mask$value[dxx_drift_mask$mask == 'none'])

# plot
my_palette <- brewer.pal(9, "Greys")[c(2,8,2)]
ggplot(dxx_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1.5)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot angry drift (angry faces) by mask
setwd("")
daa <- read.csv('faceMask_hddm_faceRatings_maskFalseNegatives_aa_5000.csv', stringsAsFactors = FALSE)
rownames(daa) <- daa[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(daa["v_C(mask)[T.lower]","mean"], daa["v_Intercept","mean"], daa["v_C(mask)[T.upper]","mean"])
cimin <- c(daa["v_C(mask)[T.lower]","X2.5q"], daa["v_Intercept","X2.5q"], daa["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(daa["v_C(mask)[T.lower]","X97.5q"], daa["v_Intercept","X97.5q"], daa["v_C(mask)[T.upper]","X97.5q"])
daa_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
daa_drift_mask$value[daa_drift_mask$mask != 'none'] <- (daa_drift_mask$value[daa_drift_mask$mask != 'none'] + daa_drift_mask$value[daa_drift_mask$mask == 'none'])
daa_drift_mask$cimin[daa_drift_mask$mask != 'none'] <- (daa_drift_mask$cimin[daa_drift_mask$mask != 'none'] + daa_drift_mask$value[daa_drift_mask$mask == 'none'])
daa_drift_mask$cimax[daa_drift_mask$mask != 'none'] <- (daa_drift_mask$cimax[daa_drift_mask$mask != 'none'] + daa_drift_mask$value[daa_drift_mask$mask == 'none'])

# plot
my_palette <- brewer.pal(3, "Reds")[c(2,3,2)]
ggplot(daa_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,2.9)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot disgusted drift (disgusted faces) by mask
setwd("")
ddd <- read.csv('faceMask_hddm_faceRatings_maskFalseNegatives_dd_5000.csv', stringsAsFactors = FALSE)
rownames(ddd) <- ddd[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(ddd["v_C(mask)[T.lower]","mean"], ddd["v_Intercept","mean"], ddd["v_C(mask)[T.upper]","mean"])
cimin <- c(ddd["v_C(mask)[T.lower]","X2.5q"], ddd["v_Intercept","X2.5q"], ddd["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(ddd["v_C(mask)[T.lower]","X97.5q"], ddd["v_Intercept","X97.5q"], ddd["v_C(mask)[T.upper]","X97.5q"])
ddd_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
ddd_drift_mask$value[ddd_drift_mask$mask != 'none'] <- (ddd_drift_mask$value[ddd_drift_mask$mask != 'none'] + ddd_drift_mask$value[ddd_drift_mask$mask == 'none'])
ddd_drift_mask$cimin[ddd_drift_mask$mask != 'none'] <- (ddd_drift_mask$cimin[ddd_drift_mask$mask != 'none'] + ddd_drift_mask$value[ddd_drift_mask$mask == 'none'])
ddd_drift_mask$cimax[ddd_drift_mask$mask != 'none'] <- (ddd_drift_mask$cimax[ddd_drift_mask$mask != 'none'] + ddd_drift_mask$value[ddd_drift_mask$mask == 'none'])

# plot
my_palette <- brewer.pal(3, "Oranges")[c(2,3,2)]
ggplot(ddd_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,2.9)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot fearful drift (fearful faces) by mask
setwd("")
dff <- read.csv('faceMask_hddm_faceRatings_maskFalseNegatives_ff_5000.csv', stringsAsFactors = FALSE)
rownames(dff) <- dff[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dff["v_C(mask)[T.lower]","mean"], dff["v_Intercept","mean"], dff["v_C(mask)[T.upper]","mean"])
cimin <- c(dff["v_C(mask)[T.lower]","X2.5q"], dff["v_Intercept","X2.5q"], dff["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dff["v_C(mask)[T.lower]","X97.5q"], dff["v_Intercept","X97.5q"], dff["v_C(mask)[T.upper]","X97.5q"])
dff_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dff_drift_mask$value[dff_drift_mask$mask != 'none'] <- (dff_drift_mask$value[dff_drift_mask$mask != 'none'] + dff_drift_mask$value[dff_drift_mask$mask == 'none'])
dff_drift_mask$cimin[dff_drift_mask$mask != 'none'] <- (dff_drift_mask$cimin[dff_drift_mask$mask != 'none'] + dff_drift_mask$value[dff_drift_mask$mask == 'none'])
dff_drift_mask$cimax[dff_drift_mask$mask != 'none'] <- (dff_drift_mask$cimax[dff_drift_mask$mask != 'none'] + dff_drift_mask$value[dff_drift_mask$mask == 'none'])

# plot
my_palette <- brewer.pal(3, "Blues")[c(2,3,2)]
ggplot(dff_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,2.9)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot happy drift (happy faces) by mask
setwd("")
dhh <- read.csv('faceMask_hddm_faceRatings_maskFalseNegatives_hh_5000.csv', stringsAsFactors = FALSE)
rownames(dhh) <- dhh[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dhh["v_C(mask)[T.lower]","mean"], dhh["v_Intercept","mean"], dhh["v_C(mask)[T.upper]","mean"])
cimin <- c(dhh["v_C(mask)[T.lower]","X2.5q"], dhh["v_Intercept","X2.5q"], dhh["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dhh["v_C(mask)[T.lower]","X97.5q"], dhh["v_Intercept","X97.5q"], dhh["v_C(mask)[T.upper]","X97.5q"])
dhh_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dhh_drift_mask$value[dhh_drift_mask$mask != 'none'] <- (dhh_drift_mask$value[dhh_drift_mask$mask != 'none'] + dhh_drift_mask$value[dhh_drift_mask$mask == 'none'])
dhh_drift_mask$cimin[dhh_drift_mask$mask != 'none'] <- (dhh_drift_mask$cimin[dhh_drift_mask$mask != 'none'] + dhh_drift_mask$value[dhh_drift_mask$mask == 'none'])
dhh_drift_mask$cimax[dhh_drift_mask$mask != 'none'] <- (dhh_drift_mask$cimax[dhh_drift_mask$mask != 'none'] + dhh_drift_mask$value[dhh_drift_mask$mask == 'none'])

# plot
my_palette <- brewer.pal(3, "Greens")[c(2,3,2)]
ggplot(dhh_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,2.9)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot sad drift (sad faces) by mask
setwd("")
dss <- read.csv('faceMask_hddm_faceRatings_maskFalseNegatives_ss_5000.csv', stringsAsFactors = FALSE)
rownames(dss) <- dss[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dss["v_C(mask)[T.lower]","mean"], dss["v_Intercept","mean"], dss["v_C(mask)[T.upper]","mean"])
cimin <- c(dss["v_C(mask)[T.lower]","X2.5q"], dss["v_Intercept","X2.5q"], dss["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dss["v_C(mask)[T.lower]","X97.5q"], dss["v_Intercept","X97.5q"], dss["v_C(mask)[T.upper]","X97.5q"])
dss_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dss_drift_mask$value[dss_drift_mask$mask != 'none'] <- (dss_drift_mask$value[dss_drift_mask$mask != 'none'] + dss_drift_mask$value[dss_drift_mask$mask == 'none'])
dss_drift_mask$cimin[dss_drift_mask$mask != 'none'] <- (dss_drift_mask$cimin[dss_drift_mask$mask != 'none'] + dss_drift_mask$value[dss_drift_mask$mask == 'none'])
dss_drift_mask$cimax[dss_drift_mask$mask != 'none'] <- (dss_drift_mask$cimax[dss_drift_mask$mask != 'none'] + dss_drift_mask$value[dss_drift_mask$mask == 'none'])

# plot
my_palette <- brewer.pal(3, "Greys")[c(2,3,2)]
ggplot(dss_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,2.9)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot surprised drift (surprised faces) by mask
setwd("")
drr <- read.csv('faceMask_hddm_faceRatings_maskFalseNegatives_rr_5000.csv', stringsAsFactors = FALSE)
rownames(drr) <- drr[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(drr["v_C(mask)[T.lower]","mean"], drr["v_Intercept","mean"], drr["v_C(mask)[T.upper]","mean"])
cimin <- c(drr["v_C(mask)[T.lower]","X2.5q"], drr["v_Intercept","X2.5q"], drr["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(drr["v_C(mask)[T.lower]","X97.5q"], drr["v_Intercept","X97.5q"], drr["v_C(mask)[T.upper]","X97.5q"])
drr_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
drr_drift_mask$value[drr_drift_mask$mask != 'none'] <- (drr_drift_mask$value[drr_drift_mask$mask != 'none'] + drr_drift_mask$value[drr_drift_mask$mask == 'none'])
drr_drift_mask$cimin[drr_drift_mask$mask != 'none'] <- (drr_drift_mask$cimin[drr_drift_mask$mask != 'none'] + drr_drift_mask$value[drr_drift_mask$mask == 'none'])
drr_drift_mask$cimax[drr_drift_mask$mask != 'none'] <- (drr_drift_mask$cimax[drr_drift_mask$mask != 'none'] + drr_drift_mask$value[drr_drift_mask$mask == 'none'])

# plot
my_palette <- brewer.pal(3, "Purples")[c(2,3,2)]
ggplot(drr_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,2.9)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
