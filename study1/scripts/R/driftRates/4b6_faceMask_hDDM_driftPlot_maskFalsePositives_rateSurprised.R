# faceMask hDDM regression output plotting script: re-plot drift rate of false positives (IDing incorrect emotion) -- rating surprise
# to be run after hDDM analysis in Python
# 12/9/20
library(ggplot2)
library(RColorBrewer)

####---- plot drift rate by masks: IDing incorrect emotions ----####
## plot surprised drift (angry faces) by mask
setwd("")
dra <- read.csv('faceMask_hddm_drift_maskFalsePositives_ra_5000.csv', stringsAsFactors = FALSE)
rownames(dra) <- dra[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dra["v_C(mask)[T.lower]","mean"], dra["v_Intercept","mean"], dra["v_C(mask)[T.upper]","mean"])
cimin <- c(dra["v_C(mask)[T.lower]","X2.5q"], dra["v_Intercept","X2.5q"], dra["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dra["v_C(mask)[T.lower]","X97.5q"], dra["v_Intercept","X97.5q"], dra["v_C(mask)[T.upper]","X97.5q"])
dra_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dra_drift_mask$value[dra_drift_mask$mask != 'none'] <- (dra_drift_mask$value[dra_drift_mask$mask != 'none'] + dra_drift_mask$value[dra_drift_mask$mask == 'none'])
dra_drift_mask$cimin[dra_drift_mask$mask != 'none'] <- (dra_drift_mask$cimin[dra_drift_mask$mask != 'none'] + dra_drift_mask$value[dra_drift_mask$mask == 'none'])
dra_drift_mask$cimax[dra_drift_mask$mask != 'none'] <- (dra_drift_mask$cimax[dra_drift_mask$mask != 'none'] + dra_drift_mask$value[dra_drift_mask$mask == 'none'])

# flip coefficients
dra_drift_mask$value <- dra_drift_mask$value * -1
dra_drift_mask$cimin <- dra_drift_mask$cimin * -1
dra_drift_mask$cimax <- dra_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Reds")[c(2,3,2)]
ggplot(dra_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,3)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot surprised drift (disgust faces) by mask
setwd("")
drd <- read.csv('faceMask_hddm_drift_maskFalsePositives_rd_5000.csv', stringsAsFactors = FALSE)
rownames(drd) <- drd[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(drd["v_C(mask)[T.lower]","mean"], drd["v_Intercept","mean"], drd["v_C(mask)[T.upper]","mean"])
cimin <- c(drd["v_C(mask)[T.lower]","X2.5q"], drd["v_Intercept","X2.5q"], drd["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(drd["v_C(mask)[T.lower]","X97.5q"], drd["v_Intercept","X97.5q"], drd["v_C(mask)[T.upper]","X97.5q"])
drd_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
drd_drift_mask$value[drd_drift_mask$mask != 'none'] <- (drd_drift_mask$value[drd_drift_mask$mask != 'none'] + drd_drift_mask$value[drd_drift_mask$mask == 'none'])
drd_drift_mask$cimin[drd_drift_mask$mask != 'none'] <- (drd_drift_mask$cimin[drd_drift_mask$mask != 'none'] + drd_drift_mask$value[drd_drift_mask$mask == 'none'])
drd_drift_mask$cimax[drd_drift_mask$mask != 'none'] <- (drd_drift_mask$cimax[drd_drift_mask$mask != 'none'] + drd_drift_mask$value[drd_drift_mask$mask == 'none'])

# flip coefficients
drd_drift_mask$value <- drd_drift_mask$value * -1
drd_drift_mask$cimin <- drd_drift_mask$cimin * -1
drd_drift_mask$cimax <- drd_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Oranges")[c(2,3,2)]
ggplot(drd_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,2)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot surprised drift (fearful faces) by mask
setwd("")
drf <- read.csv('faceMask_hddm_drift_maskFalsePositives_rf_5000.csv', stringsAsFactors = FALSE)
rownames(drf) <- drf[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(drf["v_C(mask)[T.lower]","mean"], drf["v_Intercept","mean"], drf["v_C(mask)[T.upper]","mean"])
cimin <- c(drf["v_C(mask)[T.lower]","X2.5q"], drf["v_Intercept","X2.5q"], drf["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(drf["v_C(mask)[T.lower]","X97.5q"], drf["v_Intercept","X97.5q"], drf["v_C(mask)[T.upper]","X97.5q"])
drf_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
drf_drift_mask$value[drf_drift_mask$mask != 'none'] <- (drf_drift_mask$value[drf_drift_mask$mask != 'none'] + drf_drift_mask$value[drf_drift_mask$mask == 'none'])
drf_drift_mask$cimin[drf_drift_mask$mask != 'none'] <- (drf_drift_mask$cimin[drf_drift_mask$mask != 'none'] + drf_drift_mask$value[drf_drift_mask$mask == 'none'])
drf_drift_mask$cimax[drf_drift_mask$mask != 'none'] <- (drf_drift_mask$cimax[drf_drift_mask$mask != 'none'] + drf_drift_mask$value[drf_drift_mask$mask == 'none'])

# flip coefficients
drf_drift_mask$value <- drf_drift_mask$value * -1
drf_drift_mask$cimin <- drf_drift_mask$cimin * -1
drf_drift_mask$cimax <- drf_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Blues")[c(2,3,2)]
ggplot(drf_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(-1,1)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot surprised drift (happy faces) by mask
setwd("")
drh <- read.csv('faceMask_hddm_drift_maskFalsePositives_rh_5000.csv', stringsAsFactors = FALSE)
rownames(drh) <- drh[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(drh["v_C(mask)[T.lower]","mean"], drh["v_Intercept","mean"], drh["v_C(mask)[T.upper]","mean"])
cimin <- c(drh["v_C(mask)[T.lower]","X2.5q"], drh["v_Intercept","X2.5q"], drh["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(drh["v_C(mask)[T.lower]","X97.5q"], drh["v_Intercept","X97.5q"], drh["v_C(mask)[T.upper]","X97.5q"])
drh_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
drh_drift_mask$value[drh_drift_mask$mask != 'none'] <- (drh_drift_mask$value[drh_drift_mask$mask != 'none'] + drh_drift_mask$value[drh_drift_mask$mask == 'none'])
drh_drift_mask$cimin[drh_drift_mask$mask != 'none'] <- (drh_drift_mask$cimin[drh_drift_mask$mask != 'none'] + drh_drift_mask$value[drh_drift_mask$mask == 'none'])
drh_drift_mask$cimax[drh_drift_mask$mask != 'none'] <- (drh_drift_mask$cimax[drh_drift_mask$mask != 'none'] + drh_drift_mask$value[drh_drift_mask$mask == 'none'])

# flip coefficients
drh_drift_mask$value <- drh_drift_mask$value * -1
drh_drift_mask$cimin <- drh_drift_mask$cimin * -1
drh_drift_mask$cimax <- drh_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Greens")[c(2,3,2)]
ggplot(drh_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,2)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot surprised drift (sad faces) by mask
setwd("")
drs <- read.csv('faceMask_hddm_drift_maskFalsePositives_rs_5000.csv', stringsAsFactors = FALSE)
rownames(drs) <- drs[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(drs["v_C(mask)[T.lower]","mean"], drs["v_Intercept","mean"], drs["v_C(mask)[T.upper]","mean"])
cimin <- c(drs["v_C(mask)[T.lower]","X2.5q"], drs["v_Intercept","X2.5q"], drs["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(drs["v_C(mask)[T.lower]","X97.5q"], drs["v_Intercept","X97.5q"], drs["v_C(mask)[T.upper]","X97.5q"])
drs_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
drs_drift_mask$value[drs_drift_mask$mask != 'none'] <- (drs_drift_mask$value[drs_drift_mask$mask != 'none'] + drs_drift_mask$value[drs_drift_mask$mask == 'none'])
drs_drift_mask$cimin[drs_drift_mask$mask != 'none'] <- (drs_drift_mask$cimin[drs_drift_mask$mask != 'none'] + drs_drift_mask$value[drs_drift_mask$mask == 'none'])
drs_drift_mask$cimax[drs_drift_mask$mask != 'none'] <- (drs_drift_mask$cimax[drs_drift_mask$mask != 'none'] + drs_drift_mask$value[drs_drift_mask$mask == 'none'])

# flip coefficients
drs_drift_mask$value <- drs_drift_mask$value * -1
drs_drift_mask$cimin <- drs_drift_mask$cimin * -1
drs_drift_mask$cimax <- drs_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Greys")[c(2,3,2)]
ggplot(drs_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,2.5)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
