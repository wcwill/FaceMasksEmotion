# faceMask hDDM regression output plotting script: re-plot drift rate of false positives (IDing incorrect emotion) -- rating happy
# to be run after hDDM analysis in Python
# 12/9/20
library(ggplot2)
library(RColorBrewer)

####---- plot drift rate by masks: IDing incorrect emotions ----####
## plot happy drift (angry faces) by mask
setwd("")
dha <- read.csv('faceMask_hddm_drift_maskFalsePositives_ha_5000.csv', stringsAsFactors = FALSE)
rownames(dha) <- dha[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dha["v_C(mask)[T.lower]","mean"], dha["v_Intercept","mean"], dha["v_C(mask)[T.upper]","mean"])
cimin <- c(dha["v_C(mask)[T.lower]","X2.5q"], dha["v_Intercept","X2.5q"], dha["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dha["v_C(mask)[T.lower]","X97.5q"], dha["v_Intercept","X97.5q"], dha["v_C(mask)[T.upper]","X97.5q"])
dha_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dha_drift_mask$value[dha_drift_mask$mask != 'none'] <- (dha_drift_mask$value[dha_drift_mask$mask != 'none'] + dha_drift_mask$value[dha_drift_mask$mask == 'none'])
dha_drift_mask$cimin[dha_drift_mask$mask != 'none'] <- (dha_drift_mask$cimin[dha_drift_mask$mask != 'none'] + dha_drift_mask$value[dha_drift_mask$mask == 'none'])
dha_drift_mask$cimax[dha_drift_mask$mask != 'none'] <- (dha_drift_mask$cimax[dha_drift_mask$mask != 'none'] + dha_drift_mask$value[dha_drift_mask$mask == 'none'])

# flip coefficients
dha_drift_mask$value <- dha_drift_mask$value * -1
dha_drift_mask$cimin <- dha_drift_mask$cimin * -1
dha_drift_mask$cimax <- dha_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Reds")[c(2,3,2)]
ggplot(dha_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,3)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot happy drift (disgust faces) by mask
setwd("")
dhd <- read.csv('faceMask_hddm_drift_maskFalsePositives_hd_5000.csv', stringsAsFactors = FALSE)
rownames(dhd) <- dhd[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dhd["v_C(mask)[T.lower]","mean"], dhd["v_Intercept","mean"], dhd["v_C(mask)[T.upper]","mean"])
cimin <- c(dhd["v_C(mask)[T.lower]","X2.5q"], dhd["v_Intercept","X2.5q"], dhd["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dhd["v_C(mask)[T.lower]","X97.5q"], dhd["v_Intercept","X97.5q"], dhd["v_C(mask)[T.upper]","X97.5q"])
dhd_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dhd_drift_mask$value[dhd_drift_mask$mask != 'none'] <- (dhd_drift_mask$value[dhd_drift_mask$mask != 'none'] + dhd_drift_mask$value[dhd_drift_mask$mask == 'none'])
dhd_drift_mask$cimin[dhd_drift_mask$mask != 'none'] <- (dhd_drift_mask$cimin[dhd_drift_mask$mask != 'none'] + dhd_drift_mask$value[dhd_drift_mask$mask == 'none'])
dhd_drift_mask$cimax[dhd_drift_mask$mask != 'none'] <- (dhd_drift_mask$cimax[dhd_drift_mask$mask != 'none'] + dhd_drift_mask$value[dhd_drift_mask$mask == 'none'])

# flip coefficients
dhd_drift_mask$value <- dhd_drift_mask$value * -1
dhd_drift_mask$cimin <- dhd_drift_mask$cimin * -1
dhd_drift_mask$cimax <- dhd_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Oranges")[c(2,3,2)]
ggplot(dhd_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,3)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot happy drift (sad faces) by mask
setwd("")
dhs <- read.csv('faceMask_hddm_drift_maskFalsePositives_hs_5000.csv', stringsAsFactors = FALSE)
rownames(dhs) <- dhs[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dhs["v_C(mask)[T.lower]","mean"], dhs["v_Intercept","mean"], dhs["v_C(mask)[T.upper]","mean"])
cimin <- c(dhs["v_C(mask)[T.lower]","X2.5q"], dhs["v_Intercept","X2.5q"], dhs["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dhs["v_C(mask)[T.lower]","X97.5q"], dhs["v_Intercept","X97.5q"], dhs["v_C(mask)[T.upper]","X97.5q"])
dhs_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dhs_drift_mask$value[dhs_drift_mask$mask != 'none'] <- (dhs_drift_mask$value[dhs_drift_mask$mask != 'none'] + dhs_drift_mask$value[dhs_drift_mask$mask == 'none'])
dhs_drift_mask$cimin[dhs_drift_mask$mask != 'none'] <- (dhs_drift_mask$cimin[dhs_drift_mask$mask != 'none'] + dhs_drift_mask$value[dhs_drift_mask$mask == 'none'])
dhs_drift_mask$cimax[dhs_drift_mask$mask != 'none'] <- (dhs_drift_mask$cimax[dhs_drift_mask$mask != 'none'] + dhs_drift_mask$value[dhs_drift_mask$mask == 'none'])

# flip coefficients
dhs_drift_mask$value <- dhs_drift_mask$value * -1
dhs_drift_mask$cimin <- dhs_drift_mask$cimin * -1
dhs_drift_mask$cimax <- dhs_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Greys")[c(2,3,2)]
ggplot(dhs_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,3)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot happy drift (surpised faces) by mask
setwd("")
dhr <- read.csv('faceMask_hddm_drift_maskFalsePositives_hr_5000.csv', stringsAsFactors = FALSE)
rownames(dhr) <- dhr[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dhr["v_C(mask)[T.lower]","mean"], dhr["v_Intercept","mean"], dhr["v_C(mask)[T.upper]","mean"])
cimin <- c(dhr["v_C(mask)[T.lower]","X2.5q"], dhr["v_Intercept","X2.5q"], dhr["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dhr["v_C(mask)[T.lower]","X97.5q"], dhr["v_Intercept","X97.5q"], dhr["v_C(mask)[T.upper]","X97.5q"])
dhr_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dhr_drift_mask$value[dhr_drift_mask$mask != 'none'] <- (dhr_drift_mask$value[dhr_drift_mask$mask != 'none'] + dhr_drift_mask$value[dhr_drift_mask$mask == 'none'])
dhr_drift_mask$cimin[dhr_drift_mask$mask != 'none'] <- (dhr_drift_mask$cimin[dhr_drift_mask$mask != 'none'] + dhr_drift_mask$value[dhr_drift_mask$mask == 'none'])
dhr_drift_mask$cimax[dhr_drift_mask$mask != 'none'] <- (dhr_drift_mask$cimax[dhr_drift_mask$mask != 'none'] + dhr_drift_mask$value[dhr_drift_mask$mask == 'none'])

# flip coefficients
dhr_drift_mask$value <- dhr_drift_mask$value * -1
dhr_drift_mask$cimin <- dhr_drift_mask$cimin * -1
dhr_drift_mask$cimax <- dhr_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Purples")[c(2,3,2)]
ggplot(dhr_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,2.5)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
