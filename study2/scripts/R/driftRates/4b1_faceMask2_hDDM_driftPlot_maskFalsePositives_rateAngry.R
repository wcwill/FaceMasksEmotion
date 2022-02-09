# faceMask hDDM regression output plotting script: re-plot drift rate of false positives (IDing incorrect emotion) -- rating angry
# to be run after hDDM analysis in Python
# 12/9/20
library(ggplot2)
library(RColorBrewer)

####---- plot drift rate by masks: IDing incorrect emotions ----####
## plot angry drift (disgusted faces) by mask
setwd("")
dad <- read.csv('faceMask2_hddm_drift_maskFalsePositives_ad_5000.csv', stringsAsFactors = FALSE)
rownames(dad) <- dad[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dad["v_C(mask)[T.lower]","mean"], dad["v_Intercept","mean"], dad["v_C(mask)[T.upper]","mean"])
cimin <- c(dad["v_C(mask)[T.lower]","X2.5q"], dad["v_Intercept","X2.5q"], dad["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dad["v_C(mask)[T.lower]","X97.5q"], dad["v_Intercept","X97.5q"], dad["v_C(mask)[T.upper]","X97.5q"])
dad_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dad_drift_mask$value[dad_drift_mask$mask != 'none'] <- (dad_drift_mask$value[dad_drift_mask$mask != 'none'] + dad_drift_mask$value[dad_drift_mask$mask == 'none'])
dad_drift_mask$cimin[dad_drift_mask$mask != 'none'] <- (dad_drift_mask$cimin[dad_drift_mask$mask != 'none'] + dad_drift_mask$value[dad_drift_mask$mask == 'none'])
dad_drift_mask$cimax[dad_drift_mask$mask != 'none'] <- (dad_drift_mask$cimax[dad_drift_mask$mask != 'none'] + dad_drift_mask$value[dad_drift_mask$mask == 'none'])

# flip coefficients
dad_drift_mask$value <- dad_drift_mask$value * -1
dad_drift_mask$cimin <- dad_drift_mask$cimin * -1
dad_drift_mask$cimax <- dad_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Oranges")[c(2,3,2)]
ggplot(dad_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(-1,2)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot angry drift (happy faces) by mask
setwd("")
dah <- read.csv('faceMask2_hddm_drift_maskFalsePositives_ah_5000.csv', stringsAsFactors = FALSE)
rownames(dah) <- dah[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dah["v_C(mask)[T.lower]","mean"], dah["v_Intercept","mean"], dah["v_C(mask)[T.upper]","mean"])
cimin <- c(dah["v_C(mask)[T.lower]","X2.5q"], dah["v_Intercept","X2.5q"], dah["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dah["v_C(mask)[T.lower]","X97.5q"], dah["v_Intercept","X97.5q"], dah["v_C(mask)[T.upper]","X97.5q"])
dah_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dah_drift_mask$value[dah_drift_mask$mask != 'none'] <- (dah_drift_mask$value[dah_drift_mask$mask != 'none'] + dah_drift_mask$value[dah_drift_mask$mask == 'none'])
dah_drift_mask$cimin[dah_drift_mask$mask != 'none'] <- (dah_drift_mask$cimin[dah_drift_mask$mask != 'none'] + dah_drift_mask$value[dah_drift_mask$mask == 'none'])
dah_drift_mask$cimax[dah_drift_mask$mask != 'none'] <- (dah_drift_mask$cimax[dah_drift_mask$mask != 'none'] + dah_drift_mask$value[dah_drift_mask$mask == 'none'])

# flip coefficients
dah_drift_mask$value <- dah_drift_mask$value * -1
dah_drift_mask$cimin <- dah_drift_mask$cimin * -1
dah_drift_mask$cimax <- dah_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Greens")[c(2,3,2)]
ggplot(dah_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,3.5)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot angry drift (surprised faces) by mask
setwd("")
dar <- read.csv('faceMask2_hddm_drift_maskFalsePositives_ar_5000.csv', stringsAsFactors = FALSE)
rownames(dar) <- dar[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dar["v_C(mask)[T.lower]","mean"], dar["v_Intercept","mean"], dar["v_C(mask)[T.upper]","mean"])
cimin <- c(dar["v_C(mask)[T.lower]","X2.5q"], dar["v_Intercept","X2.5q"], dar["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dar["v_C(mask)[T.lower]","X97.5q"], dar["v_Intercept","X97.5q"], dar["v_C(mask)[T.upper]","X97.5q"])
dar_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dar_drift_mask$value[dar_drift_mask$mask != 'none'] <- (dar_drift_mask$value[dar_drift_mask$mask != 'none'] + dar_drift_mask$value[dar_drift_mask$mask == 'none'])
dar_drift_mask$cimin[dar_drift_mask$mask != 'none'] <- (dar_drift_mask$cimin[dar_drift_mask$mask != 'none'] + dar_drift_mask$value[dar_drift_mask$mask == 'none'])
dar_drift_mask$cimax[dar_drift_mask$mask != 'none'] <- (dar_drift_mask$cimax[dar_drift_mask$mask != 'none'] + dar_drift_mask$value[dar_drift_mask$mask == 'none'])

# flip coefficients
dar_drift_mask$value <- dar_drift_mask$value * -1
dar_drift_mask$cimin <- dar_drift_mask$cimin * -1
dar_drift_mask$cimax <- dar_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Purples")[c(2,3,2)]
ggplot(dar_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(-1,3)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
