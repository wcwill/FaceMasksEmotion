# faceMask hDDM regression output plotting script: re-plot drift rate of false positives (IDing incorrect emotion) -- rating happy
# to be run after hDDM analysis in Python
# 12/9/20
library(ggplot2)
library(RColorBrewer)

####---- plot drift rate by masks: IDing incorrect emotions ----####
## plot happy drift (sad faces) by mask
setwd("")
dhs <- read.csv('faceMask2_hddm_drift_maskFalsePositives_hs_5000.csv', stringsAsFactors = FALSE)
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
