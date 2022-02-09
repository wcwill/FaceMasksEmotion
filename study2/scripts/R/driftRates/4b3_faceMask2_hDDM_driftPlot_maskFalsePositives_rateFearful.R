# faceMask hDDM regression output plotting script: re-plot drift rate of false positives (IDing incorrect emotion) -- rating fearful
# to be run after hDDM analysis in Python
# 12/9/20
library(ggplot2)
library(RColorBrewer)

####---- plot drift rate by masks: IDing incorrect emotions ----####
## plot fearful drift (angry faces) by mask
setwd("")
dfa <- read.csv('faceMask2_hddm_drift_maskFalsePositives_fa_5000.csv', stringsAsFactors = FALSE)
rownames(dfa) <- dfa[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dfa["v_C(mask)[T.lower]","mean"], dfa["v_Intercept","mean"], dfa["v_C(mask)[T.upper]","mean"])
cimin <- c(dfa["v_C(mask)[T.lower]","X2.5q"], dfa["v_Intercept","X2.5q"], dfa["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dfa["v_C(mask)[T.lower]","X97.5q"], dfa["v_Intercept","X97.5q"], dfa["v_C(mask)[T.upper]","X97.5q"])
dfa_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dfa_drift_mask$value[dfa_drift_mask$mask != 'none'] <- (dfa_drift_mask$value[dfa_drift_mask$mask != 'none'] + dfa_drift_mask$value[dfa_drift_mask$mask == 'none'])
dfa_drift_mask$cimin[dfa_drift_mask$mask != 'none'] <- (dfa_drift_mask$cimin[dfa_drift_mask$mask != 'none'] + dfa_drift_mask$value[dfa_drift_mask$mask == 'none'])
dfa_drift_mask$cimax[dfa_drift_mask$mask != 'none'] <- (dfa_drift_mask$cimax[dfa_drift_mask$mask != 'none'] + dfa_drift_mask$value[dfa_drift_mask$mask == 'none'])

# flip coefficients
dfa_drift_mask$value <- dfa_drift_mask$value * -1
dfa_drift_mask$cimin <- dfa_drift_mask$cimin * -1
dfa_drift_mask$cimax <- dfa_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Reds")[c(2,3,2)]
ggplot(dfa_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,2.7)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot fearful drift (surprised faces) by mask
setwd("")
dfr <- read.csv('faceMask2_hddm_drift_maskFalsePositives_fr_5000.csv', stringsAsFactors = FALSE)
rownames(dfr) <- dfr[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dfr["v_C(mask)[T.lower]","mean"], dfr["v_Intercept","mean"], dfr["v_C(mask)[T.upper]","mean"])
cimin <- c(dfr["v_C(mask)[T.lower]","X2.5q"], dfr["v_Intercept","X2.5q"], dfr["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dfr["v_C(mask)[T.lower]","X97.5q"], dfr["v_Intercept","X97.5q"], dfr["v_C(mask)[T.upper]","X97.5q"])
dfr_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dfr_drift_mask$value[dfr_drift_mask$mask != 'none'] <- (dfr_drift_mask$value[dfr_drift_mask$mask != 'none'] + dfr_drift_mask$value[dfr_drift_mask$mask == 'none'])
dfr_drift_mask$cimin[dfr_drift_mask$mask != 'none'] <- (dfr_drift_mask$cimin[dfr_drift_mask$mask != 'none'] + dfr_drift_mask$value[dfr_drift_mask$mask == 'none'])
dfr_drift_mask$cimax[dfr_drift_mask$mask != 'none'] <- (dfr_drift_mask$cimax[dfr_drift_mask$mask != 'none'] + dfr_drift_mask$value[dfr_drift_mask$mask == 'none'])

# flip coefficients
dfr_drift_mask$value <- dfr_drift_mask$value * -1
dfr_drift_mask$cimin <- dfr_drift_mask$cimin * -1
dfr_drift_mask$cimax <- dfr_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Purples")[c(2,3,2)]
ggplot(dfr_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(-1,3)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot fearful drift (sad faces) by mask
setwd("")
dfs <- read.csv('faceMask2_hddm_drift_maskFalsePositives_fs_5000.csv', stringsAsFactors = FALSE)
rownames(dfs) <- dfs[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dfs["v_C(mask)[T.lower]","mean"], dfs["v_Intercept","mean"], dfs["v_C(mask)[T.upper]","mean"])
cimin <- c(dfs["v_C(mask)[T.lower]","X2.5q"], dfs["v_Intercept","X2.5q"], dfs["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dfs["v_C(mask)[T.lower]","X97.5q"], dfs["v_Intercept","X97.5q"], dfs["v_C(mask)[T.upper]","X97.5q"])
dfs_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dfs_drift_mask$value[dfs_drift_mask$mask != 'none'] <- (dfs_drift_mask$value[dfs_drift_mask$mask != 'none'] + dfs_drift_mask$value[dfs_drift_mask$mask == 'none'])
dfs_drift_mask$cimin[dfs_drift_mask$mask != 'none'] <- (dfs_drift_mask$cimin[dfs_drift_mask$mask != 'none'] + dfs_drift_mask$value[dfs_drift_mask$mask == 'none'])
dfs_drift_mask$cimax[dfs_drift_mask$mask != 'none'] <- (dfs_drift_mask$cimax[dfs_drift_mask$mask != 'none'] + dfs_drift_mask$value[dfs_drift_mask$mask == 'none'])

# flip coefficients
dfs_drift_mask$value <- dfs_drift_mask$value * -1
dfs_drift_mask$cimin <- dfs_drift_mask$cimin * -1
dfs_drift_mask$cimax <- dfs_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Greys")[c(2,3,2)]
ggplot(dfs_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,3)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
