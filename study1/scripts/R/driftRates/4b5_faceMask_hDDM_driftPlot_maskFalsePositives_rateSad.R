# faceMask hDDM regression output plotting script: re-plot drift rate of false positives (IDing incorrect emotion) -- rating sad
# to be run after hDDM analysis in Python
# 12/9/20
library(ggplot2)
library(RColorBrewer)

####---- plot drift rate by masks: IDing incorrect emotions ----####
## plot sad drift (angry faces) by mask
setwd("")
dsa <- read.csv('faceMask_hddm_drift_maskFalsePositives_sa_5000.csv', stringsAsFactors = FALSE)
rownames(dsa) <- dsa[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dsa["v_C(mask)[T.lower]","mean"], dsa["v_Intercept","mean"], dsa["v_C(mask)[T.upper]","mean"])
cimin <- c(dsa["v_C(mask)[T.lower]","X2.5q"], dsa["v_Intercept","X2.5q"], dsa["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dsa["v_C(mask)[T.lower]","X97.5q"], dsa["v_Intercept","X97.5q"], dsa["v_C(mask)[T.upper]","X97.5q"])
dsa_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dsa_drift_mask$value[dsa_drift_mask$mask != 'none'] <- (dsa_drift_mask$value[dsa_drift_mask$mask != 'none'] + dsa_drift_mask$value[dsa_drift_mask$mask == 'none'])
dsa_drift_mask$cimin[dsa_drift_mask$mask != 'none'] <- (dsa_drift_mask$cimin[dsa_drift_mask$mask != 'none'] + dsa_drift_mask$value[dsa_drift_mask$mask == 'none'])
dsa_drift_mask$cimax[dsa_drift_mask$mask != 'none'] <- (dsa_drift_mask$cimax[dsa_drift_mask$mask != 'none'] + dsa_drift_mask$value[dsa_drift_mask$mask == 'none'])

# flip coefficients
dsa_drift_mask$value <- dsa_drift_mask$value * -1
dsa_drift_mask$cimin <- dsa_drift_mask$cimin * -1
dsa_drift_mask$cimax <- dsa_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Reds")[c(2,3,2)]
ggplot(dsa_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(-0.5,1)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot sad drift (fearful faces) by mask
setwd("")
dsf <- read.csv('faceMask_hddm_drift_maskFalsePositives_sf_5000.csv', stringsAsFactors = FALSE)
rownames(dsf) <- dsf[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dsf["v_C(mask)[T.lower]","mean"], dsf["v_Intercept","mean"], dsf["v_C(mask)[T.upper]","mean"])
cimin <- c(dsf["v_C(mask)[T.lower]","X2.5q"], dsf["v_Intercept","X2.5q"], dsf["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dsf["v_C(mask)[T.lower]","X97.5q"], dsf["v_Intercept","X97.5q"], dsf["v_C(mask)[T.upper]","X97.5q"])
dsf_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dsf_drift_mask$value[dsf_drift_mask$mask != 'none'] <- (dsf_drift_mask$value[dsf_drift_mask$mask != 'none'] + dsf_drift_mask$value[dsf_drift_mask$mask == 'none'])
dsf_drift_mask$cimin[dsf_drift_mask$mask != 'none'] <- (dsf_drift_mask$cimin[dsf_drift_mask$mask != 'none'] + dsf_drift_mask$value[dsf_drift_mask$mask == 'none'])
dsf_drift_mask$cimax[dsf_drift_mask$mask != 'none'] <- (dsf_drift_mask$cimax[dsf_drift_mask$mask != 'none'] + dsf_drift_mask$value[dsf_drift_mask$mask == 'none'])

# flip coefficients
dsf_drift_mask$value <- dsf_drift_mask$value * -1
dsf_drift_mask$cimin <- dsf_drift_mask$cimin * -1
dsf_drift_mask$cimax <- dsf_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Blues")[c(2,3,2)]
ggplot(dsf_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,1.5)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")


## plot sad drift (happy faces) by mask
setwd("")
dsh <- read.csv('faceMask_hddm_drift_maskFalsePositives_sh_5000.csv', stringsAsFactors = FALSE)
rownames(dsh) <- dsh[,1]

# generate summary dataframe
mask <- c('lower', 'none', 'upper')
value <- c(dsh["v_C(mask)[T.lower]","mean"], dsh["v_Intercept","mean"], dsh["v_C(mask)[T.upper]","mean"])
cimin <- c(dsh["v_C(mask)[T.lower]","X2.5q"], dsh["v_Intercept","X2.5q"], dsh["v_C(mask)[T.upper]","X2.5q"])
cimax <- c(dsh["v_C(mask)[T.lower]","X97.5q"], dsh["v_Intercept","X97.5q"], dsh["v_C(mask)[T.upper]","X97.5q"])
dsh_drift_mask <- data.frame(mask, value, cimin, cimax)

# add baseline mask condition (i.e. no mask) intercept value to comparison mask conditions (i.e. lower/upper) to calculate non-relative coefficients and CIs
dsh_drift_mask$value[dsh_drift_mask$mask != 'none'] <- (dsh_drift_mask$value[dsh_drift_mask$mask != 'none'] + dsh_drift_mask$value[dsh_drift_mask$mask == 'none'])
dsh_drift_mask$cimin[dsh_drift_mask$mask != 'none'] <- (dsh_drift_mask$cimin[dsh_drift_mask$mask != 'none'] + dsh_drift_mask$value[dsh_drift_mask$mask == 'none'])
dsh_drift_mask$cimax[dsh_drift_mask$mask != 'none'] <- (dsh_drift_mask$cimax[dsh_drift_mask$mask != 'none'] + dsh_drift_mask$value[dsh_drift_mask$mask == 'none'])

# flip coefficients
dsh_drift_mask$value <- dsh_drift_mask$value * -1
dsh_drift_mask$cimin <- dsh_drift_mask$cimin * -1
dsh_drift_mask$cimax <- dsh_drift_mask$cimax * -1

# plot
my_palette <- brewer.pal(3, "Greens")[c(2,3,2)]
ggplot(dsh_drift_mask, aes(x=mask, y=value, fill = mask)) + 
  geom_bar(color = "black", stat = "identity") +
  geom_errorbar(aes(ymin=cimin, ymax=cimax), width=.2) +
  scale_fill_manual(values=c(my_palette[1], my_palette[2], my_palette[3])) +
  coord_cartesian(ylim=c(0,3)) +
  ylab("drift rate (v) coefficient") +
  theme_minimal(base_size=24) +
  theme(legend.position="none")
