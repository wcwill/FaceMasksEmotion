# faceMask hDDM processing script (for analysis in Python)
# note: need to have run '1_faceMask_proc.R' script first
# 11/25/20
setwd("")

# drop columns of no interest
d <- subset(d, select = -c(subjectOS, subjectBrowser, keyPair, key_press))

# subset RTs > 100 ms (see Luce, 1986; Whelan, 2008)
d <- subset(d, rt >= 100)

# format RTs in seconds
d$rt <- d$rt/1000

# drop top 0.5% of RTs to make model run (otherwise produces stochastic wfpt error)
rt995 <- quantile(d$rt, probs = seq(0, 1, by= 0.005))[200]
d <- subset(d, rt <= rt995)

# rename subject ID and rateEmotion variables
colnames(d)[colnames(d)=='subject'] <- 'subj_idx'
colnames(d)[colnames(d)=='rateEmotion'] <- 'response'

# rename 'none' level of mask
d$mask <- as.character(d$mask)
d$mask[d$mask=='none'] <- 'baseline'

# write CSV with hDDM-processed data
# write.csv(d, "faceMask_hddm_proc.csv", row.names = FALSE)
