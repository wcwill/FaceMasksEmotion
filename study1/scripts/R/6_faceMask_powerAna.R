# face mask power analysis (for study 2)
# note: need to have run '1_faceMask_proc.R' and '2b_faceMask_faceRatings_maskFalseNegatives.R' scripts first
# 9/21/20

# running main findings as logistic regressions to estimate odds ratio for G*Power
# G*Power settings:
# test family = z tests
# statistical test = logistic regression
# type of power analysis = a priori: compute required sample size - given a, power, and effect size
# tails = two
# take exponent of betaweight estimate and enter as odds ratio
# calculate mean probability with no mask and enter as 'Pr(Y=1|X=1) H0' (probability face rated as showing emotion under null hypothesis, i.e. no difference from no mask)
# a err prob = .05
# power = .95
# R^2 other X = 0 (only used for analyses with multiple predictors)
# X distribution = binomial (here contrasting mask vs. no mask, so only two levels)
# X parm ∏ = .5 (proportion of trials at each level of contrast, evenly split 50/50 here)


####---- emotion ratings by masks: IDing correct emotions ----####
## rating anger (angry faces)
glmer_daa_rateEmotion_mask <- with(daa, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_daa_rateEmotion_mask) # upper = -1.54445
exp(-1.54445) # 0.2134292
mean(daa$rateEmotion[daa$mask=='none']) # 0.7150497
# target N = 96

## rating disgust (disgusted faces)
glmer_ddd_rateEmotion_mask <- with(ddd, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_ddd_rateEmotion_mask) # lower = -1.5943
exp(-1.5943) # 0.2030506
mean(ddd$rateEmotion[ddd$mask=='none']) # 0.8185328
# target N = 102

## rating fear (fearful faces)
glmer_dff_rateEmotion_mask <- with(dff, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dff_rateEmotion_mask) # upper = -1.4730
exp(-1.4730) # 0.2292367
mean(dff$rateEmotion[dff$mask=='none']) # 0.7630583
# target N = 109

## rating happy (happy faces)
glmer_dhh_rateEmotion_mask <- with(dhh, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dhh_rateEmotion_mask) # lower = -0.7638
exp(-0.7638) # 0.4658927
mean(dhh$rateEmotion[dhh$mask=='none']) # 0.8905891
# target N = 704

## rating sadness (sad faces)
glmer_dss_rateEmotion_mask <- with(dss, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_dss_rateEmotion_mask) # lower = -1.2370
exp(-1.2370) # 0.2902537
mean(dss$rateEmotion[dss$mask=='none']) # 0.8319392
# target N = 182

## rating surprise (surprised faces)
glmer_drr_rateEmotion_mask <- with(drr, glmer(rateEmotion ~ mask + (1 | subject) + (1 | face), family='binomial'))
summary(glmer_drr_rateEmotion_mask) # lower = -1.0979
exp(-1.0979) # 0.3335708
mean(drr$rateEmotion[drr$mask=='none']) # 0.8292308
# target N = 233
