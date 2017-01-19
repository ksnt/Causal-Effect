### Reference
# Chousa-Kansatsu data no toukei kagaku (Causal estimation, selective bias, data fusion),2009 (in Japanese)
# by T.Hoshino
# and a lecture by the auhor

##
# Causal Effect(Average Treatment Effect) is "(Estimate of ivec1) - (Estimate of ivec2)"
# Standard error is "SQRT((Standard error of ivec1)**2 + (Standard error of ivec1)**2))"

#rm(list = ls()) 

library(survey)
data(lalonde)

treat <- lalonde$treat # independent variable, 0 or 1
logi <- glm(treat ~ age + educ + black + hisp + married + nodegr + re74 + re75,
            family=binomial, data=lalonde)
ivec1 <- lalonde$treat                # allocated variable (if allocated, the value of variables is 1)
ivec2 <- rep(1,nrow(lalonde)) - ivec1 # if not allocated, the value of variables is 1
ivec <- cbind(ivec1,ivec2)
propensity_score <- logi$fitted
iestp1 <- (ivec1/propensity_score)*(length(ivec1)/sum(ivec1))     # Estimate of 
iestp2 <- (ivec2/(1-propensity_score))*(length(ivec2)/sum(ivec2))
iestp <-  iestp1 +iestp2 # assign inversion of estimate of propensity score to weight

psw <- svydesign(ids = ~1, weights = iestp, data = lalonde)
ipwsu <- svyglm(re78 ~ treat, design = psw)
summary(ipwsu) # The result is different from the result in ipw_estimator.R (I guess something wrong...)