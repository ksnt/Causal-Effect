### Reference
# Chousa-Kansatsu data no toukei kagaku (Causal estimation, selective bias, data fusion),2009 (in Japanese)
# by T.Hoshino
# and a lecture by the auhor

#rm(list = ls()) 

library(Mathcing)
data(lalonde)
logi <- glm(treat ~ age + educ + black + hisp + married + nodegr 
           + re74 + re75, family=binomial, data=lalonde)
kmy <- lalonde$re74 # dependent variable
ivec1 <- lalonde$treat
estp <- logi$fitted # propensity score
km <- cbind(kmy,estp,ivec1) # dataset for kernel matching
km1 <- subset(km, ivec1==1) # assigned
km2 <- subset(km, ivec1==0) # not assigned
km1x <- km1[,2]
km1y <- km1[,1]
km2x <- km2[,2]
km2y <- km2[,1]
bw1 <- 1.06 * (nrow(km1))^(-0.2) * sd(km1x) # deciding band width
bw2 <- 1.06 * (nrow(km2))^(-0.2) * sd(km2x)
esty1 <- ksmooth(x=km1x, y=km1y, kernel = "normal", bandwidth = bw1, x.points=km2x)
esty0 <- ksmooth(x=km2x, y=km2y, kernel = "normal", bandwidth = bw2, x.points=km1x)
# esty0$y is a prediction value vector on y_0 when z=1
# esty1$y is a prediction value vector on y_1 when z=0
