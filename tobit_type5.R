### Reference
# Chousa-Kansatsu data no toukei kagaku (Causal estimation, selective bias, data fusion),2009 (in Japanese)
# by Prof. T.Hoshino at Keio Univ
# and a lecture by the auhor
###

# Heckman model (Tobit-Type5)
#rm(list = ls()) 
library(Matching)
library(sampleSelection)
data(lalonde)
seleq <- treat ~ age + educ + black + hisp + married + nodegr
outeq <-lalonde$re78 ~ re74+ re75
result <- selection(seleq,list(outeq,outeq), data=lalonde)
# selection(selection equation,list(outcome eq when treat=0,outcome eq when treat=1))
summary(result)

