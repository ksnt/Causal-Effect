library("MatchIt") # you might have to install MatchiIt library
data(lalonde) #lalonde data

### Reference
# Chousa-Kansatsu data no toukei kagaku (Causal estimation, selective bias, data fusion),2009 (in Japanese)
# by T.Hoshino
# and a lecture by the auhor

### Detail of the data is here:
## http://sekhon.berkeley.edu/matching/lalonde.html
## The data comes from:
# LaLonde, Robert. 1986. ``Evaluating the Econometric Evaluations of Training Programs.'' American Economic Review 76:604-620.
# The paper on the data is 
# Dehejia, Rajeev and Sadek Wahba. 1999.
# ``Causal Effects in Non-Experimental Studies: Re-Evaluating the Evaluation of Training Programs.''
# Journal of the American Statistical Association 94 (448): 1053-1062.
# overview of the data is following 
# summary(lalonde)
# var(lalonde)
# cor(lalonde)
demo("exact") # Demo of exact mathing
demo("subclass") # Stratified analysis 
demo("nearest") # Nearest mathing
demo("optimal")
demo("full")

m.data <- match.data(m.out) #Matched data. m.oust is created thorough above demo
pttest <- lm(re78 ~ treat, data=m.data) #Statistical test
summary(pttest)
