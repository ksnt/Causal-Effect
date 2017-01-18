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
# Journal of the American Statistical Association 94 (448): 1053-

library(Matching) 
data(lalonde)

y78 <- lalonde$re78 # Y78 is an dependent variable
                    # re78 -> "real earnings in 1978."
treat <- lalonde$treat # independent variable, 0 or 1
logi <- glm(treat ~ age + educ + black + hisp + married + nodegr + re74 + re75,
            family=binomial, data=lalonde)

# Y  -> dependent variable
# Tr -> independent variable
# X  -> esitmation of prospenty score
matching_outcome <- Match(Y=y78, Tr=treat, X=logi$fitted) 
summary(matching_outcome)

#A character string for the estimand. The default estimand is "ATT", 
#the sample average treatment effect for the treated. "ATE" is the sample average treatment effect,
#and "ATC" is the sample average treatment effect for the controls.
matching_outcome2 <- Match(Y=y78, Tr=treat, X=logi$fitted, estimand="ATE")
summary(matching_outcome2)

MatchBalance(treat ~ age + educ + black + hisp + married 
             + nodegr + re74 + re75, match.out=matching_outcome , nboots=1000, data=lalonde)
