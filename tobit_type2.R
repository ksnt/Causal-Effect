### Reference
# Chousa-Kansatsu data no toukei kagaku (Causal estimation, selective bias, data fusion),2009 (in Japanese)
# by T.Hoshino
# and a lecture by the auhor

#rm(list = ls()) 

library(sampleSelection)
data(Mroz87) 
Mroz87$kids <- ( Mroz87$kids5 + Mroz87$kids618 > 0 ) # Mroz87$kids5: The number of kids whose age is under 5, Mroz87$kids618: The number of kids whose age is between 6 and 18
ml <- selection(lfp ~ age + I(age^2) + faminc + kids + educ,
                wage ~ exper + I(exper^2) + educ + city, data=Mroz87 )    # Mroz87$lfp: if working then 1, if not then 0
summary(ml)
