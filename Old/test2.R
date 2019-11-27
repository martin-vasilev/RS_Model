
library(lme4)

load("data/Font_size.Rda")

RS<- subset(RS, undersweep_prob==1)

summary(M1<-lmer(next_sacc_deg~ LandStartVA + (1|sub)+ (1|item), data= RS))

library(effects)

plot(effect('LandStartVA', M1))


summary(M2<- lmer(fix_dur~ LandStartVA + (1|sub)+ (1|item), data= RS))
plot(effect('LandStartVA', M2))
