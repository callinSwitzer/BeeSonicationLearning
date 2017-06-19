file.choose()

foo <- read.csv("/Users/callinswitzer/Desktop/fourdays.csv")
cmp <- foo


#cmp <- foo[complete.cases(foo), ]
head(cmp)

cmp$MeanFirstTrial <- ifelse(cmp$treatment == "RewFir", cmp$meanFreqRewarded, cmp$meanFreqUnrewarded)

cmp$MeanSecondTrial <- ifelse(cmp$treatment == "RewSec", cmp$meanFreqRewarded, cmp$meanFreqUnrewarded)

head(cmp)

library(ggplot2)
theme_set(theme_bw())
ggplot(cmp, aes(x = treatment, y = MeanFirstTrial)) + 
     geom_boxplot() + 
     geom_point()

ggplot(cmp, aes(x = treatment, y = MeanSecondTrial)) + 
     geom_boxplot() + 
     geom_point()



cmp$diff <- cmp$meanFreqRewarded - cmp$meanFreqUnrewarded

head(cmp)

xtabs(~cmp$treatment)

# make into long data frame
library(tidyr)
cmp_long <- gather(foo, condition, meanFreq, meanFreqRewarded, meanFreqUnrewarded)
head(cmp_long)

cmp_order <- gather(cmp, order, meanFreq, MeanFirstTrial, MeanSecondTrial)



ggplot(cmp_long, aes(x = condition, y = meanFreq )) + 
     geom_boxplot() + 
     facet_wrap(~ treatment)

ggplot(cmp_order, aes(x = order, y = meanFreq )) + 
     geom_boxplot() + 
     facet_wrap(~ treatment)


t.test(cmp$meanFreqRewarded, cmp$meanFreqUnrewarded, paired = TRUE)

library(lme4)
mod1 <- lmer(meanFreq ~ condition * treatment + (1|beeNum), data = cmp_long)

summary(mod1)

mod1.2 <- update(mod1, .~. - condition:treatment)
anova(mod1.2, mod1) # interaction


plot(lmer(meanFreq ~ treatment +  (1|beeNum), data = cmp_long))

plot(mod1)










mod1.1 <- update(mod1, .~. - condition)
anova(mod1, mod1.1)


mod2 <- update(mod1, .~. - treatment)
anova(mod2, mod1)

summary(mod2)

mod3 <- update(mod2, .~. - condition)
anova(mod2, mod3)


mod2 <- lm()

head(cmp)
