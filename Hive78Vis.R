file.choose()

foo <- read.csv("/Users/callinswitzer/Desktop/fourdays.csv")


cmp <- foo[complete.cases(foo), ]
head(cmp)

cmp$MeanFirstTrial <- ifelse(cmp$treatment == "R")

cmp$diff <- cmp$meanFreqRewarded - cmp$meanFreqUnrewarded

xtabs(~cmp$treatment)

# make into long data frame
library(tidyr)
cmp_long <- gather(foo, condition, meanFreq, meanFreqRewarded, meanFreqUnrewarded)
head(cmp_long)

library(ggplot2)
theme_set(theme_bw())

ggplot(cmp_long, aes(x = condition, y = meanFreq )) + 
     geom_boxplot() + 
     facet_wrap(~ treatment)


t.test(cmp$meanFreqRewarded, cmp$meanFreqUnrewarded, paired = TRUE)


mod1 <- lm(diff ~ treatment , data = cmp)

summary(mod1)

mod2 <- lm()

head(cmp)
