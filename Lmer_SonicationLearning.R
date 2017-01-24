## Callin Switzer
## 23 Jan 2016
## Multilevel model to visualize bees'
## behavior on the artificial pollen system


#install packages
ipak <- function(pkg){
     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
     if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
     sapply(pkg, require, character.only = TRUE)
}

packages <- c("gsheet", "ggplot2", "reshape2", "pwr", 'lme4', 'sjPlot')
ipak(packages)

theme_set(theme_bw())


sl <- read.csv('freqLearn2.csv')

# remove test rows
sl <- sl[sl$beeCol != 'TESTTESTPHOTO',]


# TODO: check why there are some values lower than 220 and higher than 450
sl <- sl[sl$freq > 220 & sl$freq < 450,]

# plot each bee's frequency over time by treatment
ggplot(sl, aes(x = freq, y = amp)) + 
     geom_point()


## TODO, add IT Span to dataset


trt <- character()

unique(interaction(sl$lowFrq, sl$highFrq))
     
for(ii in 1:nrow(sl)){
     low <- sl$lowFrq[ii]
     high <- sl$highFrq[ii]
     if(low == 220  & high == 450) trt[ii] = 'initial'
     else if(low == 500) trt[ii] = 'unrewarded'
     else if(low > 250) trt[ii] = 'high'
     else if(low < 250 & high <= 350) trt[ii] = 'low'
}


sl$trt <- trt

# TODO: exclude individuals that have no "initial" value

# set reference level to initial
sl$trt <- relevel(as.factor(sl$trt), ref = "initial")

# convert to datetime
options(digits.secs = 5)
sl$datetime_str <- as.POSIXct(sl$datetime_str)



# multilevel model
m1 <- lmer(freq ~ trialNum*index*trt +  (1|beeCol), data = sl)
summary(m1)

m2 <- update(m1, .~. -trialNum:index:trt )
anova(m1, m2) # keep 3-way interaction


# sjp.lmer(m1, type = 'fe.cor')
# sjp.lmer(m1, type = 're.qq')
# sjp.lmer(m1, type = 'fe.pc', p.kr = FALSE)
# sjp.lmer(m1, type = 'ri.pc', facet.grid = FALSE, p.kr = FALSE)



plot(m1)

qqnorm(resid(m1), main = "")
qqline(resid(m1)) # not too bad
plot(fitted(m1), resid(m1), xlab = "fitted", ylab = "residuals")
abline(0,0)


# QQPlot for group-level effects
qqnorm(ranef(m1)$beeCol[[1]], main="Normal Q-Q plot for random effects")
qqline(ranef(m1)$beeCol[[1]])

# QQPlot for group-level effects
qqnorm(ranef(m1)$trialNum[[1]], main="Normal Q-Q plot for random effects")
qqline(ranef(m1)$trialNum[[1]])  

# QQPlot for group-level effects
qqnorm(ranef(m1)$`trialNum:beeCol`[[1]], main="Normal Q-Q plot for random effects")
qqline(ranef(m1)$`trialNum:beeCol`[[1]]) 



pdf <- sl[1,]
pdf$trt = 'initial'

predict(m1, newdata = pdf, re.form = NA)

sl[is.na(sl$trt), ]

ggplot(sl[sl$trialNum > 4,], aes(x = trt, y = freq)) + 
     geom_boxplot() + 
     geom_point(position = position_jitter(), alpha = 0.01)

# calculate mean by beeCol
beeMeans <- tapply(sl$freq, INDEX = sl$beeCol,FUN = mean )

tapply(sl$freq, INDEX = sl$trt, FUN = mean)

# get bees' most common treatmens
beeTrts <- tapply(sl$trt, INDEX = sl$beeCol,FUN = function(x) names(table(x))[which.max(table(x))]) 

# see treatments for each bee
tapply(sl$trt, INDEX = sl$beeCol,FUN = function(x) table(x)) 

data.frame(beeTrts)


avgDF <- data.frame(beeMeans, beeTrts)
ggplot(avgDF, aes(x = beeTrts, y = beeMeans)) + 
     geom_boxplot() + 
     geom_point(position = position_jitter(), alpha = 0.2)


