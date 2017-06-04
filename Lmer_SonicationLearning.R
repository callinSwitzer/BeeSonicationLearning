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

packages <- c("gsheet", "ggplot2", "reshape2", "pwr", 'lme4', 'sjPlot', 'gsheet')
ipak(packages)

theme_set(theme_bw())


sl <- read.csv('freqLearn2.csv')

# get hive
sl$hive <- sapply(1:nrow(sl), FUN = function(ii) {
     s1 <- strsplit(as.character(sl$BeeNumCol[ii]), split = "[hH]ive")[[1]][2]
     strsplit(s1, "_")[[1]][1]
})

# remove test rows
sl <- sl[sl$beeCol != 'TESTTESTPHOTO',]

table(sl$hive, useNA = 'always')

## make sure all bee colors are lowercase
sl$beeCol <- tolower(sl$beeCol)



# TODO: check why there are some values lower than 220 and higher than 450
sl <- sl[sl$freq > 220 & sl$freq < 450,]

# plot each bee's frequency over time by treatment
# ggplot(sl, aes(x = freq, y = amp)) + 
#      geom_point()


## Add IT Span to dataset
URL = 'https://docs.google.com/spreadsheets/d/1qbld3hmg-11BYmHhO6VXJ89-60xG8hY5nqmf2yKuXKQ/edit?usp=sharing'
mtadta <- gsheet2tbl(URL)

md2 <- gsheet2tbl('https://docs.google.com/spreadsheets/d/12f5Yca-MYAtvCw1WmViqPoil6LUsUsJhWK9W9tAyvp4/edit?usp=sharing')

md2 <- data.frame(md2[!is.na(md2$IT), c('BeeColorNum', 'IT')])

# get bee sizes
md <- data.frame(mtadta[!is.na(mtadta$IT), c('BeeColorNum', 'IT') ])

md <- rbind(md, md2)
table(md$BeeColorNum)
sum(table(md$BeeColorNum) != 1) # check to make sure I don't have repeated bees


# merge md into full dataset
md
head(sl)

sl_2 <- merge(md, sl, by.x = c("BeeColorNum"), by.y = ("beeCol"), all = TRUE)
head(sl_2)

sl <- sl_2


table(interaction(sl$BeeColorNum, sl$IT))[table(interaction(sl$BeeColorNum, sl$IT)) != 0]

# see how many rows are missing IT Span
nrow(sl[is.na(sl$IT), ]) # this is just a few bees
unique(sl[is.na(sl$IT), 'BeeColorNum']) # three bees


# Check on this:
plot(sl_2$IT ~sl_2$freq)
abline(lm(sl_2$IT ~ sl_2$freq))
sl[sl$IT < 3.1, ]

sl<- sl[!is.na(sl$BeeColorNum), ]

ggplot(sl, aes(x = index, y = freq, color = BeeColorNum)) + 
     geom_point(aes(shape = as.factor(trialNum))) + 
     facet_wrap(~BeeColorNum) + 
     theme(legend.position = "none")

library(lattice)
bwplot(freq ~  trt | factor(trialNum) , data = sl)

hist(sl$freq)
trt <- character()
     
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

sl$IT_C <- scale(sl$IT, center = TRUE, scale = FALSE)



# multilevel model
library(lmerTest)
step(m1.1)
sl$IT_C <- as.numeric(sl$IT_C)

m1 <- lmer(freq ~ trialNum + index + trt + IT_C + hive + (1 +  trialNum |BeeColorNum), data = sl)
summary(m1)
AIC(m1)

# multilevel model
m1.1 <- lmer(freq ~ (trialNum+index+trt+ IT_C + hive)^2 +   (1|BeeColorNum), data = sl[sl$trt != "unrewarded",])
summary(m1.1)
m11 <- step(m1.1)
m12 <- lmer(freq ~ trialNum + index + trt + IT_C + hive + 
                 (1 | BeeColorNum) + trialNum:index + trialNum:trt + trialNum:IT_C + 
                 trialNum:hive + index:trt + index:IT_C + index:hive + trt:IT_C + 
                 trt:hive, data = sl)
summary(m12)
m11
AIC(m11)

ggplot(sl[sl$trialNum == 1 & sl$trt != "unrewarded",] , aes(x = trt, y = freq)) + 
     geom_boxplot() + 
     facet_wrap(~ index %% 15)

p = ggplot(sl[sl$trt != "unrewarded",] , aes(x = trt, y = freq)) + 
     geom_boxplot(fill = "white") + 
     theme_classic() + 
     labs(x = "Reward Group", y = "Sonication Frequency (Hz)") + 
     scale_x_discrete(name ="Reward Group", 
                      labels=c("Full", "High", "Low")) + 
     theme(panel.border = element_blank(),
           legend.key = element_blank(),
           panel.grid = element_blank(),
           panel.grid.minor = element_blank(), 
           panel.grid.major = element_blank(),
           panel.background = element_blank(),
           plot.background = element_rect(fill = "transparent",colour = NA))
p
 
ggsave(p, filename = '~/Desktop/SonicationFreqLearning.pdf',  bg = "transparent", width = 5, height = 4)

# calculate % rewards
mean(sl$freq[sl$trt == "low"] < 330)

xtabs( ~ sl$trt + sl$lowFrq )
xtabs( ~ sl$trt + sl$highFrq )

gg = ggplot(sl , aes(x = trt, y = freq)) + 
     geom_boxplot(fill = "white") + 
     theme_classic() + 
     labs(x = "Reward Group", y = "Sonication Frequency (Hz)") + 
     scale_x_discrete(name ="Reward Group", 
                      labels=c("Full", "High", "Low", "Unrewarded")) + 
     theme(panel.border = element_blank(),
           legend.key = element_blank(),
           panel.grid = element_blank(),
           panel.grid.minor = element_blank(), 
           panel.grid.major = element_blank(),
           panel.background = element_blank(),
           plot.background = element_rect(fill = "transparent",colour = NA))
gg
ggsave(gg, filename = '~/Desktop/SonicationFreqLearning_unrewarded.pdf',  bg = "transparent", width = 5, height = 4)
# ggsave('~/Desktop/SonicationFreqLearning_unrewarded.pdf', width = 5, height = 4)


ggplot(sl , aes(x = trt, y = amp)) + 
     geom_boxplot() + 
     theme_classic() + 
     labs(x = "Reward Group", y = "Sonication Amplitude (V)")
ggsave('~/Desktop/SonicationAmp.pdf', width = 5, height = 4)

nrow(sl)
ll <- sl[sl$trialNum == 1,]
length(unique(sl$BeeColorNum))

summary(lmer(freq ~ trt + IT_C + hive + trialNum + index +  (1|BeeColorNum), data =sl[sl$trt != "unrewarded",] ))

# amplitude model
summary(lmer(log(amp) ~ trt + IT_C + hive + trialNum + index +  (1|BeeColorNum), data =sl ))

anova(m1, m1.1)

m1_lm <- lm(freq ~ trialNum*index*trt*IT_C, data = sl)
summary(m1_lm)

plot(m1_lm)

m2 <- update(m1, .~. -trialNum:index:trt )
anova(m1, m2) # keep 3-way interaction
sjp.lmer(m1)

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


# calculate bees' means by treatment
eDF <- list()
for(bee in unique(as.character(sl$BeeColorNum))){
     tmp <- sl[sl$BeeColorNum == bee, ]
     eDF[[bee]] <-  tapply(tmp$freq, INDEX = tmp$trt, mean)
}

beeTrtAvgs <- t(as.data.frame(eDF))
beetrtAvgs_long <- melt(beeTrtAvgs, varnames = c('bee', 'trt'))

ggplot(beetrtAvgs_long, aes(x = trt, y = value)) + 
     geom_boxplot() + 
     geom_point(aes(color = bee), alpha = 0.2) + 
     geom_line(stat="smooth",method = "lm", aes(group = bee, color = bee),
               size = 1,
               alpha = 0.3) +
      ylim(c(250, 450)) + 
     theme(legend.position = "none")


beetrtAvgs_long <- merge(beetrtAvgs_long, md, by.x = 'bee', by.y = 'BeeColorNum', all = TRUE)


mm1 <- lmer(value ~ trt * IT +  (1|bee), data = beetrtAvgs_long)
summary(mm1)

R colnames(eDF) <- c('bee', 'frqInitia', 'frqhigh', 'frqlow', 'frqUnrewarded')

# get each bee's treatments
beeTrts <- tapply(as.character(sl$trt), INDEX = sl$BeeColorNum,FUN = function(x) names(table(x))) 

# get bees' most common treatmens
beeTrts <- tapply(sl$trt, INDEX = sl$beeCol,FUN = function(x) names(table(x))[which.max(table(x))]) 

# see treatments for each bee
tapply(sl$trt, INDEX = sl$beeCol,FUN = function(x) table(x)) 

data.frame(beeTrts)


avgDF <- data.frame(beeMeans, beeTrts)
ggplot(avgDF, aes(x = beeTrts, y = beeMeans)) + 
     geom_boxplot() + 
     geom_point(position = position_jitter(), alpha = 0.2)


