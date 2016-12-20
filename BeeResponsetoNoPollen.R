# Visualize Bee Response to no Pollen
# Callin Switzer
# 19 December 2016

#install packages
ipak <- function(pkg){
     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
     if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
     sapply(pkg, require, character.only = TRUE)
}

packages <- c("gsheet", "ggplot2", "reshape2", "lme4")
ipak(packages)


URL <- "https://docs.google.com/spreadsheets/d/12f5Yca-MYAtvCw1WmViqPoil6LUsUsJhWK9W9tAyvp4/edit?usp=sharing"

npp <- gsheet2tbl(URL)



theme_set(theme_classic())

colnames(npp)

np_sm <- npp[, c("BeeColorNum", "averageFreq_unreward", "averageFreq_reward")]
np_long <- melt(np_sm, id.vars = "BeeColorNum", measure.vars = c("averageFreq_unreward", "averageFreq_reward"))


ggplot(np_long, aes(x = variable, y = value)) + 
     geom_boxplot() + 
     geom_point(aes(color = BeeColorNum))

# significant, but not a big enough sample size
t.test(np_sm$averageFreq_unreward, np_sm$averageFreq_reward, paired = TRUE)

# nonsignificant
kruskal.test(np_sm$averageFreq_unreward, np_sm$averageFreq_reward, paired = TRUE)


## load in all buzzes for each bee, and use LMER
# read in buzzes for each bee

bgDF <- data.frame()
for(ii in 1:nrow(npp)){
     tmp <- npp[ii, ]
     initialDF <- read.csv(tmp$initialPath, header = FALSE)
     initialDF$bee <- tmp$BeeColorNum
     initialDF$trt <- "initial"
     
     noDF <- read.csv(tmp$noRewardPath, header = FALSE)
     noDF$bee <- tmp$BeeColorNum
     noDF$trt <- "NoReward"
     
     combi <- rbind(initialDF, noDF)
     combi <- combi[combi[,1] > 220 & combi[,1] < 450, ]
     
     combi$buzzNum <- 1:nrow(combi)
     
     bgDF <- rbind(bgDF, combi)
     
}

head(bgDF)
names(bgDF) <- c("freq", "amp", "dateTime", "rewNum", "rewTF", "lowAmp", "highAmp", "bee", "trt", "buzzNum")



ggplot(bgDF, aes(x = buzzNum, y = freq, color = bee)) + 
     geom_line(alpha = 0.3) + 
     geom_smooth(se = FALSE) + 
     geom_vline(xintercept = 50)
