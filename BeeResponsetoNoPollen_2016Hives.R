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

# change path
# /Users/callinswitzer/Dropbox/SonicationLearningManuscript/Data/SonicationLearningWAvery_NoImages/Beeorange_18Dec2016_Hive5_initial

npp$initialPath2 <- gsub("/Volumes/My Passport/BeeSonicationLearningWithAvery/BeeFrequencyLearning/", replacement = "/Users/callinswitzer/Dropbox/SonicationLearningManuscript/Data/SonicationLearningWAvery_NoImages/", x = npp$initialPath)

npp$noRewardPath2 <- gsub("/Volumes/My Passport/BeeSonicationLearningWithAvery/BeeFrequencyLearning/", replacement = "/Users/callinswitzer/Dropbox/SonicationLearningManuscript/Data/SonicationLearningWAvery_NoImages/", x = npp$noRewardPath)

bgDF <- data.frame()
for(ii in 1:nrow(npp)){
     tmp <- npp[ii, ]
     initialDF <- read.csv(tmp$initialPath2, header = FALSE)
     initialDF$bee <- tmp$BeeColorNum
     initialDF$trt <- "initial"
     
     noDF <- read.csv(tmp$noRewardPath2, header = FALSE)
     noDF$bee <- tmp$BeeColorNum
     noDF$trt <- "NoReward"
     
     combi <- rbind(initialDF, noDF)
     combi <- combi[combi[,1] > 220 & combi[,1] < 450, ]
     
     combi$buzzNum <- 1:nrow(combi)
     
     bgDF <- rbind(bgDF, combi)
     
}

head(bgDF)
names(bgDF) <- c("freq", "amp", "dateTime", "rewNum", "rewTF", "lowAmp", "highAmp", "bee", "trt", "buzzNum")

bgDF <- bgDF[bgDF$buzzNum < 100, ]



ggplot(bgDF, aes(x = buzzNum, y = freq, color = trt)) + 
     #geom_line(alpha = 0.3) + 
     geom_smooth(se = TRUE) 
     # geom_vline(xintercept = c(51, 56))
figureDir <- "/Users/callinswitzer/Dropbox/SonicationLearningManuscript/Figures/"

ggsave(file.path(figureDir, "PreliminaryStopRewardTrials.pdf"), width = 4, height = 3)


m1 <- lmer(freq ~ trt + (1|bee), data = bgDF)
summary(m1)
