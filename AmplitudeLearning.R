## Callin Switzer
## 5 July 2017
## Analysis of bee learning experiments to see if bees can learn to 
## buzz at specific amplitudes
# updated 5 July 2017

ipak <- function(pkg){
     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
     if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
     sapply(pkg, require, character.only = TRUE)
}

dataDir <- "/Users/callinswitzer/Dropbox/SonicationLearningManuscript/Data"

packages <- c("ggplot2", "lme4", "gsheet")
ipak(packages)

# bll_url <- "https://docs.google.com/spreadsheets/d/1P5TEaVaCen4YKWXwn2kL-gI3B_9JgNdCbY7ezvoT2C0/edit?usp=sharing"
# 
# 
# foo <- gsheet2tbl(bll_url)
# head(foo)
# rm(foo)

bll <- read.csv(file.path(dataDir, "BeeLearningLong_16Sept - BeeLearningLong.csv"))

head(bll)

bll$beeDate <- paste(bll$bee, bll$date, sep = "__")

theme_set(theme_classic())

ggplot(bll, aes(x = as.numeric(minSinceStart), y = freq, color = beeDate)) + 
     geom_point(alpha = 0.5) + 
     geom_smooth(size = 2, se = FALSE) + 
     facet_grid(testTrain~trainThresh,scales = "free",labeller = label_both) + 
     labs(x = "Elapsed time (min)")

ggplot(bll, aes(x = as.numeric(minSinceStart), y = amp, color = beeDate)) + 
     geom_point(alpha = 0.1) + 
     geom_line(stat = 'smooth', size = 2, alpha = 0.6, se = FALSE) + 
     facet_grid(testTrain~trainThresh,scales = "free",labeller = label_both) + 
     labs(x = "Elapsed time (min)")

#ggsave("~/Desktop/RealFlowerBeeLearning.pdf", width = 15, height = 8)


ggplot(bll, aes(x = freq, y = amp, color = beeDate)) + 
     geom_point(alpha = 0.5) + 
     geom_smooth(size = 2, se = FALSE) + 
     facet_grid(testTrain~trainThresh,scales = "free",labeller = label_both) + 
     labs(x = "freq")

library('lme4')
m1 <- lmer(amp ~ trainThresh + IT +  (1|beeDate), data = bll[bll$testTrain == 'train',])
summary(m1)




## look at first few buzzes of test set
for(ii in unique(bll$beeDate)){
     foo <-  data.frame(bll[bll$beeDate == ii & bll$testTrain == 'test', ][1:30, ])
     
     # calculate mean amplitude for first N buzzes
     meaAmp <- median(foo$amp)
     foo$amp <- meaAmp
     foo <- foo[1, ]
     
     if(ii == unique(bll$beeDate[1])) ff <- foo
     else ff <- rbind(ff, foo)
}


ggplot(ff, aes(x = trainThresh, y = amp)) + 
     geom_boxplot() + 
     geom_point(aes(color = beeDate))


### look at initial training vs. initial testing for each bee

t.test(ff$amp ~ ff$trainThresh)

bar <- cbind.data.frame(foo)


############################ increasing amplitude ####################################
#without wingbeats
bllI_url <- "https://docs.google.com/spreadsheets/d/18HzCLZ8lbhDIsTJlpSPLMc3t0eE4OBvugfC_4vtvkWU/edit?usp=sharing"

bllI <- gsheet2tbl(bllI_url)

# with wingbeats
bmd_url_wing <- "https://docs.google.com/spreadsheets/d/1BBFtRD46-V8fwIph0vvnf8zd1RA6VDrL8liLOwyBI54/edit?usp=sharing"

bllI <- gsheet2tbl(bmd_url)


head(bllI)

bllI$beeDate <- paste(bllI$bee, bllI$date, sep = "__")

theme_set(theme_classic())


ggplot(bllI, aes(x = as.numeric(minSinceStart), y = freq, color = beeDate)) + 
     geom_point(alpha = 0.5) + 
     geom_smooth(size = 2, se = FALSE) +
     labs(x = "Elapsed time (min)")

ggplot(bllI, aes(x = as.numeric(minSinceStart), y = amp, color = beeDate)) + 
     geom_point(alpha = 0.2) + 
     geom_smooth(size = 1, se = FALSE, lty = 2) + 
     labs(x = "Elapsed time (min)") + 
     geom_step(aes(x = as.numeric(minSinceStart), y = rewardAmp, color = beeDate) ) + 
     facet_wrap(~beeDate, nrow = 2)
ggsave(filename = "~/Desktop/IncreasingAmpReward.pdf", width = 12 , heigh = 7)

ggplot(bllI, aes(x = freq, y = amp, color = beeDate)) + 
     geom_point(alpha = 0.5) + 
     geom_smooth(size = 2, se = FALSE) + 
     facet_wrap(~beeDate, nrow = 2)
     labs(x = "freq")
     
ggplot(bllI, aes(x = freq,  fill = beeDate)) + 
     geom_histogram() + 
     #geom_point(alpha = 0.5) + 
     #geom_smooth(size = 2, se = FALSE) + 
     facet_wrap(~beeDate, nrow = 2)


################ VISUALIZE FAKE FLOWERS ###############

ff_URL <- "https://docs.google.com/spreadsheets/d/1qt-T27facm8DJ9-75-j_-8OSU-OXasjrDQv9cyjNfCs/edit?usp=sharing"

bll <- gsheet2tbl(ff_URL)

head(bll)

bll$beeDate <- paste(bll$bee, bll$date, sep = "__")

theme_set(theme_classic())

ggplot(bll, aes(x = as.numeric(minSinceStart), y = freq, color = beeDate)) + 
     geom_point(alpha = 0.5) + 
     geom_smooth(size = 2, se = FALSE) + 
     facet_grid(testTrain~trainThresh,scales = "free",labeller = label_both) + 
     labs(x = "Elapsed time (min)")

ggplot(bll, aes(x = as.numeric(minSinceStart), y = amp, color = beeDate)) + 
     geom_point(alpha = 0.3) + 
     geom_smooth(size = 2, se = FALSE) + 
     facet_grid(testTrain~trainThresh,scales = "free",labeller = label_both) + 
     labs(x = "Elapsed time (min)")

#ggsave(filename = "~/Desktop/FakeFlowerLearning_18Sept.pdf", width = 15, height = 8)


ggplot(bll, aes(x = freq, y = amp, color = beeDate)) + 
     geom_point(alpha = 0.5) + 
     geom_smooth(size = 2, se = FALSE) + 
     facet_grid(testTrain~trainThresh,scales = "free",labeller = label_both) + 
     labs(x = "freq")

ggplot(bll, aes(x = freq, y = amp, color = testTrain)) + 
     geom_point(alpha = 0.5) + 
     geom_smooth(size = 2, se = FALSE) + 
     facet_grid(testTrain~trainThresh,scales = "free",labeller = label_both) + 
     labs(x = "freq")



ggplot(bll, aes(x = as.numeric(minSinceStart), y = amp, color = testTrain)) + 
     geom_point(alpha = 0.3) + 
     geom_smooth(size = 2, se = FALSE) + 
     facet_grid(testTrain~trainThresh,scales = "free",labeller = label_both) + 
     labs(x = "Elapsed time (min)")


## look at first few buzzes of test set
for(ii in unique(bll$beeDate)){
     foo <-  data.frame(bll[bll$beeDate == ii & bll$testTrain == 'train', ][70:80, ])
     
     # calculate mean amplitude for first N buzzes
     meaAmp <- median(foo$amp)
     foo$amp <- meaAmp
     foo <- foo[1, ]
     
     if(ii == unique(bll$beeDate[1])) ff <- foo
     else ff <- rbind(ff, foo)
}

ggplot(ff, aes(x = trainThresh, y = amp)) + 
     geom_boxplot() + 
     geom_point(aes(color = beeDate))

t.test(ff$amp ~ ff$trainThresh)


### look for selection bias in IT span

library(gsheet)

url <- "https://docs.google.com/spreadsheets/d/1OrVttHVfYL3CQn1OrjC33RXqtOYkZzrVFMtaomySYGE/edit?usp=sharing"

beeM <- gsheet2tbl(url)
beeM <- beeM[beeM$FlowerType == "extract",]
beeM <- beeM[!is.na(beeM$BeeColor),]

beeM$discard <- !is.na(as.numeric(sapply(X = 1:nrow(beeM), function(x) grep('discard', paste(beeM[x,], collapse = " ")))))

beeM <- beeM[beeM$TrainThresh %in% c("0", "0.25", "0.5", "0.75"),]

colnames(beeM)
ggplot(beeM, aes(x = TrainThresh, y = IT, fill = discard)) + 
     geom_boxplot() + 
     facet_wrap(~discard) + 
     labs(x = "Training Amplitude Threshold", y = "IT span (mm)")
ggsave("~/Desktop/SelectionBias.pdf", width = 5, height = 4)

