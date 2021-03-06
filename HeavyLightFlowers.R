# Heavy vs. light flowers
# Callin Switzer
# 12 October 2016
# 
# Compares the bees' frequency when they are buzzing on 
# heavy (metal added) vs. light flowers
# Note: all pores were glued shut


# install packages
ipak <- function(pkg){
     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
     if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
     sapply(pkg, require, character.only = TRUE)
}

packages <- c("gsheet", "ggplot2", "reshape2", "pwr")
ipak(packages)



# file called: BeeMetadata_RealFlowersGlued
URL <- "https://docs.google.com/spreadsheets/d/1FiSA1-4wGoWqBXa7NY-Z125xjCDANEG5m5oDo8znF9M/edit?usp=sharing"

# read in data
hlf <- gsheet2tbl(URL)

# subset to good trials
rmRows <- grep(pattern = "[dD]iscard", tolower(apply(hlf, MARGIN = 1, paste, collapse = ' ')))
hlf <- hlf[-c(rmRows), ]
hlf <- hlf[hlf$FlowerType1 %in% c("sham", "weighted"), ]

hlf$trainingFile <- gsub(pattern = 'BeeSonicationLearningWithAvery', replacement = "", x =  hlf$trainingFile)


# read in each bee's training file and make into a long data frame

# change path anem
hlf$tfile2 <- gsub("/Users/callinswitzer/Dropbox/ExperSummer2016//HeavyLightFlowers/", replacement = '/Users/callinswitzer/Dropbox/SonicationLearningManuscript/HeavyLightFlowers/' , hlf$trainingFile)

op <- options(digits.secs = 3) # set seconds digits to 3
new_df <- data.frame()
for(ii in 1:nrow(hlf)){
     flnm <- as.character(hlf$tfile2 )[ii]
     tmp_file <- read.csv(flnm, header = FALSE)
     tmp_file$beeID <- hlf$BeeColorNum[ii]
     tmp_file$trt <- sapply(X = 1:nrow(tmp_file), FUN = function(x){
          if(tmp_file$V4[x] < hlf$switchRewardNum[ii]) return(hlf$FlowerType1[ii])
          else return(hlf$FlowerType2[ii])
     })
     tmp_file$hive <- hlf$Hive[ii]
     # convert to date time
     tmp_file[,"V3"] <- paste(substr(tmp_file[,"V3"], 1, 21), ".", 
                              substr(tmp_file[,"V3"], 23, 25), sep = "")
     dt <- as.POSIXlt(strptime(as.character(tmp_file[,"V3"]), 
                               format = " %Y_%m_%d__%H_%M_%OS", tz = "EST"))
     # get minutes since start
     tmp_file$minSinceStart <- as.numeric(difftime(dt, dt[1], units = 'mins'))
     
     new_df <- rbind(new_df, tmp_file)
}
ii


# set colnames
colnames(new_df)[1:7] <- c("freq", "amp", "dateTime", "rewardNum", "rewardTF", "lowAmpThresh", "highAmpThresh")
head(new_df)

# remove non-buzzes
new_df <- new_df[new_df$freq > 220, ]

# Calculate average for each bee and for each treatment
frqMeans <- as.data.frame(tapply(X = new_df$freq, INDEX = list(new_df$beeID, new_df$trt), mean))

frqMeans$beeID <- row.names(frqMeans)
frqMeans


frqLong <- melt(frqMeans, id.vars = "beeID", measure.vars =  c("sham", "weighted"), 
     variable.name = "trt", value.name = "frq")

theme_set(theme_classic())
ggplot(frqLong, aes(x = trt, y = frq)) + 
     geom_boxplot() + 
     geom_point()

figureDir = "/Users/callinswitzer/Dropbox/SonicationLearningManuscript/Figures/"
ggsave(file.path(figureDir, "heavyLight_frq.pdf"), width = 4, height = 3)


ggplot(frqLong, aes(x = trt, y = frq)) + 
     geom_boxplot() + 
     labs(y = "Buzz Frequency (Hz)", x = "Flower treatment")
# ggsave('~/Desktop/heavyLightFreq.pdf', width = 5, heigh = 4)

# look at distribution of buzzes for each bee
ggplot(new_df, aes(x = freq, fill = trt)) + 
     geom_histogram() + 
     facet_wrap(~beeID + trt)

ggplot(new_df, aes(y = freq, x = trt, fill = trt)) + 
     geom_violin() + 
     geom_point(position= position_jitter(width = 0.2, height = 0))+
     facet_wrap(~beeID)

nrow(new_df)
head(new_df)
unique(new_df$hive)



# Calculate average amplitude for each bee and for each treatment
ampMeans <- as.data.frame(tapply(X = new_df$amp, INDEX = list(new_df$beeID, new_df$trt), mean))

ampMeans$beeID <- row.names(ampMeans)
ampMeans


ampLong <- melt(ampMeans, id.vars = "beeID", measure.vars =  c("sham", "weighted"), 
                variable.name = "trt", value.name = "amp")

theme_set(theme_classic())
ggplot(ampLong, aes(x = trt, y = amp)) + 
     geom_boxplot() + 
     geom_point()


ggplot(ampLong, aes(x = trt, y = amp)) + 
     geom_boxplot() + 
     geom_point() + 
     labs(y = "Buzz Amplitude (V)", x = "Flower treatment")
ggsave(file.path(figureDir, 'heavyLightAmp.pdf'), width = 5, heigh = 4)

nrow(ampLong)

# look at distribution of buzzes for each bee
ggplot(new_df, aes(x = amp, fill = trt)) + 
     geom_histogram() + 
     facet_wrap(~beeID + trt)



# mean amplitude vs. frequency
ampFrq <- cbind(ampLong, frqLong)
ggplot(ampFrq, aes(y = amp, x = frq, color = trt)) + 
     geom_point() + 
     geom_smooth()



# t.test(frqLong$frq ~ frqLong$trt, paired = TRUE) # pretty close, but need more data
# 
# # estimate sample size
# colMeans(frqMeans[,1:2])
# # estimate effect size
# dd <- abs(diff(colMeans(frqMeans[,1:2]))) / sd(c(frqMeans[,1], frqMeans[,2]))
# # calculate sample size, assuming a simple t-test
# pwr.t.test(d = dd, sig.level = 0.05, type = "paired", power = 0.8)
# # 49 bees....thats a lot.  Doing repeated measurements will probably require fewer bees.

