# first look at bee sonication learning frequency data
# 15 Dec 22016
# Callin Switzer



#install packages
ipak <- function(pkg){
     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
     if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
     sapply(pkg, require, character.only = TRUE)
}

packages <- c("gsheet", "ggplot2", "reshape2", "pwr")
ipak(packages)


# viewable URL (make sure it cannot be edited)
URL = "https://docs.google.com/spreadsheets/d/1qbld3hmg-11BYmHhO6VXJ89-60xG8hY5nqmf2yKuXKQ/edit?usp=sharing"

bdta <- gsheet2tbl(URL)
bdta <- bdta[!is.na(bdta$trainStartTime),]


trt <- character()
for(ii in 1:nrow(bdta)){
     if(bdta$rewardFreq1[ii] == 220 & bdta$rewardFreq2[ii] == 450){
          trt[ii] <- "control"
     }
     else if(bdta$rewardFreq1[ii] > 300 & bdta$rewardFreq2[ii] <= 450){
          trt[ii] <- "high"
     }
     else{
          trt[ii] <- 'low'
     } 
}

bdta$trt <- trt


# get info for last trial
lastTrial <- t(sapply(unique(bdta$BeeColorNum), function(ii){
     tmp <- bdta[bdta$BeeColorNum == ii, ]
     return(c(tmp$rewardFreq1[nrow(tmp)], tmp$rewardFreq2[nrow(tmp)], tmp$averageFreq[nrow(tmp)]))
     
}))

lt <- as.data.frame(lastTrial)
lt$beeCol <- row.names(lt)
colnames(lt) <- c("f1", "f2", "lastF")
lt$trt <- paste(lt$f1, lt$f2, sep = "_")
lt <- lt[lt$trt %in% names(table(lt$trt))[table(lt$trt) > 5], ]


#weird
ggplot(lt, aes(x = trt, y = lastF)) + 
     geom_boxplot()

ggplot(bdta, aes(x = trt, y = averageFreq)) + 
     geom_boxplot() + 
     geom_point() 
     

summary(aov(bdta$averageFreq~bdta$trt))

length(unique(bdta$BeeColorNum))

library(plyr)
bdta$trt <- mapvalues(bdta$BeeColorNum, from = lt[,4],  to = lt[,5])
bdta$trt[!(bdta$trt %in% names(table(lt$trt))[table(lt$trt) > 5])] <- NA

ggplot(bdta, aes(x = BeeTrialNum, y = averageFreq)) + 
     geom_point(aes(color = BeeColorNum)) + 
     geom_smooth( se = FALSE) + 
     theme(legend.position = "none") + 
     facet_wrap(~trt)

