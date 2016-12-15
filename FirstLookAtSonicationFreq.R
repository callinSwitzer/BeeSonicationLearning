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


plot(bdta$averageFreq ~ bdta$trt)

ggplot(bdta, aes(x = trt, y = averageFreq)) + 
     geom_boxplot()
