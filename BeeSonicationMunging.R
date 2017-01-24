# Callin Switzer
# 23 Jan 2017
# Merging all the data from sonication experiments into a massive dataset

#install packages
ipak <- function(pkg){
     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
     if(length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
     sapply(pkg, require, character.only = TRUE)
}

packages <- c("gsheet", "ggplot2", "reshape2", "pwr", 'lme4')
ipak(packages)