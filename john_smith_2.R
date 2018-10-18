rm(list = ls())
setwd("/Users/muyaoli/Downloads")
install.packages("text2vec")
install.packages("openNLPmodels.en")
library(NLP)
install.packages("openNLP")
install.packages("tseries")
library("tm")
library("text2vec")
require("tseries")
###load data and clean data###
fb <- data.frame(text = NA) 
fbid <- data.frame(id = NA)
for (i in 1:5) {
  for (j in 1:12) {
    fileyear <- paste("201",toString(i),sep ="")
    filemonth <- j
    file <- paste("fpost-",fileyear,"-",filemonth,".csv", sep = "") ### add new column year and month with the filename###
    df <- readLines(con = file)
    df <-as.data.frame(df)
    colnames(df) <- "text"
    fb <- rbind(fb,df)
    fbid <- rbind(fbid, data.frame(id = rep((paste(fileyear,filemonth,sep ="")),nrow(df))))
  }
}

fb_all <- data.frame(fbid,fb)
fb_all <- fb_all[-1,]
###Topic Modling###
###use the ingredients as the dictionary to find the popular topics###
ingredient<-readLines("ingredients.txt")
dic<- tolower(ingredient)
topword <- c()
for (i in 1:5) {
  for (j in 1:12) {
    fileyear <- paste("201",toString(i),sep ="")
    filemonth <- j
    dfname <- paste(fileyear,filemonth, sep = "")
    fb_sub <- fb_all[fb_all$id == dfname,]
    colnames(fb_sub) <- c("doc_id","text")
    docs <- Corpus(DataframeSource((fb_sub)))
    dtm <- DocumentTermMatrix(docs, control=list(dictionary = dic))
    dtm <- removeSparseTerms(dtm, 0.99)
    topword <- c(topword, findFreqTerms(dtm, lowfreq=300)[1:5])
  }
}
topword
unique(topword)

####count frequency of cake and plot time-seris ###
keyword <- 'cake'
df_keyword = matrix(0, 60, 1)

for(i in 1:5){
  for (j in 1:12){
    filename = paste("fpost-201", i, "-", j, ".csv", sep='')
    csvdata <- read.csv(filename, header=FALSE, sep=',', quote='"')
    csvdata <- paste(csvdata$V1, csvdata$V2, csvdata$V3)
    value <- sum(grepl(keyword, csvdata, ignore.case = T))
    row_num <- 12*(i-1) + j
    df_keyword[row_num, 1] <- value
  }
}

keywaord_freq <- ts(df_keyword, start = c(2011, 1), frequency = 12)
plot(keywaord_freq, yax.flip = TRUE)
title(paste("Time Series for cake"))

####count frequency of pumpkin and plot time-seris ###

keyword <- 'pumpkin'
df_keyword = matrix(0, 60, 1)

for(i in 1:5){
  for (j in 1:12){
    filename = paste("fpost-201", i, "-", j, ".csv", sep='')
    csvdata <- read.csv(filename, header=FALSE, sep=',', quote='"')
    csvdata <- paste(csvdata$V1, csvdata$V2, csvdata$V3)
    value <- sum(grepl(keyword, csvdata, ignore.case = T))
    row_num <- 12*(i-1) + j
    df_keyword[row_num, 1] <- value
  }
}

keywaord_freq <- ts(df_keyword, start = c(2011, 1), frequency = 12)
plot(keywaord_freq, yax.flip = TRUE)
title(paste("Time Series for pumpkin"))

####count frequency of vegetable noodle and plot time-seris ###
keyword <- 'vegetable noodle'
df_keyword = matrix(0, 60, 1)

for(i in 1:5){
  for (j in 1:12){
    filename = paste("fpost-201", i, "-", j, ".csv", sep='')
    csvdata <- read.csv(filename, header=FALSE, sep=',', quote='"')
    csvdata <- paste(csvdata$V1, csvdata$V2, csvdata$V3)
    value <- sum(grepl(keyword, csvdata, ignore.case = T))
    row_num <- 12*(i-1) + j
    df_keyword[row_num, 1] <- value
  }
}

keywaord_freq <- ts(df_keyword, start = c(2011, 1), frequency = 12)
plot(keywaord_freq, yax.flip = TRUE)
title(paste("Time Series for vegetable noodle"))

####count frequency of cauliflower rice and plot time-seris ###
keyword <- 'cauliflower rice'
df_keyword = matrix(0, 60, 1)

for(i in 1:5){
  for (j in 1:12){
    filename = paste("fpost-201", i, "-", j, ".csv", sep='')
    csvdata <- read.csv(filename, header=FALSE, sep=',', quote='"')
    csvdata <- paste(csvdata$V1, csvdata$V2, csvdata$V3)
    value <- sum(grepl(keyword, csvdata, ignore.case = T))
    row_num <- 12*(i-1) + j
    df_keyword[row_num, 1] <- value
  }
}

keywaord_freq <- ts(df_keyword, start = c(2011, 1), frequency = 12)
plot(keywaord_freq, yax.flip = TRUE)
title(paste("Time Series for cauliflower rice"))  