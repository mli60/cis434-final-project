library(lubridate)
library(syuzhet)
library(ggplot2)
library(scales)
library(reshape2)
require(dplyr)
library(foreign)
require(plyr)
require(tm)
require(RColorBrewer)
require(wordcloud)

### Sentiment Classification ###

# Read files and set encoding to work on mac
flights = read.csv("social_media_final.csv", header=T)
tweet <- iconv(flights$tweet,to ='utf-8-mac')
s <- get_nrc_sentiment(tweet)
head(s)

# argument is a row, for example s[1,]
# return 1 if the sentence is postive -1 if negative, 0 if neutral
sentiment_checker <- function(sentence) {
  neg <- sentence['negative']
  pos <- sentence['positive']
  if (neg < pos) {
    return(1)
  } else if (neg == pos) {
    return(0)
  } else {
    -1
  }
}

# Core work for Part 1
s_len <- length(tweet)
flights$sentiment <- NA
#sent_list = c()
for (i in 1:s_len) {
  flights$sentiment[i] <-sentiment_checker(s[i,])
}

affin<-get_sentiment(tweet, method = "afinn")
flights$affin <- affin
bing<-get_sentiment(tweet1, method = "bing")
nrc<-get_sentiment(tweet1, method = "nrc")
syuzhet<-get_sentiment(tweet1, method = "syuzhet")
flights$bing <- bing
flights$nrc <- nrc
flights$syuzhet <- syuzhet
non_complaint<-subset(flights,affin>0 & bing>0 & nrc >0 & syuzhet >0)


