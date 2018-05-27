require(lubridate)
library(syuzhet)
library(ggplot2)
require(scales)
library(reshape2)
require(dplyr)
flights = read.csv("social_media_final.csv", header=T)
tweet <- iconv(flights$tweet,to ='utf-8-mac')
s <- get_nrc_sentiment(tweet)
head(s)
# Bar plot
barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'Count',
        main = 'Sentiment Scores for Apple Tweets')

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

head(s)
s_len <- length(tweet)
flights$sentiment <- NA
#sent_list = c()
for (i in 1:s_len) {
  flights$sentiment[i] <-sentiment_checker(s[i,])
}

#myfunction <- function(arg1, arg2, ... ){
 # statements
#  return(object)
#}