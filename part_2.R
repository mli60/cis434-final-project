# Get a List of all files in directory named with a key word, say all `.csv` files
library(NLP)
library(tm)
library(gtools)
require(lubridate)
library(topicmodels)
library(RColorBrewer)
library(wordcloud)
library(data.table)

#setwd("/Users/muyaoli/Downloads")
file_list <- list.files("fbpost", pattern="*.csv", full.names=TRUE)

ldf <- lapply(mixedsort(file_list, decreasing=TRUE), function (filename) {
  # print(filename)
  read.table(filename, quote="", fill=TRUE, blank.lines.skip=TRUE)
})

names(ldf) <- c("2011-01","2011-02","2011-03","2011-04","2011-05","2011-06","2011-07","2011-08","2011-09","2011-10","2011-11","2011-12",
                "2012-01","2012-02","2012-03","2012-04","2012-05","2012-06","2012-07","2012-08","2012-09","2012-10","2012-11","2012-12",
                "2013-01","2013-02","2013-03","2013-04","2013-05","2013-06","2013-07","2013-08","2013-09","2013-10","2013-11","2013-12",
                "2014-01","2014-02","2014-03","2014-04","2014-05","2014-06","2014-07","2014-08","2014-09","2014-10","2014-11","2014-12",
                "2015-01","2015-02","2015-03","2015-04","2015-05","2015-06","2015-07","2015-08","2015-09","2015-10","2015-11","2015-12") 
#df.final <- do.call("smartbind", ldf)
df.final <- rbindlist(ldf, idcol = 'id',fill = TRUE)[, `:=` (YEAR = substr(id,1,4),Month=substr(id,6,7))]
df.final<-as.data.frame(df.final)
df.final$id <- NULL
colname<-colnames(df.final)
colname <-colname[1:185]
#YEAR<-df.final[145]
#Month<-df.final[146]
#df.final$YEAR<-NULL
#df.final$Month<-NULL
df.final$post <- do.call(paste, c(df.final[colname], sep=" "))
for (co in colname) df.final[co] <- NULL
df.final$post<- gsub("NA","",df.final$post)





write.csv(df.final,"df_final.csv")
########################
temp<-df.final[,c("Month","post")]
temp$id<- 1:nrow(temp)
temp<-subset(temp,Month == "01")
temp <- temp[,c("id","post")]
names(temp) = c("doc_id", "text")
docs <- Corpus(DataframeSource(temp))
mystop=c("&amp","amp", "get", "got", "just", "can", "now", "will", "dont", "like","http","www")
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=F, stopwords=c(stopwords("english"),stopwords("spanish"), stopwords("portuguese"),mystop))
dtm = DocumentTermMatrix(docs, control=dtm.control)
dtm = removeSparseTerms(dtm,0.999)
idx <- rowSums(as.matrix(dtm))>0
newdocs <- docs[idx]
dtm = dtm[idx,]
freq = colSums( as.matrix(dtm) )
wordcloud(names(freq), freq, max.words=50, colors=brewer.pal(6,"Dark2"))

# LDA
lda.model = LDA(dtm, 12)
myposterior <- posterior(lda.model)
topics = myposterior$topics
terms = myposterior$terms

# Plot word cloud for a specific topic
tid <- 10
freq <- terms[tid, ] 
freq["turkey"]
wordcloud(names(freq), freq, max.words=30, colors=brewer.pal(6,"Dark2"))

# Term frequency
freq = colSums( as.matrix(dtm) )
freq.sorted = sort( freq, decreasing=TRUE )
freq.sorted[1:10]
freq["rice"]
wordcloud(names(freq), freq, max.words=50, colors=brewer.pal(6,"Dark2"))

# LDA
lda.model = LDA(dtm, 12)
myposterior <- posterior(lda.model)
topics = myposterior$topics
terms = myposterior$terms

# Plot word cloud for a specific topic
tid <- 10
freq <- terms[tid, ] 
wordcloud(names(freq), freq, max.words=30, colors=brewer.pal(6,"Dark2"))
