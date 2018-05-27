# Get a List of all files in directory named with a key word, say all `.csv` files
file_list <- list.files("fb2011(1)", pattern="*.csv", full.names=TRUE)
#ldf <- lapply(file_list , read.csv, quote="")
ldf <- lapply(mixedsort(file_list, decreasing=TRUE), function (filename) {
  # print(filename)
  read.table(filename, quote="", fill=TRUE)
})
df.final <- do.call("smartbind", ldf)

colname<-colnames(df.final)
df.final$post <- do.call(paste, c(df.final[colname], sep=" "))
df.final$post<- gsub("NA","",df.final$post)
df.final$Month<-NA
df_2011<-df.final[,144:145]
#for (co in cols) data[co] <- NULL