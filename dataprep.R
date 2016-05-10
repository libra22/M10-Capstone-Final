setwd("C:/Users/BPM/Downloads/DataScience/Assignment/M10-Capstone")


library(RWeka)
library(ggplot2)
library(doParallel)

library(stringi)
library(tm)
library(RCurl)

jobcluster <- makeCluster(detectCores())
invisible(clusterEvalQ(jobcluster, library(tm)))
invisible(clusterEvalQ(jobcluster, library(stringi)))
invisible(clusterEvalQ(jobcluster, library(wordcloud)))
invisible(clusterEvalQ(jobcluster, library(RWeka)))


#reading files
datatwitter <-readLines(conn <-file("./data/en_US.twitter.txt",encoding="UTF-8"))
close(conn)
datablog <- readLines(conn <-file("./data/en_US.blogs.txt",encoding="UTF-8"))
close(conn)
datanews <- readLines(conn <-file("./data/en_US.news.txt","rb",encoding="UTF-8"))
close(conn)

#combine all datasets into one
alldata <- c(datatwitter,datablog,datanews)

#get files size
twtfilesize <- format(file.info("./data/en_US.twitter.txt")$size/1024/1000,digits=6)
blogfilesize <- format(file.info("./data/en_US.blogs.txt")$size/1024/1000,digits=6)
newsfilesize <- format(file.info("./data/en_US.news.txt")$size/1024/1000,digits=6)

#get object size
twtobjsize <- format(object.size(datatwitter),units="auto")
blogobjsize <- format(object.size(datablog),units="auto")
newsobjsize <- format(object.size(datanews),units="auto")
allobjsize <- format(object.size(alldata),units="auto")
  
#get num of lines
twtlines <- prettyNum(length(datatwitter),big.mark = ",",width=7)
bloglines <- prettyNum(length(datablog),big.mark = ",",width=8)
newslines <- prettyNum(length(datanews),big.mark = ",",width=7)
alllines <- prettyNum(length(alldata),big.mark = ",",width=7)

#get num of words
twtwordcount <- stri_count_words(datatwitter)
blogwordcount <- stri_count_words(datablog)
newswordcount <- stri_count_words(datanews)
allwordcount <- stri_count_words(alldata)

#combine into table
twtinfo <- c(twtfilesize,twtobjsize,twtlines,prettyNum(sum(twtwordcount),big.mark = ",",width=8),prettyNum(mean(twtwordcount),big.mark = ",",width=4))
bloginfo <- c(blogfilesize,blogobjsize,bloglines,prettyNum(sum(blogwordcount),big.mark = ",",width=8),prettyNum(mean(blogwordcount),big.mark = ",",width=4))
newsinfo <- c(newsfilesize,newsobjsize,newslines,prettyNum(sum(newswordcount),big.mark = ",",width=8),prettyNum(mean(newswordcount),big.mark = ",",width=4))
allinfo <- c("NA",allobjsize,alllines,prettyNum(sum(allwordcount),big.mark = ",",width=8),prettyNum(mean(allwordcount),big.mark = ",",width=4))
datainfo <- rbind(twtinfo, bloginfo,newsinfo,allinfo)

#add row and column names
rownames(datainfo) <- c("Twitter", "Blogs", "News","Combined")
colnames(datainfo) <- c("File Size (MB)", "Object Size", "Lines","Number of Words", "Mean Number of Words")

#display dataset info
datainfo

#clean memory 1
rm(conn,allobjsize,alllines,allinfo,alldata,twtfilesize,blogfilesize,newsfilesize, twtobjsize,blogobjsize,newsobjsize,twtlines,bloglines,newslines,twtinfo,bloginfo,newsinfo,datainfo)

#save 1
save(allwordcount,file="./reportdata/allwordcount.RData")
save(blogwordcount,file="./reportdata/blogwordcount.RData")
save(newswordcount,file="./reportdata/newswordcount.RData")
save(twtwordcount,file="./reportdata/twtwordcount.RData")
save(datablog,file="./reportdata/datablog.RData")
save(datanews,file="./reportdata/datanews.RData")
save(datatwitter,file="./reportdata/datatwitter.RData")

load("./reportdata/datablog.RData")
load("./reportdata/datanews.RData")
load("./reportdata/datatwitter.RData")


#set seed for reproducibility
set.seed(1234)

#create sample 1%
sampletwt <- sample(datatwitter,round(0.01*length(datatwitter)))
sampleblog <- sample(datablog,round(0.01*length(datablog)))
samplenews <- sample(datanews,round(0.01*length(datanews)))

#sampletwt <- sample(datatwitter,round(0.005*length(datatwitter)))
#sampleblog <- sample(datablog,round(0.005*length(datablog)))
#samplenews <- sample(datanews,round(0.005*length(datanews)))

allsample <- c(sampletwt,sampleblog,samplenews)

#clean to ensure proper encoding. removes all the weird, unintelligible word or characters.
#sampletwt <- sapply(sampletwt,function(x) iconv (enc2utf8(x),sub="byte"))
#sampleblog <- sapply(sampleblog,function(x) iconv (enc2utf8(x),sub="byte"))
#samplenews <- sapply(samplenews,function(x) iconv (enc2utf8(x),sub="byte"))
#allsample <- sapply(allsample,function(x) iconv (enc2utf8(x),sub="byte"))

#convert to ASCII
sampletwt <- iconv(sampletwt, 'UTF-8', 'ASCII', "byte")
sampleblog <- iconv(sampleblog, 'UTF-8', 'ASCII', "byte")
samplenews <- iconv(samplenews, 'UTF-8', 'ASCII', "byte")
allsample <- iconv(allsample, 'UTF-8', 'ASCII', "byte")


#get sample object size
sampletwtobjsize <- format(object.size(sampletwt),units="auto")
sampleblogobjsize <- format(object.size(sampleblog),units="auto")
samplenewsobjsize <- format(object.size(samplenews),units="auto")
sampleallobjsize <- format(object.size(allsample),units="auto")

#get sample num of lines
sampletwtlines <- prettyNum(length(sampletwt),big.mark = ",",width=6)
samplebloglines <- prettyNum(length(sampleblog),big.mark = ",",width=6)
samplenewslines <- prettyNum(length(samplenews),big.mark = ",",width=6)
samplealllines <- prettyNum(length(allsample),big.mark = ",",width=6)

#get sample num of words
sampletwtwordcount <- stri_count_words(sampletwt)
sampleblogwordcount <- stri_count_words(sampleblog)
samplenewswordcount <- stri_count_words(samplenews)
sampleallwordcount <- stri_count_words(allsample)

#get unique number of words
sampletwtuniquewords <- stri_unique(stri_extract_all_words(sampletwt, simplify=TRUE))
samplebloguniquewords <- stri_unique(stri_extract_all_words(sampleblog, simplify=TRUE))
samplenewsuniquewords <- stri_unique(stri_extract_all_words(samplenews, simplify=TRUE))
samplealluniquewords <- stri_unique(stri_extract_all_words(allsample, simplify=TRUE))

#combine sample into table
sampletwtinfo <- c(sampletwtobjsize,sampletwtlines,prettyNum(sum(sampletwtwordcount),big.mark = ",",width=7),prettyNum(mean(sampletwtwordcount),big.mark = ",",width=4),prettyNum(length(sampletwtuniquewords),big.mark = ",",width=5),prettyNum(length(sampletwtuniquewords)/sum(sampletwtwordcount)*100,width=4))
samplebloginfo <- c(sampleblogobjsize,samplebloglines,prettyNum(sum(sampleblogwordcount),big.mark = ",",width=7),prettyNum(mean(sampleblogwordcount),big.mark = ",",width=4),prettyNum(length(samplebloguniquewords),big.mark = ",",width=5),prettyNum(length(samplebloguniquewords)/sum(sampleblogwordcount)*100,width=4))
samplenewsinfo <- c(samplenewsobjsize,samplenewslines,prettyNum(sum(samplenewswordcount),big.mark = ",",width=7),prettyNum(mean(samplenewswordcount),big.mark = ",",width=4),prettyNum(length(samplenewsuniquewords),big.mark = ",",width=5),prettyNum(length(samplenewsuniquewords)/sum(samplenewswordcount)*100,width=4))
sampleallinfo <- c(sampleallobjsize,samplealllines,prettyNum(sum(sampleallwordcount),big.mark = ",",width=7),prettyNum(mean(sampleallwordcount),big.mark = ",",width=4),prettyNum(length(samplealluniquewords),big.mark = ",",width=5),prettyNum(length(samplealluniquewords)/sum(sampleallwordcount)*100,width=4))
sampledatainfo <- rbind(sampletwtinfo,samplebloginfo,samplenewsinfo,sampleallinfo)

#add row and column names
rownames(sampledatainfo) <- c("Twitter", "Blogs", "News","Combined")
colnames(sampledatainfo) <- c("Object Size", "Lines","Number of Words", "Mean Number of Words", "Unique Words", "% Unique Words")

#display sample dataset info
sampledatainfo

#clean memory 2
rm(sampleallobjsize,samplealllines,sampleallwordcount,sampleallinfo,sampletwtobjsize,sampleblogobjsize,samplenewsobjsize,sampletwtlines,samplebloglines,samplenewslines,sampletwtwordcount,sampleblogwordcount,samplenewswordcount,sampletwtinfo,samplebloginfo,samplenewsinfo,sampledatainfo,allwordcount,blogwordcount,newswordcount,twtwordcount,samplealluniquewords,sampletwtuniquewords,samplebloguniquewords,samplenewsuniquewords)

#save 2
save(allsample,file="./reportdata/allsample.RData")
save(sampleblog,file="./reportdata/sampleblog.RData")
save(samplenews,file="./reportdata/samplenews.RData")
save(sampletwt,file="./reportdata/sampletwt.RData")

load("./reportdata/sampletwt.RData")
load("./reportdata/sampleblog.RData")
load("./reportdata/samplenews.RData")
load("./reportdata/allsample.RData")


#display sample's wordcloud ###TO BE COMPLETED##
library(wordcloud)
wordcloud(sampletwt,max.words = 50, random.color = TRUE, random.order = FALSE, colors = brewer.pal(12,"Set3"))
wordcloud(sampleblog,max.words = 50, random.color = TRUE, random.order = FALSE, colors = brewer.pal(12,"Set3"))
wordcloud(samplenews,max.words = 50, random.color = TRUE, random.order = FALSE, colors = brewer.pal(12,"Set3"))
wordcloud(allsample,max.words = 50, random.color = TRUE, random.order = FALSE, colors = brewer.pal(12,"Set3"))


#create profanity file
library(RCurl)

# url pointing to the data source
url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
profanitylist <- getURL(url,ssl.verifypeer = FALSE)

#read into a data frame then convert to character vector
profanitylist <- read.csv(textConnection(profanitylist),header=FALSE) 
plist <- as.vector(profanitylist$V1)

#clean up memory
rm(url,datablog,datanews,datatwitter,sampleblog,samplenews,sampletwt,profanitylist)

#save 3
save(plist,file="./reportdata/plist.RData")

#create corpus cleaning
library(tm)
library(RWeka)
allsample.vec <- VectorSource(allsample)
allsample.corpus <- Corpus(allsample.vec)

#clean memory
rm(allsample)

#save 4
save(allsample.corpus,file="./reportdata/allsamplecorpus.RData")
load("./reportdata/allsamplecorpus.RData")
load("./reportdata/plist.RData")

#clean corpus
#allsample.corpus <- tm_map(allsample.corpus,tolower)
allsample.corpus <- tm_map(allsample.corpus, content_transformer(tolower))
#removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
#allsample.corpus <- tm_map(allample.corpus,removeURL)
allsample.corpus <- tm_map(allsample.corpus,removePunctuation)
allsample.corpus <- tm_map(allsample.corpus,removeNumbers)
allsample.corpus <- tm_map(allsample.corpus, removeWords,plist)
allsample.corpus <- tm_map(allsample.corpus,stripWhitespace)
allsample.corpus <- tm_map(allsample.corpus,PlainTextDocument)

save(allsample.corpus,file="./reportdata/allsamplecorpuscleaned.RData")

#create tokenizers
library(RWeka)
u_token <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bi_token <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tri_token <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
four_token <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
five_token <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

#tokenization for 1 to 5 ngrams
tdm1 <- TermDocumentMatrix(allsample.corpus, control=list(tokenize=u_token))
save(tdm1,file="./reportdata/tdm1.RData")
tdm2 <- TermDocumentMatrix(allsample.corpus, control=list(tokenize=bi_token))
save(tdm2,file="./reportdata/tdm2.RData")
tdm3 <- TermDocumentMatrix(allsample.corpus, control=list(tokenize=tri_token))
save(tdm3,file="./reportdata/tdm3.RData")
tdm4 <- TermDocumentMatrix(allsample.corpus, control=list(tokenize=four_token))
save(tdm4,file="./reportdata/tdm4.RData")
tdm5 <- TermDocumentMatrix(allsample.corpus, control=list(tokenize=five_token))
save(tdm5,file="./reportdata/tdm5.RData")

load("./reportdata/tdm1.RData")
load("./reportdata/tdm2.RData")
load("./reportdata/tdm3.RData")
load("./reportdata/tdm4.RData")
load("./reportdata/tdm5.RData")


#clean memory 5
rm(plist,allsample.vec)


#Markov assumption:  the probability of a word depends only on the probability of a limited history
#Generalization: the probability of a word depends only on the probability of the n previous words

#prepare the TDMs for plotting
#check sparsity
inspect(tdm1)
inspect(tdm2)
inspect(tdm3)
inspect(tdm4)
inspect(tdm5)

#remove sparse terms
tdm1a <- removeSparseTerms(tdm1,0.99)
tdm2a <- removeSparseTerms(tdm2,0.99)
#tdm3a <- removeSparseTerms(tdm3,0.5)
#tdm4a <- removeSparseTerms(tdm4,0.99)
#tdm5a <- removeSparseTerms(tdm5,0.99)

save(tdm1a, file="./reportdata/tdm1a.RData")
save(tdm2a, file="./reportdata/tdm2a.RData")
#save(tdm3a, file="tdm3a.RData")
#save(tdm4a, file="tdm4a.RData")
#save(tdm5a, file="tdm5a.RData")

#convert to matrix
tdm1matrix <- as.matrix(tdm1a)
tdm2matrix <- as.matrix(tdm2a)
#get term freq
tdm1freq <- sort(rowSums(tdm1matrix),decreasing=TRUE)
tdm1df <- data.frame(term=names(tdm1freq),frequency =tdm1freq)
head(tdm1df,50)
tdm2freq <- sort(rowSums(tdm2matrix),decreasing=TRUE)
tdm2df <- data.frame(term=names(tdm2freq),frequency =tdm2freq)
head(tdm2df,50)
#for trigram, fourgram and fivegram, the matrix is too big, thus have to select subset.
tdm3a <- findFreqTerms(tdm3, lowfreq = 10)
tdm3matrix <- as.matrix(tdm3[tdm3a,])
tdm3freq <- sort(rowSums(tdm3matrix), decreasing = TRUE)
tdm3df <- data.frame(term=names(tdm3freq), frequency=tdm3freq)
tdm4a <- findFreqTerms(tdm4, lowfreq = 5)
tdm4matrix <- as.matrix(tdm4[tdm4a,])
tdm4freq <- sort(rowSums(tdm4matrix), decreasing = TRUE)
tdm4df <- data.frame(term=names(tdm4freq), frequency=tdm4freq)
tdm5a <- findFreqTerms(tdm5, lowfreq = 3)
tdm5matrix <- as.matrix(tdm5[tdm5a,])
tdm5freq <- sort(rowSums(tdm5matrix), decreasing = TRUE)
tdm5df <- data.frame(term=names(tdm5freq), frequency=tdm5freq)

save(tdm1df,file="./reportdata/tdm1df.RData")
save(tdm2df,file="./reportdata/tdm2df.RData")
save(tdm3df,file="./reportdata/tdm3df.RData")
save(tdm4df,file="./reportdata/tdm4df.RData")
save(tdm5df,file="./reportdata/tdm5df.RData")

rm(tdm1a,tdm1freq,tdm1matrix,tdm2a,tdm2freq,tdm2matrix,tdm3a,tdm3freq,tdm3matrix,tdm4a,tdm4freq,tdm4matrix,tdm5a,tdm5freq,tdm5matrix)
rm(tdm1,tdm2,tdm3,tdm4,tdm5)

#wordclouds
library(wordcloud)


wordcloud(tdm1df$term,tdm1df$frequency,min.freq=200,max.words = 50, random.color = TRUE, random.order = FALSE, colors = brewer.pal(12,"Set3"))
wordcloud(tdm2df$term,tdm2df$frequency,min.freq=200,max.words = 50, random.color = TRUE, random.order = FALSE, colors = brewer.pal(12,"Set3"))
wordcloud(tdm3df$term,tdm3df$frequency,max.words = 50, random.color = TRUE, random.order = FALSE, colors = brewer.pal(12,"Set3"))
wordcloud(tdm4df$term,tdm4df$frequency,max.words = 50, random.color = TRUE, random.order = FALSE, colors = brewer.pal(12,"Set3"))
wordcloud(tdm5df$term,tdm5df$frequency,max.words = 50, random.color = TRUE, random.order = FALSE, colors = brewer.pal(12,"Set3"))
wordcloud(allsample.corpus,max.words = 50, random.color = TRUE, random.order = FALSE, colors = brewer.pal(12,"Set3"))



#plots
library(ggplot2)

g1 <- ggplot(head(tdm1df,30), aes(x=reorder(term, frequency), y=frequency, fill=frequency)) +
  geom_bar(stat = "identity") +  coord_flip() +  theme_gray() + 
  theme(legend.title=element_blank()) +
  xlab("Unigram") + ylab("Frequency") +
  labs(title = "Top Unigrams by Frequency")
print(g1)

g2 <- ggplot(head(tdm2df,30), aes(x=reorder(term, frequency), y=frequency, fill=frequency)) +
  geom_bar(stat = "identity") +  coord_flip() +  theme_gray() + 
  theme(legend.title=element_blank()) +
  xlab("Unigram") + ylab("Frequency") +
  labs(title = "Top Bigrams by Frequency")
print(g2)

g3 <- ggplot(head(tdm3df,30), aes(x=reorder(term, frequency), y=frequency, fill=frequency)) +
  geom_bar(stat = "identity") +  coord_flip() + theme_gray() + 
  theme(legend.title=element_blank()) +
  xlab("Unigram") + ylab("Frequency") +
  labs(title = "Top Trigrams by Frequency")
print(g3)

g4 <- ggplot(head(tdm4df,30), aes(x=reorder(term, frequency), y=frequency, fill=frequency)) +
  geom_bar(stat = "identity") +  coord_flip() + theme_gray() +
  theme(legend.title=element_blank()) +
  xlab("Unigram") + ylab("Frequency") +
  labs(title = "Top Unigrams by Frequency")
print(g4)

g5 <- ggplot(head(tdm5df,30), aes(x=reorder(term, frequency), y=frequency, fill=frequency)) +
  geom_bar(stat = "identity") +  coord_flip() +  theme_gray() + 
  theme(legend.title=element_blank()) +
  xlab("Unigram") + ylab("Frequency") +
  labs(title = "Top Unigrams by Frequency")
print(g5)


#-----unused------

#calc and display basic info like file size, object size and number of lines.
#file.info("./data/en_US.twitter.txt")$size/1024/1000
#file.info("./data/en_US.blogs.txt")$size/1024/1000
#file.info("./data/en_US.news.txt")$size/1024/1000

#unused word count method
#twordcount <- prettyNum(sum(sapply(gregexpr("\\S+",datatwitter),length)),big.mark = ",") ## capital S
#bwordcount <- prettyNum(sum(sapply(gregexpr("\\S+",datablog),length)),big.mark = ",") ## capital S
#nwordcount <- prettyNum(sum(sapply(gregexpr("\\S+",datanews),length)),big.mark = ",") ## capital S

#identify longest line
#twtcount <- nchar(datatwitter) #count number of char per line
#tmax <- which.max(twtcount)
#nchar(datatwitter[tmax])
#blogcount <- nchar(datablog)
#bmax <- which.max(blogcount)
#nchar(datablog[bmax])
#newscount <- nchar(datanews)
#nmax <- which.max(newscount)
#nchar(datanews[nmax])

#num of words in longest line
#sum(sapply(gregexpr("\\S+",datablog[bmax]),length)) #number of words in the longest line
#sum(sapply(gregexpr("\\S+",datanews[nmax]),length)) #number of words in the longest line
#sum(sapply(gregexpr("\\S+",datatwitter[tmax]),length)) #number of words in the longest line

#write cleaned files for reuse.
#writeLines(cleanedtwt,conn <-file("./data/cleanedtwitter.txt",encoding="UTF-8"))
#close(conn)
#writeLines(cleanedblog,conn <-file("./data/cleanedblog.txt",encoding="UTF-8"))
#close(conn)
#writeLines(cleanednews,conn <-file("./data/cleanednews.txt",encoding="UTF-8"))
#close(conn)

#read clean files
#datatwitterclean <-readLines(conn <-file("./data/cleanedtwitter.txt",encoding="UTF-8"))
#close(conn)
#datanewsclean <-readLines(conn <-file("./data/cleanednews.txt",encoding="UTF-8"))
#close(conn)
#datablogclean <-readLines(conn <-file("./data/cleanedblog.txt",encoding="UTF-8"))
#close(conn)

#remove weird characters
#cleanedtwt <- sapply(datatwitter,function(x) iconv (enc2utf8(x),sub="byte"))
#cleanedblog <- sapply(datablog,function(x) iconv (enc2utf8(x),sub="byte"))
#cleanednews <- sapply(datanews,function(x) iconv (enc2utf8(x),sub="byte"))

#save(allsample,file="allsample.RData")
#load("alldata.RData")

#length(inspect(tdm1[, grepl("sex$", tdm1$dimnames$Terms)]))
