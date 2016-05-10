setwd("C:/Users/BPM/Downloads/DataScience/Assignment/M10-Capstone")


##con <- file("./data/en_US.twitter.txt", "r") 
## Read the first line of text readLines(con, 1) 
## Read the next line of text readLines(con, 5) 
## Read in the next 5 lines of text close(con) ##

datatwitter <-readLines(conn <-file("./data/en_US.twitter.txt",encoding="UTF-8"))
close(conn)
datablog <- readLines(conn <-file("./data/en_US.blogs.txt",encoding="UTF-8"))
close(conn)
datanews <- readLines(conn <-file("./data/en_US.news.txt",encoding="UTF-8"))
close(conn)

head(datatwitter)
summary(datatwitter)
summary(datablog)
summary(datanews)
length(datatwitter)
length(datablog)
length(datanews)
file.info("./data/en_US.twitter.txt")$size
file.info("./data/en_US.blogs.txt")$size/1024/1000
twordcount <- sum(sapply(gregexpr("\\S+",datatwitter),length)) ## capital S
bwordcount <- sum(sapply(gregexpr("\\S+",datablog),length)) ## capital S
nwordcount <- sum(sapply(gregexpr("\\S+",datanews),length)) ## capital S



#identify longest line
twtcount <- nchar(datatwitter) #count number of char per line
tmax <- which.max(twtcount)
nchar(datatwitter[tmax])
blogcount <- nchar(datablog)
bmax <- which.max(blogcount)
nchar(datablog[bmax])
newscount <- nchar(datanews)
nmax <- which.max(newscount)
nchar(datanews[nmax])

sum(sapply(gregexpr("\\S+",datablog[bmax]),length)) #number of words in the longest line
sum(sapply(gregexpr("\\S+",datanews[nmax]),length)) #number of words in the longest line
sum(sapply(gregexpr("\\S+",datatwitter[tmax]),length)) #number of words in the longest line


#compare two words ratio
twtword1 <- grep("love",datatwitter)
twtword2 <- grep("hate",datatwitter)
length(twtword1)/length(twtword2)

#find matching word
twtword <- grep("biostats",datatwitter)
datatwitter[twtword]

#find matching string
twtstring <- grep("I'm gonna start tweeting more.",datatwitter)
twtstring <- grep("A computer once beat me at chess, but it was no match for me at kickboxing",dataraw2)
datatwitter[twtstring]

#remove weird characters
cleanedtwt <- sapply(datatwitter,function(x) iconv (enc2utf8(x),sub="byte"))
cleanedblog <- sapply(datablog,function(x) iconv (enc2utf8(x),sub="byte"))
cleanednews <- sapply(datanews,function(x) iconv (enc2utf8(x),sub="byte"))
summary(cleanedtwt)

writeLines(cleanedtwt,conn <-file("./data/cleanedtwitter.txt",encoding="UTF-8"))
close(conn)
writeLines(cleanedblog,conn <-file("./data/cleanedblog.txt",encoding="UTF-8"))
close(conn)
writeLines(cleanednews,conn <-file("./data/cleanednews.txt",encoding="UTF-8"))
close(conn)

datatwitterclean <-readLines(conn <-file("./data/cleanedtwitter.txt",encoding="UTF-8"))
close(conn)
datanewsclean <-readLines(conn <-file("./data/cleanednews.txt",encoding="UTF-8"))
close(conn)
datablogclean <-readLines(conn <-file("./data/cleanedblog.txt",encoding="UTF-8"))
close(conn)

summary(datatwitterclean)
datatwitter[167155]
datatwitterclean[167155]

#create sample
set.seed(1234)
sampletwt <- sample(cleanedtwt,10000)
sampleblog <- sample(cleanedblog,10000)
samplenews <- sample(cleanednews,10000)



#cleaning
library(tm)
twt.vec <- VectorSource(sampletwt)
twt.corpus <- Corpus(twt.vec)
twt.corpus <- tm_map(twt.corpus,tolower)
twt.corpus <- tm_map(twt.corpus,removePunctuation)
twt.corpus <- tm_map(twt.corpus,removeNumbers)
twt.corpus <- tm_map(twt.corpus,stripWhitespace)
twt.corpus <- tm_map(twt.corpus,PlainTextDocument)

library(wordcloud)
wordcloud(twt.corpus,max.words = 200, random.color = TRUE, random.order = TRUE, colors = brewer.pal(12,"Set3"))


