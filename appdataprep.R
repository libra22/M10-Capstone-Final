setwd("C:/Users/BPM/Downloads/DataScience/Assignment/M10-Capstone")


library(RWeka)
library(doParallel)
library(stringi)
library(tm)
library(RCurl)
library(stringr)

jobcluster <- makeCluster(detectCores())
invisible(clusterEvalQ(jobcluster, library(tm)))
invisible(clusterEvalQ(jobcluster, library(stringi)))
invisible(clusterEvalQ(jobcluster, library(wordcloud)))
invisible(clusterEvalQ(jobcluster, library(RWeka)))


#reading files
datatwitter <-readLines(conn <-file("./data/en_US.twitter.txt",encoding="UTF-8"),skipNul = TRUE)
close(conn)
datablog <- readLines(conn <-file("./data/en_US.blogs.txt",encoding="UTF-8"), skipNul = TRUE)
close(conn)
datanews <- readLines(conn <-file("./data/en_US.news.txt","rb",encoding="UTF-8"), skipNul = TRUE)
close(conn)

#combine all datasets into one
alldata <- c(datatwitter,datablog,datanews)

#save 1
save(datablog,file="./appdata/datablog.RData")
save(datanews,file="./appdata/datanews.RData")
save(datatwitter,file="./appdata/datatwitter.RData")
save(alldata,file="./appdata/alldata.RData")

#load 1
load("./appdata/datablog.RData")
load("./appdata/datanews.RData")
load("./appdata/datatwitter.RData")
load("./appdata/alldata.RData")

rm(conn)

#set seed for reproducibility
set.seed(1234)

#create sample 1%
sampletwt <- sample(datatwitter,round(0.01*length(datatwitter)))
sampleblog <- sample(datablog,round(0.01*length(datablog)))
samplenews <- sample(datanews,round(0.01*length(datanews)))

#create sample 5%
sampletwt <- sample(datatwitter,round(0.05*length(datatwitter)))
sampleblog <- sample(datablog,round(0.05*length(datablog)))
samplenews <- sample(datanews,round(0.05*length(datanews)))

#create sample 10%
sampletwt <- sample(datatwitter,round(0.1*length(datatwitter)))
sampleblog <- sample(datablog,round(0.1*length(datablog)))
samplenews <- sample(datanews,round(0.1*length(datanews)))

#create sample 20% but without blog
sampletwt <- sample(datatwitter,round(0.2*length(datatwitter)))
samplenews <- sample(datanews,round(0.2*length(datanews)))


#create sample 0.5%
#sampletwt <- sample(datatwitter,round(0.005*length(datatwitter)))
#sampleblog <- sample(datablog,round(0.005*length(datablog)))
#samplenews <- sample(datanews,round(0.005*length(datanews)))

#create sample 0.1%
sampletwt <- sample(datatwitter,round(0.001*length(datatwitter)))
sampleblog <- sample(datablog,round(0.001*length(datablog)))
samplenews <- sample(datanews,round(0.001*length(datanews)))

sampletwt = gsub("&amp", "", sampletwt)
sampletwt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", sampletwt)
sampletwt = gsub("@\\w+", "", sampletwt)
sampletwt = gsub("[[:punct:]]", "", sampletwt)
sampletwt = gsub("[[:digit:]]", "", sampletwt)
sampletwt = gsub("http\\w+", "", sampletwt)
sampletwt = gsub("[ \t]{2,}", "", sampletwt)
sampletwt = gsub("^\\s+|\\s+$", "", sampletwt) 

#get rid of unnecessary spaces
sampletwt <- str_replace_all(sampletwt," "," ")
# Get rid of URLs
sampletwt <- str_replace_all(sampletwt, "http://t.co/[a-z,A-Z,0-9]*{8}","")
# Take out retweet header, there is only one
sampletwt <- str_replace(sampletwt,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
sampletwt <- str_replace_all(sampletwt,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
sampletwt <- str_replace_all(sampletwt,"@[a-z,A-Z]*","")   

allsample <- c(sampletwt,sampleblog,samplenews)
allsample <- c(sampletwt,samplenews)

save(sampletwt,file="./appdata/sampletwt1pct.RData")
save(sampleblog,file="./appdata/sampleblog1pct.RData")
save(samplenews,file="./appdata/samplenews1pct.RData")
save(allsample,file="./appdata/allsample1pct.RData")

save(sampletwt,file="./appdata/sampletwt5pct.RData")
save(sampleblog,file="./appdata/sampleblog5pct.RData")
save(samplenews,file="./appdata/samplenews5pct.RData")
save(allsample,file="./appdata/allsample5pct.RData")

save(sampletwt,file="./appdata/sampletwt01pct.RData")
save(sampleblog,file="./appdata/sampleblog01pct.RData")
save(samplenews,file="./appdata/samplenews01pct.RData")
save(allsample,file="./appdata/allsample01pct.RData")

save(sampletwt,file="./appdata/sampletwt10pct.RData")
save(sampleblog,file="./appdata/sampleblog10pct.RData")
save(samplenews,file="./appdata/samplenews10pct.RData")
save(allsample,file="./appdata/allsample10pct.RData")

save(sampletwt,file="./appdata/sampletwt20pct.RData")
save(samplenews,file="./appdata/samplenews20pct.RData")
save(allsample,file="./appdata/allsample20pct-minusblog.RData")


rm(alldata,datablog,datanews,datatwitter)

load("./appdata/allsample5pct.RData")

#convert to ASCII
sampletwt <- iconv(sampletwt, 'UTF-8', 'ASCII', " ")
sampleblog <- iconv(sampleblog, 'UTF-8', 'ASCII', " ")
samplenews <- iconv(samplenews, 'UTF-8', 'ASCII', " ")
allsample <- iconv(allsample, 'UTF-8', 'ASCII', " ")



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

#clean memory 2
rm(sampleallobjsize,samplealllines,sampleallwordcount,sampleallinfo,sampletwtobjsize,sampleblogobjsize,samplenewsobjsize,sampletwtlines,samplebloglines,samplenewslines,sampletwtwordcount,sampleblogwordcount,samplenewswordcount,sampletwtinfo,samplebloginfo,samplenewsinfo,sampledatainfo,allwordcount,blogwordcount,newswordcount,twtwordcount,samplealluniquewords,sampletwtuniquewords,samplebloguniquewords,samplenewsuniquewords)


# ** how to map words missing aposthrophe to correct term. e.g. whats -> what's

#create profanity file
library(RCurl)

# url pointing to the data source
url <- "https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
profanitylist <- getURL(url,ssl.verifypeer = FALSE)

#read into a data frame then convert to character vector
profanitylist <- read.csv(textConnection(profanitylist),header=FALSE) 
plist <- as.vector(profanitylist$V1)
save(plist,file="./appdata/plist.RData")

#clean up memory
rm(url,datablog,datanews,datatwitter,sampleblog,samplenews,sampletwt,profanitylist)


#create corpus cleaning
library(tm)
library(RWeka)
allsample.vec <- VectorSource(allsample)
allsample.corpus <- Corpus(allsample.vec)

#clean memory
rm(allsample,allsample.vec)

#save 4
save(allsample.corpus,file="./appdata/allsamplecorpus1pct.RData")
save(allsample.corpus,file="./appdata/allsamplecorpus5pct.RData")
save(allsample.corpus,file="./appdata/allsamplecorpus01pct.RData")
save(allsample.corpus,file="./appdata/allsamplecorpus10pct.RData")
save(allsample.corpus,file="./appdata/allsamplecorpus20pct-minusblog.RData")

load("./appdata/allsamplecorpus5pct.RData")
load("./appdata/allsamplecorpus1pct.RData")
load("./appdata/allsamplecorpus01pct.RData")
load("./appdata/plist.RData")

#clean corpus
#allsample.corpus <- tm_map(allsample.corpus,tolower)
allsample.corpus <- tm_map(allsample.corpus, content_transformer(tolower))

#remove http
for (i in 1:length(allsample.corpus))
{
  allsample.corpus[[i]]$content <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+"," ",allsample.corpus[[i]]$content)
}
save(allsample.corpus,file="./appdata/allsamplecorpuscleanedhttp20pct.RData")

#remove www
for (i in 1:length(allsample.corpus))
{
  print(i)
  allsample.corpus[[i]]$content <- gsub("www[^[:space:]]*"," ",allsample.corpus[[i]]$content)
}

save(allsample.corpus,file="./appdata/allsamplecorpuscleaned10pct.RData")

#remove repeating multi chars (kahkahkah, hohoho etc)
for (i in 1:length(allsample.corpus))
{
  print(i)
  allsample.corpus[[i]]$content <- gsub("\\b(\\S+?)\\1\\1\\S*\\b"," ",allsample.corpus[[i]]$content)
}

#save(allsample.corpus,file="./appdata/allsamplecorpuscleaned10pct.RData")

#remove words with chars repeating 3 times or more (awwww, hmmmm etc)
for (i in 1:length(allsample.corpus))
{
  print(i)
  allsample.corpus[[i]]$content <- gsub("[[:alpha:]]*([[:alpha:]])\\1{3,}[[:alpha:]]*"," ",allsample.corpus[[i]]$content)
}
#save(allsample.corpus,file="./appdata/allsamplecorpuscleaned10pct.RData")

#remove @ and # words
for (i in 1:length(allsample.corpus))
{
  print(i)
  allsample.corpus[[i]]$content <- gsub("[#@]\\w+"," ",allsample.corpus[[i]]$content)
}

#remove @ and # words
for (i in 1:length(allsample.corpus))
{
  print(i)
  allsample.corpus[[i]]$content <- gsub("[#@]\\w+"," ",allsample.corpus[[i]]$content)
}
#save(allsample.corpus,file="./appdata/allsamplecorpuscleaned01pct.RData")

allsample.corpus <- tm_map(allsample.corpus,removePunctuation)
allsample.corpus <- tm_map(allsample.corpus,removeNumbers)
allsample.corpus <- tm_map(allsample.corpus,removeWords,plist)
allsample.corpus <- tm_map(allsample.corpus,stripWhitespace)
allsample.corpus <- tm_map(allsample.corpus,PlainTextDocument)

#save(allsample.corpus,file="./appdata/allsamplecorpusfullycleaned01pct.RData")

#------------------------
st <- "@johna awwwww my kahkahkah, kakaka hoho name llaaaannnaa #hastags www. is john bbbba beckons jojojojo 3 www.book ok www.got.com www.234.ok bye bye http://books.com"

#------------------------
for (i in 1:100)
{
  print(i)
  print(allsample.corpus[[v[i]]]$content)
}

#------------------------



save(allsample.corpus,file="./appdata/allsamplecorpuscleaned5pct.RData")
save(allsample.corpus,file="./appdata/allsamplecorpuscleaned1pct.RData")
save(allsample.corpus,file="./appdata/allsamplecorpuscleaned01pct.RData")

#create tokenizers
library(RWeka)
u_token <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bi_token <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tri_token <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
four_token <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
five_token <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))

#tokenization for 1 to 5 ngrams
tdm1 <- TermDocumentMatrix(allsample.corpus, control=list(tokenize=u_token))
save(tdm1,file="./appdata/tdm15pct.RData")
save(tdm1,file="./appdata/tdm11pct.RData")
save(tdm1,file="./appdata/tdm101pct.RData")
save(tdm1,file="./appdata/tdm110pct.RData")

tdm2 <- TermDocumentMatrix(allsample.corpus, control=list(tokenize=bi_token))
save(tdm2,file="./appdata/tdm25pct.RData")
save(tdm2,file="./appdata/tdm2.RData")
save(tdm2,file="./appdata/tdm210pct.RData")
tdm3 <- TermDocumentMatrix(allsample.corpus, control=list(tokenize=tri_token))
save(tdm3,file="./appdata/tdm35pct.RData")
save(tdm3,file="./appdata/tdm3.RData")
save(tdm3,file="./appdata/tdm310pct.RData")
tdm4 <- TermDocumentMatrix(allsample.corpus, control=list(tokenize=four_token))
save(tdm4,file="./appdata/tdm45pct.RData")
save(tdm4,file="./appdata/tdm410pct.RData")
tdm5 <- TermDocumentMatrix(allsample.corpus, control=list(tokenize=five_token))
save(tdm5,file="./appdata/tdm55pct.RData")
save(tdm5,file="./appdata/tdm5.RData")
save(tdm5,file="./appdata/tdm510pct.RData")

load("./appdata/tdm110pct.RData")
load("./appdata/tdm210pct.RData")
load("./appdata/tdm310pct.RData")
load("./appdata/tdm25pct.RData")
load("./appdata/tdm35pct.RData")
load("./appdata/tdm45pct.RData")
load("./appdata/tdm55pct.RData")
load("./appdata/tdm101pct.RData")


#clean memory 5
rm(plist,allsample.vec,allsample.corpus)


#prepare the TDMs for plotting
#check sparsity
inspect(tdm1)
inspect(tdm2)
inspect(tdm3)
inspect(tdm4)
inspect(tdm5)

#remove sparse terms
tdm1a <- removeSparseTerms(tdm1,0.9999)
tdm2a <- removeSparseTerms(tdm2,0.9999)
tdm3a <- removeSparseTerms(tdm3,0.9999)
#tdm4a <- removeSparseTerms(tdm4,0.99)
tdm5a <- removeSparseTerms(tdm5,0.99999)

save(tdm1a, file="./appdata/tdm1a5pct.RData")
save(tdm2a, file="./appdata/tdm2a5pct.RData")
#save(tdm3a, file="./appdata/tdm3a5pct.RData")
#save(tdm4a, file="./appdata/tdm4a5pct.RData")
#save(tdm5a, file="./appdata/tdm5a5pct.RData")

library(slam)
library(data.table)

#unigram. further processing is either using simple matrix via slam or convert to matrix

#using slam
freq <- rowapply_simple_triplet_matrix(tdm1,sum)
freqa <- rowapply_simple_triplet_matrix(tdm1a,sum)
save(freq,freqa,file="./appdata/tdm1freq10pct.RData")
load("./appdata/tdm1freq10pct.RData")
frequni <- sort(freq, decreasing = T)
unigramDT <- data.table(names(freq),freq)
names(unigramDT) <- c('unigram','freq')


#convert to matrix
tdm1matrix <- as.matrix(tdm1) #not enough memory
tdm1amatrix <- as.matrix(tdm1a)
tdm1freq <- sort(rowSums(tdm1amatrix),decreasing=TRUE)
tdm1df <- data.frame(term=names(tdm1freq),frequency =tdm1freq)
head(tdm1df,50)
save(tdm1amatrix, file="./appdata/tdm1amatrix5pct.RData")
save(unigramDT,file="./appdata/unigramDT10pct.RData")


#bigram
#using slam
freqbi <- rowapply_simple_triplet_matrix(tdm2,sum)
freqabi <- rowapply_simple_triplet_matrix(tdm2a,sum)
save(freqbi,freqabi,file="./appdata/tdm2freq10pct.RData")
firstname <- sapply(strsplit(names(freqbi), ' '), function(a) a[1])
secname <- sapply(strsplit(names(freqbi), ' '), function(a) a[2])
bigramDT <- data.table(names(freqbi),freqbi,firstname,secname)
names(bigramDT) <- c('bigram','freq',"unigram","word")
load("./appdata/tdm2freq10pct.RData")
save(bigramDT,file="./appdata/bigramDT10pct.RData")

#using matrix
tdm2amatrix <- as.matrix(tdm2a)
save(tdm2amatrix, file="./appdata/tdm2amatrix5pct.RData")
#get term freq
tdm2freq <- sort(rowSums(tdm2amatrix),decreasing=TRUE)
tdm2df <- data.frame(term=names(tdm2freq),frequency =tdm2freq)
head(tdm2df,50)

#for trigram, fourgram and fivegram, the matrix is too big, thus have to select subset or using slam
#trigram
#slam
freqtri <- rowapply_simple_triplet_matrix(tdm3,sum)
freqatri <- rowapply_simple_triplet_matrix(tdm3a,sum)
save(freqtri,freqatri,file="./appdata/tdm3freq10pct.RData")
firsttriname <- sapply(strsplit(names(freqtri), ' '),function(a) a[1])
sectriname <- sapply(strsplit(names(freqtri), ' '),function(a) a[2])
tritriname <- sapply(strsplit(names(freqtri), ' '),function(a) a[3])
trigramDT <- data.table(names(freqtri),freqtri,paste(firsttriname,sectriname),tritriname)
names(trigramDT) <- c('trigram','freq','bigram','word')
save(trigramDT,file="./appdata/trigramDT10pct.RData")
save(unigramDT,bigramDT,trigramDT,file = "./appdata/ngram10pct.RData")

setorder(bigramDT,-freq)
setorder(trigramDT,-freq)

bigramDT <- bigramDT[1:500000]
trigramDT <- trigramDT[1:500000]

save(unigramDT,bigramDT,trigramDT,file = "./appdata/ngram10pcttop500k.RData")


load("./appdata/ngram10pct.RData")


#matrix
tdm3a <- findFreqTerms(tdm3, lowfreq = 60)
tdm3amatrix <- as.matrix(tdm3[tdm3a,])
tdm3freq <- sort(rowSums(tdm3amatrix), decreasing = TRUE)
tdm3df <- data.frame(term=names(tdm3freq), frequency=tdm3freq)
save(tdm3a, file="./appdata/tdm3a5pct.RData")
save(tdm3amatrix, file="./appdata/tdm3amatrix5pct.RData")
load("./appdata/tdm3amatrix5pct.RData")

#fourgram

#matrix
tdm4a <- findFreqTerms(tdm4, lowfreq = 20)
tdm4amatrix <- as.matrix(tdm4[tdm4a,])
tdm4freq <- sort(rowSums(tdm4amatrix), decreasing = TRUE)
tdm4df <- data.frame(term=names(tdm4freq), frequency=tdm4freq)
save(tdm4a, file="./appdata/tdm4a5pct.RData")
save(tdm4amatrix, file="./appdata/tdm4amatrix5pct.RData")

#fivegram

#matrix
tdm5a <- findFreqTerms(tdm5, lowfreq = 30)
tdm5amatrix <- as.matrix(tdm5[tdm5a,])
tdm5amatrix <- as.matrix(tdm5a)
tdm5freq <- sort(rowSums(tdm5amatrix), decreasing = TRUE)
tdm5df <- data.frame(term=names(tdm5freq), frequency=tdm5freq)
load(file="./appdata/tdm510pct.RData")

#slam using tdm already reduced via removeSparseTerm
freq <- rowapply_simple_triplet_matrix(tdm5,sum) #doesnt work, too long hours > 8hrs
freqa <- rowapply_simple_triplet_matrix(tdm5a,sum) #0.99999
save(freqa,file="./appdata/freqa510pct.RData")
library(data.table)
dtfreqa <- data.table(term = names(freqa),freq=freqa)

save(tdm1df,file="./appdata/tdm1df5pct.RData")
save(tdm2df,file="./appdata/tdm2df5pct.RData")
save(tdm3df,file="./appdata/tdm3df5pct.RData")
save(tdm4df,file="./appdata/tdm4df5pct.RData")
save(tdm5df,file="./appdata/tdm5df5pct.RData")

rm(tdm1a,tdm1freq,tdm1matrix,tdm2a,tdm2freq,tdm2matrix,tdm3a,tdm3freq,tdm3matrix,tdm4a,tdm4freq,tdm4matrix,tdm5a,tdm5freq,tdm5matrix)
rm(tdm1,tdm2,tdm3,tdm4,tdm5)


test <- tdm5$dimnames$Terms[grep("^all of adam sandlers ", tdm5$dimnames$Terms)]
length(test)

test <- tdm4$dimnames$Terms[grep("^of adam sandlers ", tdm4$dimnames$Terms)]
length(test)
tf <- sort(rowSums(as.matrix(tdm4[test,])), decreasing = TRUE)
tf <- data.frame(tdm4=names(tf), frequency=tf)
head(tf, 4)

test <- tdm3$dimnames$Terms[grep("^adam sandlers ", tdm3$dimnames$Terms)]
length(test)
tf <- sort(rowSums(as.matrix(tdm3[test,])), decreasing = TRUE)
tf <- data.frame(tdm3=names(tf), frequency=tf)
head(tf, 100)

test <- tdm2$dimnames$Terms[grep("^sandlers ", tdm2$dimnames$Terms)]
length(test)


#-----unused-------------------------------------------


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


#length(inspect(tdm1[, grepl("sex$", tdm1$dimnames$Terms)]))

v <- grep(" (w)\\1\\1[a-z|0-9]",allsample.corpus[]$content)
#check for <space>www
v <- grep(" (w)\\1\\1[:alphanum:]?",allsample.corpus[]$content)

x<- grepl("www[^[:space:]]*",st)
x<- gsub("www[^[:space:]]*"," ",st)

#"www\\..*?\\.com
v <- grep("www\\..*? ",allsample.corpus[]$content)
v <- grep("www",allsample.corpus[]$content)

wwwlist <- allsample[grep("www",allsample)]

repeatlist <- allsample[grep("(a|A)\\1{3,}",allsample)]

#------------------------
#search repeating chars
c <- grep("([:alpha:])\\1{3,}",allsample.corpus[]$content)
#------------------------
#search repeating 2-chars (haha, wowo etc)
d<- grep("([:alpha:][:alpha:])\\1{3,}",allsample.corpus[]$content)
#------------------------

x<- grepl("([[:alpha:]][[:alpha:]])\\1{2,} ",st)
x<- gsub("([[:alpha:]][[:alpha:]])\\1{2,} "," ",st)

#------------------------
# #remove repeating 2-chars that repeat at least 3 times example hohoho
x<- grepl("\\b(\\S+?)\\1\\1\\S*\\b",st, perl=TRUE)
x<- gsub("\\b(\\S+?)\\1\\1\\S*\\b"," ",st, perl=TRUE)

#------------------------
#remove repeating char that repeats 3 or more times.
x<- grep("[[:alpha:]]*([[:alpha:]])\\1{3,}[[:alpha:]]*",st, perl=TRUE)
x<- gsub("[[:alpha:]]*([[:alpha:]])\\1{3,}[[:alpha:]]*"," ",st, perl=TRUE)

#-------------------------------------
# #remove hashtags
x<- grep("[#@]\\w+",st)
x<- gsub("[#@]\\w+"," ",allsample.corpus[[11]]$content)

for (i in 1:length(allsample.corpus))
{
  allsample.corpus[[i]]$content <- gsub("[#@]\\w+"," ",allsample.corpus[[i]]$content)
}

v <- grep("[#@]\\w+",allsample.corpus[]$content)

allsample.corpus[[11]]$content

#------------------------
v<- grep("\\b(\\S+?)\\1\\1\\S*\\b",allsample.corpus[]$content, perl=TRUE)
allsample.corpus[[555]]$content
#------------------------
v<- grep("[[:alpha:]]*([[:alpha:]])\\1{3,}[[:alpha:]]*",allsample.corpus[]$content, perl=TRUE)

allsample.corpus[[249]]$content
#------------------------
#remove http
x<- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", st)

for (i in 1:length(allsample.corpus))
{
  allsample.corpus[[i]]$content <- gsub("(f|ht)tp(s?)://(.*)[.][a-z]+"," ",allsample.corpus[[i]]$content)
}

v <- grep("(f|ht)tp(s?)://(.*)[.][a-z]+",allsample.corpus[]$content)

allsample.corpus[[120836]]$content

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

tdm1$dimnames$Terms[grep("([a-z])\\1\\1", tdm1$dimnames$Terms)]
tdm1$dimnames$Terms[grep("([a-z])\\1", tdm1$dimnames$Terms)]

tdm1$dimnames$Terms[grep("^(w)\\1", tdm1$dimnames$Terms)] #w repeating twice or more

tdm1$dimnames$Terms[grep("^(.)\\1", tdm1$dimnames$Terms)] #any char repeating twice or more

tdm1$dimnames$Terms[grep("(.)\\1{3,}", tdm1$dimnames$Terms)] #any char appear 4 times r mor continuously

input <- "  f   "
length(input)
input <- tolower(input)#to lower case
input <- removePunctuation(input)#remove punctuation
input <- removeNumbers(input)#remove number
#input <- stemDocument(input)#hmmm, get rid?
input <- stripWhitespace(input)
##  input <- input[grepl('[[:alpha:]]',input)] #what does this do?

input[grepl('[[:alpha:]]',input)]
