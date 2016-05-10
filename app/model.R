library(tm)
library(data.table)
library(stringr)

predictSBO <-function(input,plist,unigramDT, bigramDT, trigramDT) {

  setkey(unigramDT,unigram,freq) 
  setkey(bigramDT,bigram,freq)
  setkey(trigramDT,trigram,freq)
  setorder(unigramDT,unigram)
  setorder(bigramDT,bigram)
  setorder(trigramDT,trigram)
  
  input <- tolower(input)#to lower case
  input <- removePunctuation(input)#remove punctuation
  input <- removeNumbers(input)#remove number
  input <- stripWhitespace(input) #remove unwanted whitespaces
  input <- input[grepl('[[:alpha:]]',input)] #makes sure its alphabets
  input <- gsub("^ ", "", input) # Remove blank spaces at the front
  input <- gsub(" $", "", input) # Remove blank spaces at the end
  
  
  ##if(input == ''|input == "na na" | input == "na") return("No word predicted as user did not provide valid input!") #end if empty input
  if(length(input) == 0) return("No word predicted as user did not provide valid input!") #end if empty input
  
  inputwords <- strsplit(input,split=" ") #split input into vector of words
  
  ##if length of input >2, get last 2 words. else input is already either 1 or 2 words.
  if (length(inputwords[[1]]) > 2)  
  {
    splitinput <- tail(inputwords[[1]],2)
    input <- paste(splitinput[1],splitinput[2])
  }
  
  if (length(inputwords[[1]]) >=  2) #can do trigram search as two words available
  {
    seektri<-grepl(paste0("^",input,"$"),trigramDT$bigram)#match input to bigram component of trigram
    subtri<-trigramDT[seektri,]#retrieve all matched trigram if present
    input2 <- tail(strsplit(input,split=" ")[[1]],1) #get the second word
    seekbi <- grepl(paste0("^",input2,"$"),bigramDT$unigram)#match second word to unigram component of bigram
    subbi <- bigramDT[seekbi,]#retrieve all matched unigram in bigram if present
    
    if (sum(seektri) == 0) #no match found in trigram
    {
      if(sum(seekbi)==0) #no match found in bigram
      {                 # so take top most frequency word in unigram
        setorder(unigramDT,-freq)
        return(unigramDT[1]$unigram) #in function, change print() to return()
     ##   print("unigram two word") #this line just for debugging
      } else
      {
        #got match in bigram i.e. sum(seekbi) > 0
        setorder(subbi,-freq)
        final <- setdiff(subbi[1]$word,plist)
        final <- final[grepl('[[:alpha:]]',final)]
        return(final) #in function, change print() to return()
      ##  print("bigram two word") #this line just for debugging
      } 
    } else
    {
      #got match in trigram i.e. sum(seektri) > 0
      setorder(subtri,-freq)
      final <- setdiff(subtri[1]$word,plist)
      final <- final[grepl('[[:alpha:]]',final)]
      return(final) #in function, change print() to return()
    ##  print("trigram two word") #this line just for debugging
    }
  } else #input is one word only, means compare with bigram and skip trigram
  {
    seekbi <- grepl(paste0("^",input,"$"),bigramDT$unigram)#match second word to unigram component of bigram
    subbi <- bigramDT[seekbi,]#retrieve all matched unigram in bigram if present
    
    if(sum(seekbi)==0) #no match found in bigram
    {                 # so take top most frequency word in unigram
      setorder(unigramDT,-freq)
      return(unigramDT[1]$unigram) #in function, change print() to return()
    ##  print("unigram one word") #this line just for debugging
    } else
    {
      #got match in bigram i.e. sum(seekbi) > 0
      setorder(subbi,-freq)
      final <- setdiff(subbi[1]$word,plist)
      final <- final[grepl('[[:alpha:]]',final)]
      return(final) #in function, change print() to return()
   ##   print("bigram one word") #this line just for debugging
    } 
  } #close else for one word
}



SimpleGT <- function(table_N){
  #Simple Good Turing Algorithm - Gale And Simpson
  #Good Turing Smoothing
  
  # table_U is a table of frequency of frequencies
  # The frequencies are stored as names of the list in the table structure
  # the values are the frequency of frequencies.
  # In Good Turing Smoothing, we are concerned with the frequency of frequencies
  # So, to extract the number of times that words of frequency n occur in the training set, we need to do:
  # table(freq_B)[[as.character(pairCount)]]
  # In a tables with a number of holes or non-contiguous sequence of frequency of words,
  # we can compute N(c+1) as the mean of all frequencies that occur more than Nc times
  # to do this, create a vector that is in the numerical form of the names of the table
  
  # create a data table
  # r is the frequencies of various trigrams
  #n is the frequency of frquencies
  SGT_DT <- data.frame(r=as.numeric(names(table_N)),n=as.vector(table_N),Z=vector("numeric",length(table_N)), 
                       logr=vector("numeric",length(table_N)),
                       logZ=vector("numeric",length(table_N)),
                       r_star=vector("numeric",length(table_N)),
                       p=vector("numeric",length(table_N)))
  #p=vector("numeric",length(table_N)),key="r")
  
#  str(SGT_DT)
  
  num_r <- nrow(SGT_DT)
  for (j in 1:num_r) {
    if(j==1) {r_i<-0} else {r_i <- SGT_DT$r[j-1]}
    if(j==num_r){r_k<-SGT_DT$r[j]} else {r_k <- SGT_DT$r[j+1]}
    SGT_DT$Z[j] <- 2*SGT_DT$n[j] / (r_k-r_i)
    #print(paste(r_i,j,r_k))
  }
  SGT_DT$logr <- log(SGT_DT$r)
  SGT_DT$logZ <- log(SGT_DT$Z)
  linearFit <- lm(SGT_DT$logZ ~ SGT_DT$logr)
#  print(linearFit$coefficients)
  c0 <- linearFit$coefficients[1]
  c1 <- linearFit$coefficients[2]
  
 # plot(SGT_DT$logr, SGT_DT$logZ)
 # abline(linearFit,col="red")
  
  use_y = FALSE
  for (j in 1:(num_r-1)) {
    r_plus_1 <- SGT_DT$r[j] + 1
    
    s_r_plus_1 <- exp(c0 + (c1 * SGT_DT$logr[j+1]))
    s_r <- exp(c0 + (c1 * SGT_DT$logr[j]))
    y<-r_plus_1 * s_r_plus_1/s_r
    
    if(use_y) {
      SGT_DT$r_star[j] <- y
    } else { 
      n_r_plus_1 <- SGT_DT$n[SGT_DT$r == r_plus_1]
      n_r <- SGT_DT$n[j]
      x<-(r_plus_1) * n_r_plus_1/n_r
      
      if (abs(x-y) > 1.96 * sqrt(((r_plus_1)^2) * (n_r_plus_1/((n_r)^2))*(1+(n_r_plus_1/n_r)))) {
        SGT_DT$r_star[j] <- x
      }else {
        SGT_DT$r_star[j] <- y
        use_y = TRUE
      }
    }
    if(j==(num_r-1)) {
      SGT_DT$r_star[j+1] <- y
    }
    
  }
  N <- sum(SGT_DT$n * SGT_DT$r)
  Nhat <- sum(SGT_DT$n * SGT_DT$r_star)
  Po <- SGT_DT$n[1] / N
  SGT_DT$p <- (1-Po) * SGT_DT$r_star/Nhat
  
  return(SGT_DT)
  
}


predictSGT <- function(input,plist,unigramDT, bigramDT, trigramDT) {
  
  input <- tolower(input)#to lower case
  input <- removePunctuation(input)#remove punctuation
  input <- removeNumbers(input)#remove number
  input <- stripWhitespace(input) #remove unwanted whitespaces
  input <- input[grepl('[[:alpha:]]',input)] #makes sure its alphabets
  input <- gsub("^ ", "", input) # Remove blank spaces at the front
  input <- gsub(" $", "", input) # Remove blank spaces at the end
  
  freqt <- table(trigramDT$freq)
  freqb <- table(bigramDT$freq)
  
  tri_SGT_DT <- SimpleGT(freqt)
  bi_SGT_DT <- SimpleGT(freqb)
  
  ##if(input == ''|input == "na na" | input == "na") return("No word predicted as user did not provide valid input!") #end if empty input
  if(length(input) == 0) return("No word predicted as user did not provide valid input!") #end if empty input
  
  inputwords <- strsplit(input,split=" ") #split input into vector of words
  
  ##if length of input >2, get last 2 words. else input is already either 1 or 2 words.
  if (length(inputwords[[1]]) > 2)  
  {
    splitinput <- tail(inputwords[[1]],2)
    input <- paste(splitinput[1],splitinput[2])
  }
  

  # get the subset of the trigram data table witha matching bigram start
  swf_T <- trigramDT[bigram == input]
  #check if bigram was found in the trigram table
  if(nrow(swf_T) > 0) {
    # use the counts in the Simple GT table to extract the probability
    swf_T$p <- sapply(swf_T$freq,FUN=function(x) tri_SGT_DT$p[tri_SGT_DT$r==x]) ##need do
    # order by probability
    #swf_T <- swf_T[with(swf_T,order(-p))]
    # find the largest probability
    maxP <- max(swf_T$p)
    #get the end words with the highest probability
    predictList <- swf_T[p == maxP]$word  
    return(head(predictList,1))
    #pl_T <- data.frame(words=swf_T$end,probs=swf_T$p)
    #return(pl_T)
  } else {
    input2 <- tail(strsplit(input,split=" ")[[1]],1) #get the second word
    swf_B <- bigramDT[unigram == input2]
    if(nrow(swf_B) > 0) {
      # use the counts in the Simple GT table to extract the probability
      swf_B$p <- sapply(swf_B$freq,FUN=function(x) bi_SGT_DT$p[bi_SGT_DT$r==x]) 
      # order by probability
      swf_B <- swf_B[with(swf_B,order(-p))]
      # find the largest probability
      maxP <-max(swf_B$p)
      #get the end words with the highest probability
      predictList <- swf_B[p == maxP]$word
      return(head(predictList,1))
    } else {
      return(paste("No match found"))
    }
  }
}