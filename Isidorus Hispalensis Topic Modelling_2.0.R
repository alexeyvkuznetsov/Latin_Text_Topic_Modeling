# clean current workspace
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # supress math annotation
# install libraries
#install.packages(c("tm", "topicmodels", "reshape2", "ggplot2", "wordcloud", "pals"))


#LOADING A CORPUS FROM LOCAL FOLDERS:
  
#Load the text mining package(s)
library("tm")

#Set the working directory

setwd("D:/GitHub/Latin_Text_Topic_Modeling/")

#Here the file path is input using a Direct Source command 
#which is in turn transformed into a corpus. Also telling R 
#to look in multiple folders (recursive)
corpus <- Corpus(DirSource(
  directory = "D:/GitHub/Latin_Text_Topic_Modeling/files/", 
  encoding = "UTF-8",
  recursive = TRUE,
  mode = "text",
))

#PRE-PROCESSING THE CORPUS:

#Load the text mining package(s)
#library("SnowballC")

#Setting the stopwords dictionaries and custom words
#myStopwords <- c(stopwords("english"), "my", "custom", "words")

#Pre-processing and tranforming the Corpus
#myStopwords <- c(stopwords("english"), stopwords("SMART"))
your_corpus <- tm_map(corpus, content_transformer(tolower))
#your_corpus <- tm_map(your_corpus, removeWords, myStopwords) 
your_corpus <- tm_map(your_corpus, removeNumbers)
your_corpus <- tm_map(your_corpus, removePunctuation)
your_corpus <- tm_map(your_corpus, stripWhitespace)
your_corpus <- tm_map(your_corpus, stemDocument)

#CREATING A DOCUMENT TERM MATRIX:

#Create a document term matrix (containing between 3 & 
#Infinite characters)
myDtm <- DocumentTermMatrix(
  your_corpus, control=list(
    wordLengths=c(3,Inf)
  )
)

#show the document terms matrix summary
myDtm

#Find frequent terms
findFreqTerms(myDtm, 100)





#CALCULATING THE TERM FREQUENCY - INVERSE DOCUMENT FREQUENCY:
  
library("slam")

#Calculate TF-IDF
term_tfidf <- 
  tapply(myDtm$v/row_sums(myDtm)[myDtm$i], myDtm$j, mean) * log2(nDocs(myDtm)/col_sums(myDtm > 0))

#Display results of TF-IDF
summary(term_tfidf)


#Retaining features that have a high TF-IDF value
myDtm <- myDtm[,term_tfidf >= 0.008] 
myDtm <- myDtm[row_sums(myDtm) > 0,] 
summary(col_sums(myDtm))

#Reports the dimensions of the DTM
dim(myDtm)


#Saving the DTM to local folder
save(myDtm, file = "my_Dtm.Rdata")


#CREATING A WORDCLOUD:

library("wordcloud")

#convert document term matrix to a term document matrix
myTdm <- t(myDtm)

#define tdm as matrix
m = as.matrix(myTdm)

# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)

# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)

# plot wordcloud with maximum of 200 words
wordcloud(dm$word, dm$freq,  max.words=200, random.order=FALSE, 
          rot.per=.2, colors=brewer.pal(9, "Dark2"))


#SETTING LDA PARAMETERS FOR TOPIC MODELLING:

#Load the text mining package(s)
library("topicmodels")

#Set the working directory
setwd("C:/Users/Documents/R/DATASET/")

#Setting up parameters for LDA topic modelling
topics <- 5 * c(1:3, 5, 7)
SEED <- 20080809
BURNIN = 1000
THIN = 100 
ITER = 1000
BEST = FALSE

#Loading your doucment term matrix
load("my_Dtm.Rdata")
data <- myDtm


#X-VALIDATION TO DETERMINE OPTIMUM NUMBER OF TOPICS (1):

# X-Validation - splitting data into 10 Test datasets 
# with remainder used in Training
D <- nrow(data)
folding <- sample(rep(seq_len(10), ceiling(D))[seq_len(D)]) 
for (k in topics)
{
  for (chain in seq_len(10)) 
  {
    FILE <- paste("Gibbs_", k, "_", chain, ".rda", sep = "")
    
    training <- LDA(data[folding != chain,], k = k, 
                    control = list(seed = SEED, 
                                   burnin = BURNIN, thin = THIN, iter = ITER, best = BEST), 
                    method = "Gibbs")
    
    best_training <- training@fitted[[which.max(logLik(training))]]
    
    testing <- LDA(data[folding == chain,], model = best_training, 
                   control = list(estimate.beta = FALSE, seed = SEED, 
                                  burnin = BURNIN, 
                                  thin = THIN, iter = ITER, best = BEST))
    
    save(training, testing, file = file.path("results", FILE))
  }
}
