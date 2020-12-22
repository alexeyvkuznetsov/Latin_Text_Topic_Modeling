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

setwd("D:/GitHub/Latin_Topic_Modeling/")

#Here the file path is input using a Direct Source command 
#which is in turn transformed into a corpus. Also telling R 
#to look in multiple folders (recursive)
corpus <- Corpus(DirSource(
  directory = "D:/GitHub/Latin_Topic_Modeling/files/", 
  encoding = "UTF-8",
  recursive = TRUE,
  mode = "text"
))
