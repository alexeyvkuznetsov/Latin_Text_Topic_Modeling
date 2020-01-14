#library(textmineR)


library(stringr)
library(igraph)
library(ggraph)
library(ggplot2)
library(qgraph)
library(textrank)
library(topicmodels)


#setwd("D:/GitHub/Latin_Text_Topic_Modeling/")

library(tidytext)
library(NLP)
library(tm)
library(topicmodels)
library(udpipe)




prologus<-paste(scan(file ="files/01 prologus.txt",what='character'),collapse=" ")
historia_g<-paste(scan(file ="files/02 historia_g.txt",what='character'),collapse=" ")
recapitulatio<-paste(scan(file ="files/03 recapitulatio.txt",what='character'),collapse=" ")
historia_w<-paste(scan(file ="files/04 historia_w.txt",what='character'),collapse=" ")
historia_s<-paste(scan(file ="files/05 historia_s.txt",what='character'),collapse=" ")

prologus <- tolower(prologus)
historia_g <- tolower(historia_g)
recapitulatio <- tolower(recapitulatio)
historia_w <- tolower(historia_w)
historia_s <- tolower(historia_s)

prologus <- stripWhitespace(prologus)
historia_g <- stripWhitespace(historia_g)
recapitulatio <- stripWhitespace(recapitulatio)
historia_w <- stripWhitespace(historia_w)
historia_s <- stripWhitespace(historia_s)



prologus <- removePunctuation (prologus)
historia_g <- removePunctuation (historia_g)
recapitulatio <- removePunctuation (recapitulatio)
historia_w <- removePunctuation (historia_w)
historia_s <- removePunctuation (historia_s)


prologus<-data.frame(texts=prologus)
historia_g<-data.frame(texts=historia_g)
recapitulatio<-data.frame(texts=recapitulatio)
historia_w<-data.frame(texts=historia_w)
historia_s<-data.frame(texts=historia_s)



prologus$book<-"Prologus"
historia_g$book<-"Historia_Gothorum"
recapitulatio$book<-"Recapitulatio"
historia_w$book<-"Historia_Wandalorum"
historia_s$book<-"Historia_Suevorum"


fivebooks<-rbind(prologus,historia_g,recapitulatio,historia_w,historia_s)

#fourbooks<-rbind(prologus,historia_g,historia_w,historia_s)

#threebooks<-rbind(historia_g,historia_w,historia_s)



udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.4-190531.udpipe")


x <- udpipe_annotate(udmodel_latin, x = fivebooks$texts, doc_id = fivebooks$book, tagger = "default", parser = "default", trace = TRUE)
x <- as.data.frame(x)

x <- udpipe_annotate(udmodel_latin, x = fourbooks$texts, doc_id = fourbooks$book, tagger = "default", parser = "default", trace = TRUE)
x <- as.data.frame(x)

x <- udpipe_annotate(udmodel_latin, x = threebooks$texts, doc_id = threebooks$book, tagger = "default", parser = "default", trace = TRUE)
x <- as.data.frame(x)

save(x,file="fivebooks_annotated_dataset.Rda")


#load("fivebooks_annotated_dataset.Rda")

class(x)

x$topic_level_id <- unique_identifier(x, fields = c("doc_id", "paragraph_id", "sentence_id"))

dtf <- subset(x, upos %in% c("NOUN"))

dtf <- document_term_frequencies(dtf, document = "topic_level_id", term = "lemma")

head(dtf)


dtm <- document_term_matrix(x = dtf)

dtm_clean <- dtm_remove_lowfreq(dtm, minfreq = 2)

head(dtm_colsums(dtm_clean))



#dtm_clean <- dtm_remove_terms(dtm_clean, terms = c("ann.", "annus", "aer", "filius"))

#dtm_clean <- dtm_remove_terms(dtm_clean, terms = c("ann.", "ann", "an", "annus", "aer", "aes", "suus", "filius", "pater", "frater", "pars", "maldra", "theudericus", "hucusque", "hispanium", "caeter", "justinianus", "praelio", "cdxxxnum._rom.", "cdxinum._rom.", "cdxix", "op"))

dtm_clean <- dtm_remove_terms(dtm_clean, terms = c("ann.", "ann", "an", "annus", "aer", "aes", "suus", "filius", "pater", "frater", "pars", "maldra", "theudericus", "gothus", "hucusque", "hispanium", "caeter", "justinianus", "praelio", "cdxxxnum._rom.", "cdxinum._rom.", "cdxix", "op"))


## Or keep of these nouns the top 50 based on mean term-frequency-inverse document frequency

dtm_clean <- dtm_remove_tfidf(dtm_clean, top = 50)



####FINAL

library(topicmodels)

topicModel <- LDA(dtm_clean, k = 4, method = "Gibbs", control = list(nstart = 5, iter = 4000, burnin = 500, best = TRUE, seed = 1:5, alpha = 0.1))

topics(topicModel)




## Менять параметр free_y на free для изменения масштаба

library(ggplot2)
library(dplyr)


td_beta <- tidy(topicModel)
td_beta %>%
  group_by(topic) %>%
  top_n(6, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Наиболее часто встречающиеся слова для каждой темы")





### Распределение тем по документам https://tm4ss.github.io/docs/Tutorial_6_Topic_Models.html


textIds <- c(1, 2, 3, 4, 5)

lapply(fivebooks$texts[textIds], as.character)

tmResult <- posterior(topicModel)

theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicModel, 7), 2, paste, collapse = " ")  

attr(topicModel, "alpha")

# load libraries for visualization

library("reshape2")
library("ggplot2")


# get topic proportions form example documents

N <- 5

topicProportionExamples <- theta[textIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)





###
Определение оптимального значения K для моделирования
# https://rpubs.com/MNidhi/NumberoftopicsLDA
# https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
###
library(topicmodels)
library(ldatuning)

result <- FindTopicsNumber(
  dtm_clean,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "Arun2010"),
  method = "Gibbs",
  control = NULL,
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

control = list(iter = 2000, burnin = 1000, best = TRUE, seed = 1:6, alpha = 0.1),


library(topicmodels)
library(ldatuning)

result <- FindTopicsNumber(
  dtm_clean,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "VEM",
  control = NULL,
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)

dtm_clean

result <- FindTopicsNumber(
  dtm_clean,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(nstart = 5, burnin = 1000, iter = 1000, best = TRUE, seed = 1:5, alpha = 0.1),
  mc.cores = 2L,
  verbose = TRUE
)

FindTopicsNumber_plot(result)






###
Другой вариант log-likelihood логарифмическое правдоподобие

library(topicmodels)

best.model <- lapply(seq(2,30, by=1), function(k){LDA(dtm_clean, k)})

best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))

best.model.logLik.df <- data.frame(topics=c(2:30), LL=as.numeric(as.matrix(best.model.logLik)))


library(ggplot2)
ggplot(best.model.logLik.df, aes(x=topics, y=LL)) + 
  xlab("Количество тем k") + ylab("Log likelihood of the model") + 
  geom_line() + 
  theme_bw() 

best.model.logLik.df[which.max(best.model.logLik.df$LL),]

best.model.logLik.df



best.model <- lapply(seq(2,30, by=1), function(k){LDA(dtm_clean, k, method = "Gibbs",control = list(burnin = 1000, iter = 2000, seed = 1:5, best = TRUE, alpha =0.1))})


best.model <- lapply(seq(2,20, by=1), function(k){LDA(dtm_clean, k, method = "Gibbs",control = list(best = TRUE, alpha =0.1))})




library(ggplot2)
ggplot(best.model.logLik.df, aes(x=topics, y=LL)) + 
  xlab("Number of topics") + ylab("Log likelihood of the model") + 
  geom_line() + 
  theme_bw()  + 
  opts(axis.title.x = theme_text(vjust = -0.25, size = 14)) + 
  opts(axis.title.y = theme_text(size = 14, angle=90))


best.model.logLik.df[which.max(best.model.logLik.df$LL),]

###

###
Другой вариант The harmonic mean function
https://knowledger.rbind.io/post/topic-modeling-using-r/
###

library(Rmpfr)

harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}


k <- 25
burnin <- 1000
iter <- 1000
keep <- 50
alpha <- 0.1
fitted <- topicmodels::LDA(dtm_clean, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep, alpha =alpha) )

## assuming that burnin is a multiple of keep
logLiks <- fitted@logLiks[-c(1:(burnin/keep))]

## This returns the harmomnic mean for k = 25 topics.
harmonicMean(logLiks)


seqk <- seq(2, 25, 1)
burnin <- 1000
iter <- 1000
keep <- 50
alpha <- 0.1
system.time(fitted_many <- lapply(seqk, function(k) topicmodels::LDA(dtm_clean, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep, alpha =alpha) )))


# extract logliks from each topic
logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

# compute harmonic means
hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))



ldaplot <- ggplot(data.frame(seqk, hm_many), aes(x=seqk, y=hm_many)) + geom_path(lwd=1.5) +
  theme(text = element_text(family= NULL),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Harmonic Mean') +
     annotate("text", x = 25, y = -80000, label = paste("The optimal number of topics is", seqk[which.max(hm_many)])) +
  ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of NEN LLIS", atop(italic("How many distinct topics in the abstracts?"), ""))))
ldaplot


####
3 вариант
Когерентность
####



k_list <- seq(1, 20, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm_clean, k = k, iterations = 500)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm_clean, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) 


#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")


####
4 вариант
Когерентность
https://cran.r-project.org/web/packages/textmineR/vignettes/c_topic_modeling.html 
####

# load a sample DTM
data(dtm_clean)

# choose a range of k 
# - here, the range runs into the corpus size. Not recommended for large corpora!
k_list <- seq(2,30, by=1)

# you may want toset up a temporary directory to store fit models so you get 
# partial results if the process fails or times out. This is a trivial example, 
# but with a decent sized corpus, the procedure can take hours or days, 
# depending on the size of the data and complexity of the model.
# I suggest using the digest package to create a hash so that it's obvious this 
# is a temporary directory

model_dir <- paste0("models_", digest::digest(colnames(dtm_clean), algo = "sha1"))

# Fit a bunch of LDA models
# even on this trivial corpus, it will a bit of time to fit all of these models

model_list <- TmParallelApply(X = k_list, FUN = function(k){

  m <- FitLdaModel(dtm = dtm_clean, 
                   k = k, 
                   iterations = 2000, 
                   burnin = 180,
                   alpha = 0.1,
                   beta = colSums(dtm_clean) / sum(dtm_clean) * 100,
                   optimize_alpha = TRUE,
                   calc_likelihood = FALSE,
                   calc_coherence = TRUE,
                   calc_r2 = FALSE,
                   cpus = 1)
  m$k <- k
  
  m
}, export= c("dtm_clean"), 
cpus = 2) 

# c("dtm_clean"), # export only needed for Windows machines
# Get average coherence for each model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)


# Plot the result
# On larger (~1,000 or greater documents) corpora, you will usually get a clear peak
plot(coherence_mat, type = "o")


# https://towardsdatascience.com/beginners-guide-to-lda-topic-modelling-with-r-e57a5a8e7a25


coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
 coherence = sapply(model_list, function(x) mean(x$coherence)), 
 stringsAsFactors = FALSE)

ggplot(coherence_mat, aes(x = k, y = coherence)) +
 geom_point() +
 geom_line(group = 1)+
 ggtitle("Best Topic by Coherence Score") + theme_minimal() +
 scale_x_continuous(breaks = seq(1,40,1)) + ylab("Coherence")


#########

library(textmineR)

# load a sample DTM
data(nih_sample_dtm)
# choose a range of k 
# - here, the range runs into the corpus size. Not recommended for large corpora!
k_list <- seq(1,20, by=1)
# you may want toset up a temporary directory to store fit models so you get 
# partial results if the process fails or times out. This is a trivial example, 
# but with a decent sized corpus, the procedure can take hours or days, 
# depending on the size of the data and complexity of the model.
# I suggest using the digest package to create a hash so that it's obvious this 
# is a temporary directory
model_dir <- paste0("models_", digest::digest(colnames(dtm_clean), algo = "sha1"))
# Fit a bunch of LDA models
# even on this trivial corpus, it will a bit of time to fit all of these models
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  m <- FitLdaModel(dtm = dtm_clean, 
                   k = k, 
                   iterations = 1000, 
                   burnin = 1000,
                   alpha = 0.1,
                   beta = colSums(dtm_clean) / sum(dtm_clean) * 100,
                   optimize_alpha = TRUE,
                   calc_likelihood = FALSE,
                   calc_coherence = TRUE,
                   calc_r2 = FALSE,
                   cpus = 1)
  m$k <- k
  
  m
}, export= ls(), # c("dtm_clean"), 
cpus = 2) 
# Get average coherence for each model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
# Plot the result
# On larger (~1,000 or greater documents) corpora, you will usually get a clear peak
plot(coherence_mat, type = "o")




##########














##4-2-2
library(topicmodels)
m <- LDA(dtm_clean, k = 4, method = "Gibbs", control = list(nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))

topics(m)

##ИЛИ

topicModel <- LDA(dtm_clean, k = 4, method = "Gibbs", control = list(iter = 2000, nstart = 5, burnin = 2000, best = TRUE, seed = 1:5, alpha = 0.2) )

topics(topicModel)



####FINAL

topicModel <- LDA(dtm_clean, k = 4, method = "Gibbs", control = list(nstart = 5, iter = 4000, burnin = 500, best = TRUE, seed = 1:5, alpha = 0.1))

topics(topicModel)


### ИНТЕРЕСНОЕ РАСПРЕДЕЛЕНИЕ 6 тем

topicModel <- LDA(dtm_clean, k = 6, method = "Gibbs", control = list(nstart = 5, iter = 2000, burnin = 1000, best = TRUE, seed = 1:5, alpha = 0.1))


topicModel <- LDA(dtm_clean, k = 16, method = "Gibbs", control = list(nstart = 5, iter = 2000, burnin = 1000, best = TRUE, seed = 1:5, alpha = 0.1))




topicModel <- LDA(dtm_clean, k = 9, method = "Gibbs", control = list(alpha = 0.1))




topicModel <- LDA(dtm_clean, k = 5, method = "Gibbs", control = list(nstart = 5, burnin = 2000, best = TRUE, verbose = 25, seed = 1:5, alpha = 0.2))

topicModel <- LDA(dtm_clean, k = 5, method = "Gibbs", control = list(nstart = 5, burnin = 2000, best = TRUE, verbose = 25, seed = 1:5))

topicModel <- LDA(dtm_clean, k = 8, method = "Gibbs", control = list(nstart = 5, burnin = 4000, iter = 2000, thin = 500, best = TRUE, seed = 1:5, alpha = 0.1))

topics(topicModel)





#####
_______
set.seed(1234)
topicModel<-LDA(dtm_clean,k=6,method = "Gibbs")

topics(topicModel)

freq<-colSums(as.matrix(dtm_clean))

ord<-order(-freq)

freq<-colSums(as.matrix(dtm_clean))

barplot(freq[head(ord)])
______
####




### Визуализация https://www.tidytextmining.com/topicmodeling.html
library(tidytext)

ap_topics <- tidy(topicModel, matrix = "beta")
ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(6, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()


###
ХОРОШАЯ ВИЗУАЛИЗАЦИЯ МОДЕЛЕЙ
###

## Менять параметр free_y на free для изменения масштаба

library(ggplot2)
library(dplyr)


td_beta <- tidy(topicModel)
td_beta %>%
    group_by(topic) %>%
    top_n(6, beta) %>%
    ungroup() %>%
    mutate(topic = paste0("Topic ", topic),
           term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL, y = expression(beta),
         title = "Наиболее часто встречающиеся слова для каждой темы")



td_beta <- tidy(topicModel)
td_beta %>%
    group_by(topic) %>%
    top_n(6, beta) %>%
    ungroup() %>%
    mutate(topic = paste0("Topic ", topic),
           term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(term, beta, fill = as.factor(topic))) +
    geom_col(alpha = 0.8, show.legend = FALSE) +
facet_wrap(~ topic, scales = "free_y") +
    coord_flip() +
    scale_x_reordered() +
    labs(x = NULL, y = expression(beta),
         title = "Highest word probabilities for each topic",
         subtitle = "Different words are associated with different topics")




### Distribution of document probabilities for each topic

td_gamma <- tidy(topicModel, matrix = "gamma",                    
                 document_names = rownames(sherlock_dfm))

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with 1-3 stories",
       y = "Number of stories", x = expression(gamma))






### Распределение тем по документам https://tm4ss.github.io/docs/Tutorial_6_Topic_Models.html


textIds <- c(1, 2, 3, 4, 5)
lapply(fivebooks$texts[textIds], as.character)


### K = значение 10, 6, 4
###topicModel2 <- LDA(dtm_clean, k = 6, method = "Gibbs", control = list(iter = 2000, nstart = 5, burnin = 2000, best = TRUE, seed = 1:5))

topicModel <- CTM(dtm_clean, k=3, method="VEM", control = NULL, model = NULL)

topicModel <- LDA(dtm_clean, k=4, method="VEM", control = NULL, model = NULL)

topicModel <- LDA(dtm_clean, k=5, method="VEM", control = list(iter = 700, burnin = 500, verbose = 25, alpha = 0.2))

topicModel <- LDA(dtm_clean, K, method="Gibbs", control=list(iter = 500, verbose = 25, , alpha = 0.1))


topicModel <- LDA(dtm_clean, k = 6, method = "Gibbs", control = list(nstart = 5, iter = 1000, burnin = 1000, best = TRUE, verbose = 25, seed = 1:5, alpha = 0.1))

class(topicModel)






tmResult <- posterior(topicModel)

theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicModel, 7), 2, paste, collapse = " ")  

attr(topicModel, "alpha")

# load libraries for visualization

library("reshape2")
library("ggplot2")


# get topic proportions form example documents

N <- 5

topicProportionExamples <- theta[textIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  

ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)














library(textmineR)

set.seed(12345)

model <- FitLdaModel(dtm = dtm_clean, 
                     k = 6,
                     iterations = 3000, 
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2) 

str(model)


model$r2

plot(model$log_likelihood, type = "l")






perplexity_df <- data.frame(train=numeric(), test=numeric())
topics <- c(2:15)
burnin = 100
iter = 1000
keep = 50

set.seed(12345)
for (i in topics){
 
  fitted <- LDA(dtm_clean, k = i, method = "Gibbs",
                control = list(burnin = burnin, iter = iter, keep = keep) )
  perplexity_df[i,1] <- perplexity(fitted, newdata = dtm_clean)
  perplexity_df[i,2]  <- perplexity(fitted, newdata = dtm_clean) 
}


##plotting the perplexity of both train and test

g <- ggplot(data=perplexity_df, aes(x= as.numeric(row.names(perplexity_df)))) + labs(y="Perplexity",x="Number of topics") + ggtitle("Perplexity of hold out  and training data")

g <- g + geom_line(aes(y=test), colour="red")
g <- g + geom_line(aes(y=train), colour="green")
g








k_list <- seq(1, 20, by = 1)
model_dir <- paste0("models_", digest::digest(vocabulary, algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)
model_list <- TmParallelApply(X = k_list, FUN = function(k){
  filename = file.path(model_dir, paste0(k, "_topics.rda"))
  
  if (!file.exists(filename)) {
    m <- FitLdaModel(dtm = dtm, k = k, iterations = 500)
    m$k <- k
    m$coherence <- CalcProbCoherence(phi = m$phi, dtm = dtm, M = 5)
    save(m, file = filename)
  } else {
    load(filename)
  }
  
  m
}, export=c("dtm", "model_dir")) # export only needed for Windows machines
#model tuning
#choosing the best model
coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)
ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Best Topic by Coherence Score") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,20,1)) + ylab("Coherence")
