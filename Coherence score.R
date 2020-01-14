#setwd("D:/GitHub/Latin_Text_Topic_Modeling/")

library(textmineR)
library(igraph)
library(ggraph)
library(ggplot2)
library(tm)
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


udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.4-190531.udpipe")


x <- udpipe_annotate(udmodel_latin, x = fivebooks$texts, doc_id = fivebooks$book, tagger = "default", parser = "default", trace = TRUE)
x <- as.data.frame(x)


dtf <- subset(x, upos %in% c("NOUN"))

dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")

head(dtf)


dtm <- document_term_matrix(x = dtf)

dtm <- dtm_remove_lowfreq(dtm, minfreq = 4)

head(dtm_colsums(dtm))

dtm <- dtm_remove_terms(dtm, terms = c("ann.", "ann", "an", "annus", "aer", "aes", "suus", "filius", "pater", "frater", "pars", "maldra", "theudericus", "gothus", "hucusque", "hispanium", "caeter", "justinianus", "praelio", "cdxxxnum._rom.", "cdxinum._rom.", "cdxix", "op"))


dtm <- dtm_remove_terms(dtm, terms = c("ann.", "ann", "an", "annus", "aer", "aes", "suus", "filius", "pater", "frater", "pars", "maldra", "theudericus", "hucusque", "hispanium", "caeter", "justinianus", "praelio", "cdxxxnum._rom.", "cdxinum._rom.", "cdxix", "op"))




k_list <- seq(1,15, by=1)



model_dir <- paste0("models_", digest::digest(colnames(dtm), algo = "sha1"))
if (!dir.exists(model_dir)) dir.create(model_dir)


model_list <- TmParallelApply(X = k_list, FUN = function(k){
  
  m <- FitLdaModel(dtm = dtm, 
                   k = k, 
                   iterations = 4000, 
                   burnin = 500,
                   alpha = 0.1,
                   optimize_alpha = FALSE,
                   calc_likelihood = FALSE,
                   calc_coherence = TRUE,
                   calc_r2 = FALSE,
                   cpus = 1)
  m$k <- k
  
  m
}, export= c("dtm"), 
cpus = 2)



coherence_mat <- data.frame(k = sapply(model_list, function(x) nrow(x$phi)), 
                            coherence = sapply(model_list, function(x) mean(x$coherence)), 
                            stringsAsFactors = FALSE)

ggplot(coherence_mat, aes(x = k, y = coherence)) +
  geom_point() +
  geom_line(group = 1)+
  ggtitle("Оптимальное количество тем (k)") + theme_minimal() +
  scale_x_continuous(breaks = seq(1,15,1)) + ylab("Когерентность модели")
























