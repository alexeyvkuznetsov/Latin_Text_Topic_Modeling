setwd("D:/GitHub/Latin_Text_Topic_Modeling/")

# clean current workspace
rm(list=ls(all=T))

library(textmineR)
library(igraph)
library(ggraph)
library(ggplot2)
library(tm)
library(udpipe)
library(stm)

prologus<-paste(scan(file ="files/01 prologus.txt",what='character'),collapse=" ")
historia_g<-paste(scan(file ="files/02 historia_g.txt",what='character'),collapse=" ")
recapitulatio<-paste(scan(file ="files/03 recapitulatio.txt",what='character'),collapse=" ")
historia_w<-paste(scan(file ="files/04 historia_w.txt",what='character'),collapse=" ")
historia_s<-paste(scan(file ="files/05 historia_s.txt",what='character'),collapse=" ")

prologus<-data.frame(texts=prologus)
historia_g<-data.frame(texts=historia_g)
recapitulatio<-data.frame(texts=recapitulatio)
historia_w<-data.frame(texts=historia_w)
historia_s<-data.frame(texts=historia_s)

prologus$book<-"01 Prologus"
historia_g$book<-"02 Historia Gothorum"
recapitulatio$book<-"03 Recapitulatio"
historia_w$book<-"04 Historia Wandalorum"
historia_s$book<-"05 Historia Suevorum"


historia<-rbind(prologus,historia_g,recapitulatio,historia_w,historia_s)

#historia$texts <- stripWhitespace(historia$texts)
historia$texts <- tolower(historia$texts)
historia$texts <- removePunctuation(historia$texts)
historia$texts <- removeNumbers(historia$texts)



# UDPipe annotation
#udmodel_latin <- udpipe_download_model(language = "latin-ittb")
#udmodel_latin <- udpipe_load_model(ud_model$file_model)
udmodel_latin <- udpipe_load_model(file = "latin-ittb-ud-2.5-191206.udpipe")


x <- udpipe_annotate(udmodel_latin, x = historia$texts, doc_id = historia$book, tagger = "default", parser = "default", trace = TRUE)
x <- as.data.frame(x)


#save(x,file="hustoria_annotated_dataset.Rda")


#load("fivebooks_annotated_dataset.Rda")



dtf <- subset(x, upos %in% c("NOUN"))

dtf <- document_term_frequencies(dtf, document = "doc_id", term = "lemma")

head(dtf)


dtm <- document_term_matrix(x = dtf)

dtm <- dtm_remove_lowfreq(dtm, minfreq = 4)

head(dtm_colsums(dtm))

dtm <- dtm_remove_terms(dtm, terms = c("ann.", "ann", "an", "annus", "aer", "aes", "suus", "filius", "pater", "frater", "pars", "maldra", "theudericus", "hucusque", "hispanium", "caeter", "justinianus", "praelio", "cdxxxnum._rom.", "cdxinum._rom.", "cdxix", "op"))


# https://github.com/AlexByzov93/ibd_2020/blob/master/Class%209/Scripts_for%20project%202/Models%20preparation.R


library(tidyverse)
library(stm)
library(tidytext)
library(quanteda)
library(qdap)
library(furrr)
annotated_plots <- x


#####
#Preprocess the data:
#- lowercase the lemmas;
#- delete numbers, symbols, punctuation, and unknown parts of speech (X in the `upos` column);
#- remove stop words
#####

annotated_plots_clean <- annotated_plots %>% 
  mutate(lemma = str_to_lower(lemma)) %>% 
  filter(!upos %in% c("X", "SYM", "NUM", "PUNCT")) %>% 
  anti_join(stop_words, by = c("lemma" = "word"))



# Создание dfm из data frame UDPipe
annotated_plots_clean %>% 
  count(doc_id, lemma) %>% 
  cast_dfm(doc_id, lemma, n) -> dfm



DFM2stm <- convert(dfm, to = "stm") 

docs <- DFM2stm$documents
vocab <- DFM2stm$vocab
meta <-DFM2stm$meta

documents <- out$documents
vocab <- out$vocab
meta <- out$meta
set.seed(02138)
K<-c(2,4,10) 
kresult <- searchK(docs, vocab, K, prevalence=~treatment + s(pid_rep), data=meta)
plot(kresult)



IdealK <- searchK(DFM2stm$documents, DFM2stm$vocab
                  , K = seq(4, 15, by = 1), max.em.its = 75
                  , seed = 9999)




#Construct term-document matrix
mat <- readCorpus(dtm)
processed <- prepDocuments(mat$documents, mat$vocab, lower.thresh = 2)

#ptm <- proc.time()
#Run searchK function
kresult <- searchK(processed$documents, processed$vocab, c(0))
#print(proc.time() - ptm)



dfm2stm <- convert(dfm, to = "stm", docvars = docvars(dfm))

mein.stm.idealK <- searchK(dfm2stm$documents, dfm2stm$vocab, K = seq(2, 15, by = 1), max.em.its = 75)




kresult.D.de.deepl <- searchK(documents = dfm2stm$documents, vocab = dfm2stm$vocab, data = dfm2stm$meta,
                              K = c(2, 3, 4, 5), init.type = "LDA")
save(kresult.D.de.deepl, file = 'data/kresult_D_de_deepl.RData')
plot.searchK(kresult)








library(stm)
library(furrr)


plan("default")
start_time_stm <- Sys.time()

many_models <- tibble(K = c(2, 4, 6, 8, 10, 12)) %>%
  mutate(topic_model = future_map(K, ~stm(dfm, K = .,
                                          verbose = FALSE)))
threads_sparse <- dfm

many_models <- tibble::tibble(K = seq(2, 15, by = 1)) %>%
  dplyr::mutate(topic_model = future_map(K, ~stm(threads_sparse, K = .,
                                                 verbose = FALSE)))
heldout <- make.heldout(threads_sparse)

many_models <- data_frame(K = seq(2, 15, by = 1)) %>% 
  mutate(topic_model = future_map(K, ~stm(threads_sparse, 
                                          K = .,
                                          verbose = FALSE)))



nTopics <- seq(2,10,1)

many_models_stm <- data_frame(K = nTopics) %>%
  mutate(topic_model = future_map(K, ~stm(dfm, K = ., verbose = TRUE)))





end_time_stm <- Sys.time() # 4 mins

heldout <- make.heldout(dfm)

k_result <- many_models_stm %>%
  mutate(exclusivity        = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, dfm),
         eval_heldout       = map(topic_model, eval.heldout, heldout$missing),
         residual           = map(topic_model, checkResiduals, gfm),
         bound              = map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact              = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound             = bound + lfact,
         iterations         = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  transmute(K,
            `Lower bound`         = lbound,
            Residuals             = map_dbl(residual, "dispersion"),
            `Semantic coherence`  = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x        = "K (number of topics)",
       y        = NULL,
       title    = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicate that a good number of topics would be around 15")


excl_sem_plot <- k_result                          %>%
  select(K, exclusivity, semantic_coherence)       %>%
  filter(K %in% seq(2,16,2)) %>%
  unnest()                                         %>%
  mutate(K = as.factor(K))                         %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 5, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence",
       subtitle = "Models with fewer topics have higher semantic coherence for more topics, but lower exclusivity") +
  scale_color_viridis_d()

excl_sem_plot

anim_plot <- excl_sem_plot +
  labs(title = 'K: {round(frame_time,0)}') +
  transition_time(as.numeric(K)) +
  ease_aes('linear')
animate(anim_plot, nframes = 8, fps = 0.5)




#++++++++++++++++






dfm <- dfm %>% 
  dfm_trim(min_termfreq = 4)

#plan(multicore)

models <- tibble(
  K = seq(1, 10, 1)
) %>% 
  mutate(
    topic_models = future_map(K,
                              ~stm(dfm,
                                   K = .x,
                                   seed = 1000
                              )
    )
  )

many_models <- data_frame(K = c(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)) %>%
  mutate(topic_model = future_map(K, ~stm(dfm, K = .,
                                          verbose = FALSE)))


heldout <- make.heldout(dfm)



library("stm")
library(furrr)
plan(multiprocess)

many_models <- tibble::tibble(K = seq(2, 10, by = 1)) %>%
  dplyr::mutate(topic_model = future_map(K, ~stm(dfm, K = .,
                                                 verbose = FALSE)))



many_models <- data_frame(K = seq(5, 20, by = 5)) %>% # Here we're running four topic models: 5 topics, 10 topics, 15 topics and 20 topics
  mutate(topic_model = future_map(K, ~stm(dfm, 
                                          K = .,
                                          verbose = FALSE)))
toc()

DFM <- dfm

t <- Sys.time()

DFM2stm <- convert(DFM, to = "stm") 
IdealK <- searchK(DFM2stm$documents, DFM2stm$vocab, 
                  K = seq(3, 10, by = 1), 
                  seed = 9999)

plot(IdealK)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, dfm),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, dfm),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result

# Diagnostic plots
k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics",
       subtitle = "These diagnostics indicatre 10 to be a good number")



saveRDS(models, "STM_1.RDS")

heldout <- make.heldout(dfm)

models_statistics <- models %>% 
  mutate(
    exclusivity = map(topic_models, exclusivity),
    semantic_coherence = map(topic_models, semanticCoherence, dfm),
    heldout = map(topic_models, eval.heldout, heldout$missing),
    residual = map(topic_models, checkResiduals, dfm)
  )

models_statistics %>% 
  select(K, exclusivity, semantic_coherence) %>% 
  unnest(cols = c(exclusivity, semantic_coherence)) %>% 
  ggplot(aes(x = exclusivity, y = semantic_coherence, color = factor(K))) +
  geom_point()


models_statistics %>% 
  select(K, exclusivity, semantic_coherence) %>% 
  mutate(
    exclusivity = map_dbl(exclusivity, ~mean(unlist(.x))),
    semantic_coherence = map_dbl(semantic_coherence, ~mean(unlist(.x)))
  )

models_statistics %>% 
  mutate(
    exclusivity = map_dbl(exclusivity, ~mean(unlist(.x))),
    semantic_coherence = map_dbl(semantic_coherence, ~mean(unlist(.x))),
    heldout = map_dbl(heldout, function(.x) .x[[1]]),
    residual = map_dbl(residual, function(.x) .x[[1]])
  ) %>% 
  select(K, exclusivity, semantic_coherence, heldout, residual) %>% 
  View()

models_statistics %>% 
  mutate(
    exclusivity = map_dbl(exclusivity, ~mean(unlist(.x))),
    semantic_coherence = map_dbl(semantic_coherence, ~mean(unlist(.x))),
    heldout = map_dbl(heldout, function(.x) .x[[1]]),
    residual = map_dbl(residual, function(.x) .x[[1]])
  ) %>% 
  select(K, exclusivity, semantic_coherence, heldout, residual) %>% 
  pivot_longer(
    exclusivity:residual
  ) %>% 
  ggplot(aes(K, value, factor(name))) +
  geom_smooth() +
  theme_classic() +
  facet_wrap(~factor(name), scales = "free")

sageLabels(models$topic_models[3][[1]])

broom::tidy(models$topic_models[3][[1]], "beta") %>% 
  group_by(topic) %>% 
  slice_max(beta, n = 5) %>% 
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ factor(topic), scales = 'free')


# estimamos K ----------------------

library(furrr)
plan(multiprocess)

many_models <- data_frame(K = seq(1, 10, by = 1)) %>%
  mutate(topic_model = future_map(K, ~stm(dtm, K = .,
                                          verbose = TRUE)))

many_models <- data_frame(K = c(5, 6, 7, 8, 9, 10, 11)) %>%
  mutate(topic_model = future_map(K, ~stm(dtm, K = .,
                                          verbose = FALSE)))

heldout <- make.heldout(dtm)

k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, dtm),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, dtm),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

k_result %>%
  transmute(K,
            `Lower bound` = lbound,
            Residuals = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (number of topics)",
       y = NULL,
       title = "Model diagnostics by number of topics")

k_result %>%
  select(K, exclusivity, semantic_coherence) %>%
  unnest() %>%
  mutate(K = as.factor(K)) %>%
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

topic_model <- k_result %>% 
  filter(K == 50) %>% 
  pull(topic_model) %>% 
  .[[1]]

topic_model







# Coherence score for topics

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
  scale_x_continuous(breaks = seq(1,15,1)) + ylab("Когерентность тем")




# Topic modeling

library(topicmodels)

topicModel <- topicmodels::LDA(dtm, k = 4, method = "Gibbs", control = list(nstart = 5, iter = 4000, burnin = 500, best = TRUE, seed = 1:5, alpha = 0.1))

topics(topicModel)


# Topics vizualization

library(tidytext)
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






### Topic proportions https://tm4ss.github.io/docs/Tutorial_6_Topic_Models.html


textIds <- c(1, 2, 3, 4, 5)

lapply(historia$book[textIds], as.character)

tmResult <- topicmodels::posterior(topicModel)

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




#########################################################################
### LDAvis VISUALISATION                                              ###
#########################################################################
# https://github.com/love-borjeson/tm_ws_cloud/blob/master/3_ling_filter.R
# udpipe + LDAvis



topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

require(LDAvis)

serVis(topicmodels2LDAvis(topicModel),  out.dir = 'LDAvis', open.browser = interactive())

serVis(topicmodels2LDAvis(topicModel))
#servr::daemon_stop(1) # to stop the server 



