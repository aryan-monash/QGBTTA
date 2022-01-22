library(tidyverse)
library(tidytext)
library(textmineR)
library(SnowballC)
library(textstem)
library(proustr)
library(spacyr)
library(parallel)
library(text2vec)
#library(tm)



# Initial Data Manipulation -----------------------------------------------


# all_pm_speech <- readRDS("data/all_pm_speech.Rds")
# current_pm_speech <- readRDS("data/2018-09-24-morrison.Rds")
# 
# pm_speech_final <- bind_rows(all_pm_speech, current_pm_speech)

# Read binded file directly
pm_speech_final <- readRDS("data/pm_speech_final.Rds")

# # Short document removal
pm_speech_final <- pm_speech_final[
  stringi::stri_length(pm_speech_final$document) > 50,]

# Fix punctuations
library(proustr)
pm_speech_final <- pm_speech_final %>%
  pr_normalize_punc(document)

#saveRDS(pm_speech_final, "Data/pm_speech_final.Rds")


# Text Preprocessing ---------------------------------------------------------

## Stopword assignment -------------------------------------------------------

custom_stop_words <- tribble(
    ~word, ~lexicon,
    "prime", "CUSTOM",
    "minister", "CUSTOM",
    "australia", "CUSTOM",
    "australian", "CUSTOM",
    "state", "CUSTOM",
    "government", "CUSTOM",
    "prime", "CUSTOM",
    "pm", "CUSTOM",
    "member", "CUSTOM",
    "committee", "CUSTOM",
    "commission", "CUSTOM",
    "house", "CUSTOM",
    "journalist", "CUSTOM"
)

## Named entity recognition to filter out names of people ---------------------

# Taking random sample
training_ids = sample(1:5000,5000)
pm_speech_final_sam = pm_speech_final[training_ids,]
# Running parts of speech recognition system
parsed_speeches <- spacyr::spacy_parse(pm_speech_final_sam$document,  
                                         lemma = FALSE, 
                                         entity = TRUE, 
                                         nounphrase = TRUE, multithread = TRUE)
# Removing stopwords from parsed speech
parsed_nostop <- parsed_speeches %>% 
  filter(!(token %in% stopwords))
# Keeping distinct versions of lemmatized tokens
spacy_filter <- parsed_nostop %>% 
  distinct(token, .keep_all = TRUE) %>% 
  mutate(lemma = textstem::lemmatize_words(token)) %>% 
  distinct(lemma, .keep_all = TRUE)
# Filter to keep only words recognised as Persons in speech
spacy_filter <- spacy_filter[spacy_filter$entity %in% c("PERSON_I", "PERSON_B"),]
# Renaming NER filter to be compatible with existing stopwords
spacy_filter <- spacy_filter %>% mutate("lexicon"="PERSONS") %>% rename("word"="token")

## Creating document term matrix ----------------------------------------------

# Creating Tidy dataset

stop_words2 <- get_stopwords(source = "stopwords-iso")  %>%
  bind_rows(custom_stop_words)

tidy_speech <- pm_speech_final %>%
  #mutate(line = row_number()) %>%
  #proustr::pr_normalize_punc(document)  %>%
  unnest_tokens(word, document, strip_numeric = TRUE, strip_punct = TRUE) %>%
  anti_join(stop_words2) %>% 
  anti_join(spacy_filter[,c("word", "lexicon" )]) %>% 
  mutate(word = textstem::lemmatize_words(word)) %>%
  count(id, word, sort = TRUE) %>% 
  filter(nchar(word) > 2)

# Creating DFM
speech_dfm <- tidy_speech %>%
  cast_dfm(id, word, n)

# Creating DTM
dtm <- CreateDtm(doc_vec = pm_speech_final$document, # character vector of documents
                 doc_names = pm_speech_final$id, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords(source = "stopwords-iso"), # this is the default value
                                  custom_stop_words,
                                  spacy_filter[,c("word", "lexicon")]),
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 stem_lemma_function = function(x) textstem::lemmatize_words(x),
                 verbose = FALSE,
                 cpu = 2) # Turn off status bar for this demo)
dtm <- dtm[,colSums(dtm) > 2]

# #explore the basic frequency
# tf <- TermDocFreq(dtm = dtm)
# original_tf <- tf %>% select(term, term_freq, doc_freq)
# rownames(original_tf) <- 1:nrow(original_tf)
# # Eliminate words appearing less than 2 times or in more than half of the documents
# vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]

# Hyperparameter Tuning - K -----------------------------------------------

fit_lda_model <- function(numtopics, dataset) {
  lda_model <- LDA$new(n_topics = numtopics,
                       doc_topic_prior = 1, #0.1
                       topic_word_prior = 1) #0.01
  lda_model_fit <- lda_model$fit_transform(dataset,
                                           n_iter = 300, #2000
                                           convergence_tol = 0.001,
                                           n_check_convergence = 25)
  lda_model
}

validate_top <- function(numtopics,
                         in_data,
                         out_data){
  fitted_lda <- fit_lda_model(numtopics = numtopics, dataset = in_data)
  perpl <- perplexity(out_data, 
                      topic_word_distribution = fitted_lda$topic_word_distribution, 
                      doc_topic_distribution = fitted_lda$transform(out_data))
  perpl
}


compute_models <- function(topics, numfolds, trainingdata){
  splitfolds <- sample(1:numfolds, quanteda::ndoc(trainingdata), replace = TRUE)
  perplexities <- matrix(nrow = numfolds, ncol = length(topics))
  for (i in 1:numfolds) {
    in_data <- trainingdata[splitfolds != i, ]
    out_data <- trainingdata[splitfolds == i, ]
    perplexities[i,] <- unlist(mclapply(topics, 
                                        validate_top, 
                                        in_data, 
                                        out_data,
                                        mc.cores = 1L))
  }
  final_models <- mclapply(topics, 
                           fit_lda_model, 
                           dataset = trainingdata, 
                           mc.cores = 1L)
  list(perplexities, final_models)
}

# Running CV
ntop <- seq(150, 200, by = 10) # (100, 125, 150, 175, 200)
fm_12grams_measures <- compute_models(ntop, numfolds = 5, trainingdata = speech_dfm)

# Perplexity
colMeans(fm_12grams_measures[[1]]) %>% plot(type = "b")

# Coherence
tcm = crossprod(sign(dtm))
coherence_sc <- data.frame(k = ntop)
#coherence_sc['k'] <- ntop 
coherence_sc['Perplexity'] <- colMeans(fm_12grams_measures[[1]])
for(i in 1:length(ntop)) {
  tw = fm_12grams_measures[[2]][[i]]$get_top_words(n = 10, lambda = 1)
  res = coherence(tw, tcm, n_doc_tcm = nrow(pm_speech_final))
  #colMeans(res)[[1]] -> c_v
  coherence_sc[i,3] = colMeans(res)[[1]] #mean_logratio
  coherence_sc[i,4] = colMeans(res)[[2]] #mean_pmi
  coherence_sc[i,5] = colMeans(res)[[3]] #mean_npmi
  coherence_sc[i,6] = colMeans(res)[[4]] #mean_difference
  coherence_sc[i,7] = colMeans(res)[[5]] #mean_npmi_cosim
  coherence_sc[i,8] = colMeans(res)[[6]] #mean_npmi_cosim2
}

coherence_sc %>% 
  #mutate(V2 = ((V2 - min(V2)) / (max(V2) - min(V2)))) %>% 
  scale() %>% as.data.frame() -> csc 
# Peaks in coherence and dips in perplexity indicate 
# higher computational accuracy of model.
csc %>% 
  ggplot(aes(x = ntop)) +
  geom_point(aes(y=Perplexity,color="Perplexity"), shape=17) +
  geom_line(aes(y=Perplexity,color="Perplexity")) +
  geom_point(aes(y=V3, color = "mean_logratio")) +
  geom_line(aes(y=V3, color = "mean_logratio")) +
  geom_point(aes(y=V4, color = "mean_pmi")) +
  geom_line(aes(y=V4, color = "mean_pmi")) +
  geom_point(aes(y=V5, color = "mean_npmi")) +
  geom_line(aes(y=V5, color = "mean_npmi")) +
  geom_point(aes(y=V6, color = "mean_difference")) +
  geom_line(aes(y=V6, color = "mean_difference")) +
  geom_point(aes(y=V7, color = "mean_npmi_cosim")) +
  geom_line(aes(y=V7, color = "mean_npmi_cosim")) +
  geom_point(aes(y=V8, color = "mean_npmi_cosim2")) +
  geom_line(aes(y=V8, color = "mean_npmi_cosim2")) +
  scale_x_continuous(breaks = seq(from = 100, to = 200, by = 10))+
  labs(y="Scaled Coherence", x = "Topic Number (k)")


# Filtered model -------------------------------------------------------------

## Gibbs-sampler (Computationally Heavy)
set.seed(12345)
model_110 <- FitLdaModel(dtm = dtm_sam,
                     k = 110,
                     iterations = 300, #same as bybee
                     burnin = 180,
                     alpha = 1, # same as bybee
                     beta = 1, # same as bybee
                     optimize_alpha = FALSE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpu = 2)

model_160 <- FitLdaModel(dtm = dtm_sam,
                         k = 160,
                         iterations = 300, #same as bybee
                         burnin = 180,
                         alpha = 1, # same as bybee
                         beta = 1, # same as bybee
                         optimize_alpha = FALSE,
                         calc_likelihood = TRUE,
                         calc_coherence = TRUE,
                         calc_r2 = TRUE,
                         cpu = 2)

model_190 <- FitLdaModel(dtm = dtm_sam,
                         k = 190,
                         iterations = 300, #same as bybee
                         burnin = 180,
                         alpha = 1, # same as bybee
                         beta = 1, # same as bybee
                         optimize_alpha = FALSE,
                         calc_likelihood = TRUE,
                         calc_coherence = TRUE,
                         calc_r2 = TRUE,
                         cpu = 2)


# Final Model evaluation --------------------------------------------------------

## Coherence score --------
coh_score <- function(ldamod, dtm, n_topwords){
  # Get the top terms of each topic
  ldamod$top_terms <- GetTopTerms(phi = ldamod$phi, M = n_topwords)
  # See top terms
  head(t(ldamod$top_terms))
  # prevalence should be proportional to alpha
  ldamod$prevalence <- colSums(ldamod$theta) / sum(ldamod$theta) * 100
  # textmineR has a naive topic labeling tool based on probable bigrams
  ldamod$labels <- LabelTopics(assignments = ldamod$theta > 0.05, 
                              dtm = dtm,
                              M = 1)
  # put them together, with coherence into a summary table
  ldamod$summary <- data.frame(topic = rownames(ldamod$phi),
                              label = ldamod$labels,
                              coherence = round(ldamod$coherence, 3),
                              prevalence = round(ldamod$prevalence,3),
                              top_terms = apply(ldamod$top_terms, 2, function(x){
                                paste(x, collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
  model_test <- ldamod$summary[ order(ldamod$summary$prevalence, decreasing = TRUE) , ][ 1:20 , ]
  coh_score <- mean(model_test$coherence)
  return(coh_score)
}

coh_score(model_110, dtm, 20) # coherence score for k=110
coh_score(model_160, dtm, 20) # coherence score for k=160
coh_score(model_190, dtm, 20) # coherence score for k=190

## LDAVIS -----

## Warp LDA (Much quicker, slightly less accurate) ---
lda_model_110 = LDA$new(n_topics = 110, doc_topic_prior = 1, topic_word_prior = 1)
doc_topic_distr =
  lda_model_110$fit_transform(x = dtm, n_iter = 300,
                              convergence_tol = 0.001, n_check_convergence = 25,
                              progressbar = TRUE)
lda_model_110$plot(out.dir = "ldavis_110", open.browser = FALSE)

lda_model_160 = LDA$new(n_topics = 160, doc_topic_prior = 1, topic_word_prior = 1)
doc_topic_distr =
  lda_model_160$fit_transform(x = dtm, n_iter = 300,
                              convergence_tol = 0.001, n_check_convergence = 25,
                              progressbar = TRUE)
lda_model_160$plot(out.dir = "ldavis_160", open.browser = FALSE)

lda_model_190 = LDA$new(n_topics = 190, doc_topic_prior = 1, topic_word_prior = 1)
doc_topic_distr =
  lda_model_190$fit_transform(x = dtm, n_iter = 300,
                              convergence_tol = 0.001, n_check_convergence = 25,
                              progressbar = TRUE)
lda_model_190$plot(out.dir = "ldavis_190", open.browser = FALSE)


# Plot dendogram ------------------
model$topic_linguistic_dist <- CalcHellingerDist(model$phi)
model$hclust <- hclust(as.dist(model$topic_linguistic_dist), "ward.D")
model$hclust$labels <- paste(model$hclust$labels, model$labels[ , 1])
plot(model$hclust)


# word, topic relationship ------------------------------------------------


#looking at the terms allocated to the topic and their pr(word|topic)
allterms <-data.frame(t(model$phi))
allterms$word <- rownames(allterms)
rownames(allterms) <- 1:nrow(allterms)
allterms <- data.table::melt(allterms,idvars = "word") 
allterms <- allterms %>% rename(topic = variable)
FINAL_allterms <- allterms %>% group_by(topic) %>% arrange(desc(value))


# per-document-per-topic probabilities ------------------------------------


#trying to see the topic in each document
theta_df <- data.frame(model$theta)
theta_df$document <-rownames(theta_df) 
rownames(theta_df) <- 1:nrow(theta_df)
theta_df <- data.table::melt(theta_df,id.vars = "document")
theta_df <- theta_df %>% rename(topic = variable) 
theta_df <- theta_df %>% left_join(rownames_to_column((as.data.frame(model$labels))),
                                   by=c("topic" = "rowname"))
theta_df <- theta_df %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
FINAL_document_topic <- theta_df %>% group_by(document) %>% 
  arrange(desc(value)) %>% filter(row_number() == 1)
theta_df <- theta_df %>% 
  mutate(document = as.numeric(str_extract(document, "[[:digit:]]+")))


# Visualise topics over time ----------------------------------------------

library(tsibble)
selected_topics <- c(1,2,3,4,5) #select interested topcis

speech_topic_prob <- pm_speech_final %>% mutate(id = as.numeric(id))
speech_topic_prob <- dplyr::left_join(speech_topic_prob,theta_df,by=c("id" = "document"))
stp1 <- filter(speech_topic_prob, topic %in% selected_topics)
test <- stp1 %>%
  mutate(month = yearmonth(date)) %>%
  group_by(month, topic) %>%
  mutate(value = quantile(value, 0.5)) %>% 
  ungroup() %>% 
  arrange(month)
test2 <- stp1 %>%
  mutate(month = yearmonth(date)) %>%
  group_by(month, topic) %>%
  arrange(desc(value)) %>%
  filter(row_number() %in% c(1,2,3)) %>% 
  arrange(month) %>% 
  select(-value)
test3 <- dplyr::left_join(test2,test)
#test3 <- test3 %>% filter(value <= quantile(test3$value, .99)[[1]])

plotly::ggplotly(test3 %>% ggplot(aes(x = month, y = value, color = topic)) +
                   geom_line() +
                   facet_wrap(~topic))
