library(tidyverse)
library(textmineR)
library(SnowballC)
library(textstem)
library(proustr)
#library(spacyr)
#library(tm)



# Preprocessing -----------------------------------------------------------


all_pm_speech <- readRDS("data/all_pm_speech.Rds")
current_pm_speech <- readRDS("data/2018-09-24-morrison.Rds")

pm_speech_final <- bind_rows(all_pm_speech, current_pm_speech)

# Short document removal
pm_speech_final <- pm_speech_final[stringi::stri_length(pm_speech_final$document) > 50,]

pm_speech_final <- readRDS("data/pm_speech_final.Rds")


# Fix punctuations
library(proustr)
pm_speech_final <- pm_speech_final %>%
  pr_normalize_punc(document)

#saveRDS(pm_speech_final, "Data/pm_speech_final.Rds")

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

dtm <- CreateDtm(doc_vec = pm_speech_final$document, # character vector of documents
                 doc_names = pm_speech_final$id, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords(source = "stopwords-iso"), # this is the default value
                                  custom_stop_words),
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 stem_lemma_function = function(x) textstem::lemmatize_words(x),
                 verbose = FALSE,
                 cpu = 2) # Turn off status bar for this demo)
dtm <- dtm[,colSums(dtm) > 2]

#explore the basic frequency
tf <- TermDocFreq(dtm = dtm)
original_tf <- tf %>% select(term, term_freq, doc_freq)
rownames(original_tf) <- 1:nrow(original_tf)
# Eliminate words appearing less than 2 times or in more than half of the documents
vocabulary <- tf$term[ tf$term_freq > 1 & tf$doc_freq < nrow(dtm) / 2 ]


# Creating Model ----------------------------------------------------------


set.seed(12345)
model_150 <- FitLdaModel(dtm = dtm,
                     k = 150,
                     iterations = 200,
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = FALSE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE)


# Model evaluation --------------------------------------------------------


# Get the top terms of each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 20)
# See top terms
head(t(model$top_terms))
# prevalence should be proportional to alpha
model$prevalence <- colSums(model$theta) / sum(model$theta) * 100
# textmineR has a naive topic labeling tool based on probable bigrams
model$labels <- LabelTopics(assignments = model$theta > 0.05, 
                            dtm = dtm,
                            M = 1)
# put them together, with coherence into a summary table
model$summary <- data.frame(topic = rownames(model$phi),
                            label = model$labels,
                            coherence = round(model$coherence, 3),
                            prevalence = round(model$prevalence,3),
                            top_terms = apply(model$top_terms, 2, function(x){
                              paste(x, collapse = ", ")
                            }),
                            stringsAsFactors = FALSE)
model_test <- model$summary[ order(model$summary$prevalence, decreasing = TRUE) , ][ 1:20 , ]
m1_score <- mean(model_test$coherence)

# Plot dendogram
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
theta_df <- theta_df %>% tidyr::separate(topic, into =c("t","topic")) %>% select(-t)
FINAL_document_topic <- theta_df %>% group_by(document) %>% 
  arrange(desc(value)) %>% filter(row_number() == 1)
theta_df <- theta_df %>% 
  mutate(document = as.numeric(str_extract(document, "[[:digit:]]+")))


# Visualise topics over time ----------------------------------------------


speech_topic_prob <- pm_speech_final
speech_topic_prob <- dplyr::left_join(speech_topic_prob,theta_df,by=c("link" = "document"))
stp1 <- filter(speech_topic_prob, topic %in% c(1,2,3,4,5))
test <- stp1 %>%
  mutate(month = yearmonth(as.Date(date, "%d/%m/%Y"))) %>%
  group_by(month, topic) %>%
  mutate(value = quantile(value, 0.5)) %>% 
  ungroup() %>% 
  arrange(month)
test2 <- stp1 %>%
  mutate(month = yearmonth(as.Date(date, "%d/%m/%Y"))) %>%
  group_by(month, topic) %>%
  arrange(desc(value)) %>%
  filter(row_number() %in% c(1,2,3)) %>% 
  arrange(month) %>% 
  select(-value)
test3 <- dplyr::left_join(test2,test)
plotly::ggplotly(test3 %>% ggplot(aes(x = month, y = value, color = topic)) +
                   geom_line() +
                   facet_wrap(~topic))
