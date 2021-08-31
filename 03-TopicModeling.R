library(tidyverse)
library(tidytext)
library(topicmodels)
library(wordcloud)

tidy_speech <- read_csv("tidy_speech.csv")


speech_dtm <- tidy_speech %>%
  cast_dtm(link, word, n)

speech_lda <- LDA(speech_dtm, k = 10, control = list(seed = 1234))

# Probability that the term was generated from that topic = beta
speech_topics <- tidy(speech_lda, matrix = "beta")
speech_topics

word_freq <- speech_topics %>%
      mutate(n = trunc(beta * 10000)) %>%
      filter(topic == 6)

wordcloud(words = word_freq$term,
          freq = word_freq$n,
          max.words = 50
          )

# Top terms of every topics
top_terms <- speech_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)
top_terms

library(ggplot2)

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


library(textmineR)

custom_stop_words <- tribble(
    ~word, ~lexicon,
    "prime", "CUSTOM",
    "minister", "CUSTOM"
)

pm_speech_final <- pm_speech_final %>%
    pr_normalize_punc(document)

dtm <- CreateDtm(doc_vec = pm_speech_final$document, # character vector of documents
                 doc_names = pm_speech_final$link, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart"), # this is the default value
                                  custom_stop_words),
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 stem_lemma_function = function(x) SnowballC::wordStem(x, "porter"),
                 verbose = FALSE) # Turn off status bar for this demo)
dtm <- dtm[,colSums(dtm) > 2]

set.seed(12345)
model <- FitLdaModel(dtm = dtm,
                     k = 20,
                     iterations = 2000,
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE)

# Get the top terms of each topic
model$top_terms <- GetTopTerms(phi = model$phi, M = 5)

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

?topicmodels::topics()
