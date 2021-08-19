library(tidyverse)
library(tidytext)
library(topicmodels)

tidy_speech <- read_csv("tidy_speech.csv")


speech_dtm <- tidy_speech %>%
  cast_dtm(link, word, n)

speech_lda <- LDA(speech_dtm, k = 4, control = list(seed = 1234))

# Probability that the term was generated from that topic = beta
speech_topics <- tidy(speech_lda, matrix = "beta")
speech_topics

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
