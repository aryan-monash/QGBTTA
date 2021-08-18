library(tidyverse)
library(tidytext)
library(SnowballC)
library(tm)
library(spacyr)
library(textstem)

morrison <- load("data/2018-09-24-morrison.rda")

# Prime minister might be an important word,
# maybe remove the most frequent words
custom_stop_words <- tribble(
    ~word, ~lexicon,
    "prime", "CUSTOM",
    "minister", "CUSTOM"
)

stop_words2 <- stop_words  %>%
    bind_rows(custom_stop_words)

# Tokenization + stopword removal + stemming
tidy_speech_2 <- pm_speech_final %>%
    mutate(line = row_number()) %>%
    unnest_tokens(word, document) %>%
    anti_join(stop_words) %>%
    mutate(stem = wordStem(word)) %>%
    count(link, word, stem, sort = TRUE)

# Remove numbers
nums <- tidy_speech %>% filter(str_detect(word, "^[0-9]")) %>% select(word) %>% unique()
tidy_speech <- tidy_speech  %>% 
    anti_join(nums, by = "word")


filter(tidy_speech, link == "/media/interview-waleed-aly-project")

stemCompletion(tidy_speech$stem, tidy_speech$word)

tidy_speech2 <- tidy_speech  %>% count(link, stem, sort = TRUE)


pm_speech_final  %>% 
    mutate(word = spacy_parse(document, pos = FALSE, entity = FALSE))

write_csv(tidy_speech_2, "tidy_speech_2.csv")


test <- tidy_speech_2 %>% group_by(link) %>% slice_max(order_by = n, n = 3)  %>% arrange(desc(n))
write_csv(test, "names.csv")

