library(tidyverse)
library(tidytext)
library(SnowballC)
library(tm)
#library(spacyr)
library(textstem)
library(proustr)


pm_speech_final <- read_csv("data/2018-09-24-morrison.csv")

# Tokenization + basic stop word removal
tidy_speech <- pm_speech_final %>%
    #mutate(line = row_number()) %>%
    pr_normalize_punc(document)  %>%
    unnest_tokens(word, document, strip_numeric = TRUE) %>% 
    anti_join(tidytext::stop_words) %>%
    count(link, word, sort = TRUE)

# Custom stopwords
filter_words <- tidy_speech %>%
            group_by(link) %>%
            slice_max(order_by = n, n = 3) %>%
            arrange(desc(n)) %>%
            filter(word %in% c("prime", "minister", "host"))

# Filter custom stopwords
tidy_speech <- tidy_speech %>% 
            anti_join(filter_words)

 
write_csv(tidy_speech, "tidy_speech.csv")






# Remove numbers
nums <- tidy_speech %>%
            filter(str_detect(word, "^[0-9]")) %>%
            select(word) %>%
            unique()
tidy_speech <- tidy_speech  %>% 
    anti_join(nums, by = "word")

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
tidy_speech <- pm_speech_final %>%
    mutate(line = row_number()) %>%
    unnest_tokens(word, document) %>%
    anti_join(stop_words) %>%
    mutate(stem = wordStem(word)) %>%
    count(link, word, stem, sort = TRUE)




# stemCompletion(tidy_speech$stem, tidy_speech$word)
pm_speech_final  %>% 
    mutate(word = spacy_parse(document, pos = FALSE, entity = FALSE))