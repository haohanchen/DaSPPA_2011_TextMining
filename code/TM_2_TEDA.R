# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Text Mining Booster
# Part I: Text Data Exploratory Data Analysis
# Input: Tidied Text
# Output: Exploratory Data Analysis
# Author: Haohan Chen
# Last update: 11/15/2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())

# Load packages ----

library(tidyverse)
library(tidytext)

## The packages we use for Stemming
library(SnowballC)
library(hunspell)

## The package we use for making word clouds
library(ggwordcloud)
# Github page: https://github.com/lepennec/ggwordcloud
# Tutorial: https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

## A package supporting the function "str_view_all"
library(htmlwidgets)

dir.create("figures")

# Load cleaned text data ----

document_raw <- readRDS("data/document_raw.rds")
document_tokenized <- readRDS("data/document_tokenized.rds")
dictionary <- readRDS("data/dictionary.rds")
word_count <- readRDS("data/word_count.rds")
word_tfidf <- readRDS("data/word_tfidf.rds")

## Get a dataset of document IDs and metadata
## What's metadata? Any information associated with the document beyond the text

## Consider "book" and "chapter" two CATEGORICAL variables associated with the text
document_meta <- document_raw %>% select(doc_id, book, chapter)

# Merge word counts and tfidf with document metadata

word_count <- document_meta %>% inner_join(word_count, by = "doc_id")
word_tfidf <- document_meta %>% inner_join(word_tfidf, by = "doc_id")


# TEDA: Describe the Whole Corpus ----

## Word cloud (count) ----

sum_corpus <- word_count %>% 
  group_by(word) %>% 
  count() %>% 
  ungroup() %>% 
  arrange(desc(n))

sum_corpus %>% 
  slice_max(n, n = 100) %>%
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(0, 6), limits = c(0, NA)) +
  scale_color_gradient(low = "grey", high = "red") +
  # Replace with your favorite color scheme: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
  theme_minimal() +
  labs(
    title = "Word Cloud of the Corpus",
    subtitle = "Most Frequent Words in Jane Austen's books"
  )

ggsave("figures/teda_describe_corpus.pdf", width = 5, height = 4)


#### Note: Try the whole thing with TF-IDF yourself.

# TEDA: Describe/Compare documents along meta data ----

sum_corpus_sub <- word_count %>%
  filter(book == "Sense & Sensibility") %>%
  ungroup() %>%
  group_by(word) %>%
  summarise(n = sum(n))

sum_corpus_sub %>% 
  slice_max(n, n = 100) %>%
  ggplot(aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(2, 8), limits = c(0, NA)) +
  scale_color_gradient(low = "grey", high = "red") +
  labs(
    title = "Most Frequent Words",
    subtitle = "Word Cloud of the Sense & Sensibility Book"
  ) +
  theme_minimal()

ggsave("figures/teda_describe_subset.pdf", width = 5, height = 4)

#### Note: Try the whole thing with TF-IDF yourself.

## Compare documents grouped by categorical variable(s) ----

unique(word_count$book) # Check what books are there

sum_corpus_sub <- word_count %>%
  filter(book %in% c("Sense & Sensibility", "Pride & Prejudice", "Emma")) %>%
  group_by(book, word) %>%
  summarise(n = sum(n))

### By one categorical variable ----

sum_corpus_sub %>% 
  slice_max(n, n = 50) %>%
  ggplot(aes(label = word, size = n, color = book, x = book)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(2, 8), limits = c(0, NA)) +
  theme_minimal()

ggsave("figures/teda_compare_onecat_1.pdf", width = 6, height = 4)


#### Note: Try the whole thing with TF-IDF yourself.

sum_corpus_sub %>% 
  slice_max(n, n = 50) %>%
  ggplot(aes(label = word, size = n, color = book)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(2, 8), limits = c(0, NA)) +
  theme_minimal() +
  facet_grid(~book)

ggsave("figures/teda_compare_onecat_2.pdf", width = 6, height = 4)

#### Note: Try the whole thing with TF-IDF yourself.

### By two categorical variables ----

word_count %>%
  filter(book %in% c("Sense & Sensibility", "Pride & Prejudice", "Emma")) %>%
  group_by(book, chapter) %>%
  filter(chapter %in% 1:3) %>% 
  # Compare only the first 3 chapters 
  # (too many clouds overload the figure and makes it hard to interpret. be selective.)
  slice_max(n, n = 40) %>%
  ggplot(aes(label = word, size = n, color = book)) +
  geom_text_wordcloud(shape = "circle") +
  scale_radius(range = c(0.3, 5), limits = c(0, NA)) +
  theme_minimal() +
  facet_grid(chapter~book)

ggsave("figures/teda_compare_twocat.pdf", width = 6, height = 4)

#### Note: Try the whole thing with TF-IDF yourself.


## Special case: Compare documents belonging to two categories ----

### Measure: Log odds ratio. See: https://www.tidytextmining.com/twitter.html
### Visualize positive an negative
### Case: Compare Sense & Sensibility and Pride & Prejudice

### Calcualte Log Odds Ratio between two SETs of document
### Note: Treat "book" here as just a categorical variable. It could be any categorical
### variable in the meta data (e.g., speaker, )

sum_lor <- word_count %>%
  filter(book %in% c("Sense & Sensibility", "Pride & Prejudice")) %>%
  # Count number of words in each book
  group_by(book, word) %>%
  summarise(n = sum(n)) %>%
  # !!! Divide count by total number of words per book
  group_by(book) %>%
  mutate(n = (n + 1) / (sum(n) + 1)) %>% # !! Why plus one? This "smooth" the data a bit.
  # Reshape the table to make them side by side (prepare for the calculation of log odds ratio)
  pivot_wider(names_from = "book", values_from = "n") %>%
  # Calculate log odds ratio
  mutate(log_odds_ratio = log(`Sense & Sensibility` / `Pride & Prejudice`)) %>%
  # Select only words and log_odds_ratio variables.
  select(word, log_odds_ratio)

### Interpret positive and negative log odds ratio

### Largest POSITIVE (+) number == words that are unique to "Sense & Sensibility" compared to "Pride & Prejudice"
### Largest NEGATIVE (-) number == words that are unique to "Pride & Prejudice" compared to "Sense & Sensibility" 

### You can scroll through the table -- even export it to csv and open it with EXCEL to take a closer look
write_csv(sum_lor, "figures/teda_special_lor.csv")

### But normally comparing the top POSITIVE and top NEGATIVE words suffices.
### By analyzing these two gorups, you understand what is the BIGGEST distinction
### between the two sets of documents.
### Visualization: Word cloud again (OK... I take back my words about my dislike of word clouds :P)

sum_lor_top <- sum_lor %>% slice_max(log_odds_ratio, n = 100) %>% 
  mutate(type = "Top OR (Unique to Sense & Sensibility)")

sum_lor_btm <- sum_lor %>% slice_min(log_odds_ratio, n = 100) %>% 
  mutate(log_odds_ratio = abs(log_odds_ratio)) %>%
  mutate(type = "Bottom OR (Unique to Pride & Prejudice)")

# Combine top and bottom log odds ratios and visualize.
sum_lor_top %>% 
  bind_rows(sum_lor_btm) %>%
  ggplot(aes(label = word, color = type, x = type, size = log_odds_ratio)) +
  geom_text_wordcloud() +
  scale_radius(range = c(0, 7), limits = c(0, NA)) +
  labs(
    title = "What Words Distinguish the Two Books?",
    subtitle = "Log Odds Ratio: Sense & Sensibility vs. Pride & Prejudice"
  ) +
  theme_minimal()  +
  theme(axis.title.x = element_blank()) # Remove the ugly "type" axis title.

ggsave("figures/teda_special_lor_topbtm_1.pdf", width = 6, height = 4)

sum_lor_top %>% 
  bind_rows(sum_lor_btm) %>%
  ggplot(aes(label = word, color = type, size = log_odds_ratio)) +
  geom_text_wordcloud() +
  scale_radius(range = c(0, 7), limits = c(0, NA)) +
  labs(
    title = "What Words Distinguish the Two Books?",
    subtitle = "Log Odds Ratio: Sense & Sensibility vs. Pride & Prejudice"
  ) +
  theme_minimal()  +
  facet_wrap(~type) # Remove the ugly "type" axis title.

ggsave("figures/teda_special_lor_topbtm_2.pdf", width = 6, height = 4)


### Extension: You may try different pairs, as defined by book names or chapter numbers
### For other applications, you can use any categorical variable to perform this analysis
### For example: 
### Censored vs. non-censored social media posts. Anonymous vs non-anonymous social media posts


