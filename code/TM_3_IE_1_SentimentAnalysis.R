# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Text Mining
# Part 3: Information Extraction - Sentiment Analysis
# A more systematic treatment of the method
# Input: Tidy Text
# Output: A table of document-level sentiment scores
# Author: Haohan Chen
# Last update: 11/22/2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())

# Load general-purpose packages ----

library(tidyverse)
library(tidytext)

# Load package for stemming ----
library(SnowballC)

# Load tidy text data from Jane Austen's work ----

document_id <- readRDS("data/document_raw.rds") %>% ungroup() %>% select(doc_id)
dictionary <- readRDS("data/dictionary.rds")
word_count <- readRDS("data/word_count.rds")
word_tfidf <- readRDS("data/word_tfidf.rds")

## Get a dataset of document IDs and metadata

### Calculate document lengths (much needed variable for normalization)
document_len <- word_count %>% group_by(doc_id) %>% summarise(doc_len = n())

### Variables from document_raw
# document_meta <- document_raw %>% select(doc_id, book, chapter)

## Merge word counts and tfidf with document metadata
# word_count <- document_meta %>% inner_join(word_count, by = "doc_id")
# word_tfidf <- document_meta %>% inner_join(word_tfidf, by = "doc_id")


# Load sentiment dictionaries ----

library(textdata)
## This package load the most well-recognized dictionaries for sentiment analysis
## https://github.com/EmilHvitfeldt/textdata

## Use the functions below to see which sentiment dictionary you want to use.
## Friendly reminder: Cite the papers providing these dictionaries.
?lexicon_afinn()
?lexicon_bing()
?lexicon_nrc()
?lexicon_loughran() # Finance-specific
?lexicon_nrc_eil()
?lexicon_nrc_vad()

# Load sentiment dictionaries ----

## Note: If you load them for the first time, you will see a prompt in the console
## asking whether you want to download the datasets. Type "1" and then hit ENTER
## to download. When finished, run the code again.

dic_afinn <- lexicon_afinn()
dic_nrc_eil <- lexicon_nrc_eil()
dic_nrc_vad <- lexicon_nrc_vad()

dic_bing <- lexicon_bing()
dic_loughran <- lexicon_loughran()
dic_nrc <- lexicon_nrc()

## Let's look at them one by one. We can categorize these dictionaries into two types

### dic_afinn -- Score. One-dimensional
### dic_nrc_eil -- Score. Multi-dimensional
### dic_nrc_vad -- Score. Multi-dimensional

### dic_bing -- Categorical. Dichotomous - Positive/Negative
### dic_loughran -- Categorical. Dichotomous - Positive/Negative
### dic_nrc -- Categorical. Multiple class

## Fix the NRC VAD dataset
dic_nrc_vad <- lexicon_nrc_vad()

row_add <- names(dic_nrc_vad)
word <- row_add[1]
val <- as.numeric(row_add[2:4])

dic_nrc_vad <- lexicon_nrc_vad() %>%
  setNames(c("word", "valence", "arousal", "dominance")) %>%
  add_row(word = word, valence = val[2], arousal = val[3], dominance = val[4]) %>%
  pivot_longer(c("valence", "arousal", "dominance"), 
               names_to = "AffectDimension", values_to = "score")


## With my intention to provide you with a comprehensive introduction (more than
## pretty much most online and offline intro materials), I load all the dictionaries
## In your own practice, you absolutely don't have to do these. You may just choose one
## or two approach that fits your data.


# Stemming ----

## Remember that we stem the original text. So we can stem the Lexicon so that
## the two are consistent.
## This is not the only "right approach." It may not be the best approach.
## since words of different forms are considered the same after stemming...
## Note: you may do it in a different way -- don't stem the original text

# Stem sentiment dictionary with polarity scores

dic_afinn <- dic_afinn %>% mutate(word = wordStem(word)) %>% 
  group_by(word) %>% summarise(value = mean(value))

dic_nrc_eil <- dic_nrc_eil %>% mutate(term = wordStem(term)) %>% 
  group_by(term, AffectDimension) %>% summarise(score = mean(score))

dic_nrc_vad <- dic_nrc_vad %>% mutate(word = wordStem(word)) %>% 
  group_by(word, AffectDimension) %>% summarise(score = mean(score))

# Stem categorical sentiment dictionaries

dic_bing <- dic_bing %>% mutate(word = wordStem(word)) %>% 
  distinct()

dic_loughran <- dic_loughran %>% mutate(word = wordStem(word)) %>% 
  distinct()

dic_nrc <- dic_nrc %>% mutate(word = wordStem(word)) %>% 
  distinct()


# Get document-level sentiment with categorical sentiment dictionaries ----

## BING ----

sent_bing_count <- word_count %>%
  inner_join(dic_bing, by = "word") %>%
  group_by(doc_id, sentiment) %>%
  summarise(score = sum(n)) %>%
  pivot_wider(names_from = "sentiment", values_from = "score", 
              values_fill = 0, names_prefix = "sent_bing_count_")

sent_bing_tfidf <- word_tfidf %>%
  inner_join(dic_bing, by = "word") %>%
  group_by(doc_id, sentiment) %>%
  summarise(score = sum(tfidf)) %>%
  pivot_wider(names_from = "sentiment", values_from = "score", 
              values_fill = 0, names_prefix = "sent_bing_tfidf_")


## LOUGHRAN ----

sent_loughran_count <- word_count %>%
  inner_join(dic_loughran, by = "word") %>%
  group_by(doc_id, sentiment) %>%
  summarise(score = sum(n)) %>%
  pivot_wider(names_from = "sentiment", values_from = "score", 
              values_fill = 0, names_prefix = "sent_loughran_count_")

sent_loughran_tfidf <- word_tfidf %>%
  inner_join(dic_loughran, by = "word") %>%
  group_by(doc_id, sentiment) %>%
  summarise(score = sum(tfidf)) %>%
  pivot_wider(names_from = "sentiment", values_from = "score", 
              values_fill = 0, names_prefix = "sent_loughran_tfidf_")


## NRC ----

sent_nrc_count <- word_count %>%
  inner_join(dic_nrc, by = "word") %>%
  group_by(doc_id, sentiment) %>%
  summarise(score = sum(n)) %>%
  pivot_wider(names_from = "sentiment", values_from = "score", 
              values_fill = 0, names_prefix = "sent_nrc_count_")

sent_nrc_tfidf <- word_tfidf %>%
  inner_join(dic_nrc, by = "word") %>%
  group_by(doc_id, sentiment) %>%
  summarise(score = sum(tfidf)) %>%
  pivot_wider(names_from = "sentiment", values_from = "score", 
              values_fill = 0, names_prefix = "sent_nrc_tfidf_")

names(sent_nrc_count)

# Get document-level sentiments with sentiment dictionaries with scores ----

## _count: Scores weighted by count; _tfidf: weighted by TF-IDF

## AFINN ----

sent_afinn_count <- word_count %>%
  inner_join(dic_afinn, by = "word") %>% 
  mutate(value_total = n * value) %>% # Multiple polarity score by word frequency
  group_by(doc_id) %>% # Group by all the meta data
  summarise(sent_afinn_count = sum(value_total))

sent_afinn_tfidf <- word_tfidf %>%
  inner_join(dic_afinn, by = "word") %>% 
  mutate(value_total = tfidf * value) %>% # Multiple polarity score by word frequency
  group_by(doc_id) %>% # Group by all the meta data
  summarise(sent_afinn_tfidf = sum(value_total))


## NRC EIL ----

sent_nrceil_count <- word_count %>%
  inner_join(dic_nrc_eil, by = c("word" = "term")) %>% 
  mutate(value_total = n * score) %>% # Multiple polarity score by word frequency
  group_by(doc_id, AffectDimension) %>% # Group by all the meta data
  summarise(value_sum = sum(value_total)) %>%
  pivot_wider(names_from = "AffectDimension", values_from = "value_sum", 
              values_fill = 0, names_prefix = "sent_nrceil_count_")

sent_nrceil_tfidf <- word_tfidf %>%
  inner_join(dic_nrc_eil, by = c("word" = "term")) %>% 
  mutate(value_total = tfidf * score) %>% # Multiple polarity score by word frequency
  group_by(doc_id, AffectDimension) %>% # Group by all the meta data
  summarise(value_sum = sum(value_total)) %>%
  pivot_wider(names_from = "AffectDimension", values_from = "value_sum", 
              values_fill = 0, names_prefix = "sent_nrceil_tfidf_")

## NRC VAD ----

sent_nrcvad_count <- word_count %>%
  inner_join(dic_nrc_vad, by = "word") %>% 
  mutate(value_total = n * score) %>% # Multiple polarity score by word frequency
  group_by(doc_id, AffectDimension) %>% # Group by all the meta data
  summarise(value_sum = sum(value_total)) %>%
  pivot_wider(names_from = "AffectDimension", values_from = "value_sum", 
              values_fill = 0, names_prefix = "sent_nrcvad_count_")

names(sent_nrcvad_count)

sent_nrcvad_tfidf <- word_tfidf %>%
  inner_join(dic_nrc_vad, by = "word") %>% 
  mutate(value_total = tfidf * score) %>% # Multiple polarity score by word frequency
  group_by(doc_id, AffectDimension) %>% # Group by all the meta data
  summarise(value_sum = sum(value_total)) %>%
  pivot_wider(names_from = "AffectDimension", values_from = "value_sum", 
              values_fill = 0, names_prefix = "sent_nrcvad_tfidf_")



# Put all these sentiment indicators together ----

## Construct an output dataset containing sentiment scores by all different dictionaries

sentiment_all <- list(
  document_id,
  sent_afinn_count, sent_afinn_tfidf,
  sent_bing_count, sent_bing_tfidf,
  sent_loughran_count, sent_loughran_tfidf,
  sent_nrc_count, sent_nrc_tfidf,
  sent_nrceil_count, sent_nrceil_tfidf,
  sent_nrcvad_count, sent_nrcvad_tfidf
  ) %>%
  reduce(left_join, by = "doc_id")

summary(sentiment_all)

## Fill NAs. Sometimes some documents have NO sentiment words at all. They will
## show up as NA's -- no sentiment values. This is not a problem for the Jane Austen
## dataset. But will be a problem for many others (esp. social media short text)
sentiment_all <- sentiment_all %>%
  mutate_at(vars(starts_with("sent_")), ~replace_na(., 0))

names(sentiment_all)

# Create sentiment indicators normalized by document lengths ----

## Divide scores by document lengths

document_len # Dataset of document lengths created before

sentiment_all_n <- sentiment_all %>% 
  inner_join(document_len, by = "doc_id") %>%
  rowwise() %>%
  mutate_at(vars(starts_with("sent")), ~. / doc_len) %>%
  rename_at(vars(starts_with("sent")), ~str_c(., "_n")) %>%
  select(-doc_len)

names(sentiment_all_n)

## Merge the normalized and the un-normalized sentiment scores

sentiment_all_out <- sentiment_all %>%
  inner_join(sentiment_all_n, by = "doc_id")

View(sentiment_all_out)

# Output a dataset of sentiments for downstream analysis ----

write_csv(sentiment_all_out, "data_output/sentiment_analysis.csv")
