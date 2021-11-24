# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Text Mining
# Part 3: Information Extraction - Topic Modeling
# Input: Tidy Text
# Output: Topic model, examples, and visualizations
# Author: Haohan Chen
# Last update: 11/22/2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())

# Load general-purpose packages ----

library(tidyverse)
library(tidytext)
library(ggwordcloud)

## For convenience, set the default theme as theme_minimal()
ggplot2::theme_set(theme_minimal()) 


# Load packages for topic modeling ----

library(tm)
library(topicmodels)
library(reshape2)


# Load cleaned text data ----

dictionary <- readRDS("data/dictionary.rds")
word_count <- readRDS("data/word_count.rds")
# word_tfidf <- readRDS("data/word_tfidf.rds")

# Construct document-term matrix ----

dtm <- word_count %>% cast_dtm(doc_id, word, n)

# What is is? It's just a "wide" version of the word count
# What is a document-term matrix: https://en.wikipedia.org/wiki/Document-term_matrix
dim(dtm)


# Fit Topic Models ----

# Set number of topics
K <- 20

# Set random number generator seed
set.seed(1122)

# compute the LDA model, inference via 1000 iterations of Gibbs sampling
m_tm <- LDA(dtm, K, method="Gibbs", control=list(iter = 500, verbose = 25))

saveRDS(m_tm, "data_output/model_topicmodel.rds")

# Retrieve results from topic models ----

## beta: How words map to topics
sum_tm_beta <- tidy(m_tm, matrix = "beta")

## gamma: How documents map on topics
sum_tm_gamma <- tidy(m_tm, matrix = "gamma") %>%
  rename("doc_id" = "document") %>%
  mutate(doc_id = as.integer(doc_id)) 
  # Whether you transform doc_id to integer depends on its original type

# Topics EDA ----

sum_tm_gamma %>%
  ggplot(aes(x = gamma)) +
  geom_density() +
  facet_wrap(~topic) +
  labs(
    title = "Topic Modeling Descriptive Statistics",
    subtitle = "Distribution of gamma"
  )

# Examine topics for interpretation ----

## 1. Interpret topic meanings using top words ----

### Get top words associated with topics ----

TOP_N_WORD <- 10

topic_top_word <- sum_tm_beta %>%
  rename("word" = "term") %>%
  group_by(topic) %>%
  slice_max(beta, n = TOP_N_WORD) %>%
  arrange(topic, desc(beta))

write_csv(topic_top_word, "data_output/tm_topic_top_word.csv")

### Visualization 1: Topics in bar charts ----

topic_top_word %>%
  mutate(word = reorder_within(word, beta, topic)) %>%
  ggplot(aes(y = word, x = beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~topic, scales = "free_y") +
  scale_y_reordered() + # Very interesting function. Use with reorder_within
  labs(
    title = "Topic Modeling",
    subtitle = "Top words associated with each topic"
  )
  
ggsave("figures/ie_tm_beta_bc.pdf", width = 10, height = 8)

### Visualization 2: Topics in word cloud ----

pdf("figures/ie_tm_beta_wc.pdf", width = 10, height = 8)
topic_top_word %>%
  ggplot(aes(label = word, size = beta)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 8) + # Tune this number to change the scale
  facet_wrap(~factor(topic)) +
  labs(
   title = "Topic Modeling: Top words associated with each topic"
  ) +
  theme_minimal()
dev.off()

ggsave("figures/ie_tm_beta_wc.pdf", width = 10, height = 8)


## 2. Validate topic meanings using example documents ----

TOP_N_DOC = 10

### Load raw text to get the original text
document_raw <- readRDS("data/document_raw.rds")

### Output top documents associated with each topic to an Excel for manual examination

topic_top_doc <- sum_tm_gamma %>%
  group_by(topic) %>%
  slice_max(gamma, n = TOP_N_DOC) %>%
  arrange(desc(gamma)) %>%
  ungroup() %>%
  inner_join(document_raw, by = "doc_id") %>%
  arrange(topic, desc(gamma))

write_csv(topic_top_doc, "data_output/tm_topic_top_doc.csv")



## 3. Assign names to topics of interest ----

topic_name <- tribble(
  ~ topic, ~ topic_name, ~ topic_description,
  3, "Feelings", "Various types of sentiment",
  6, "Feelings", "Various types of sentiment",
  14, "People in P&P", "Looks like it's about people in this book",
  18, "Family", "Some family members"
)

topic_name

### This part is "art." You need to use your domain knowledge to "name" topics.
### You need not interpret every topic. 
### A lot of them may be irrelevant to your interest.
### You may also merge multiple topics into one.

# Output topic modeling results ----

## Produce visualization for *selected* topics of interest ----

### Visualization 1: Topics in bar charts ----

topic_top_word %>%
  inner_join(topic_name, by = "topic") %>% # FILTER HERE
  mutate(word = reorder_within(word, beta, topic)) %>%
  ggplot(aes(y = word, x = beta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~topic+topic_name, scales = "free_y") +
  scale_y_reordered() + # Very interesting function. Use with reorder_within
  labs(
    title = "Topic Modeling",
    subtitle = "Top words associated with selected topic"
  ) +
  theme_minimal()

ggsave("figures/ie_tm_beta_bc_s.pdf", width = 5, height = 4)

### Visualization 2: Topics in word cloud ----

topic_top_word %>%
  inner_join(topic_name, by = "topic") %>% # FILTER HERE
  ggplot(aes(label = word, size = beta)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 8) +
  facet_wrap(~factor(topic)) +
  theme_minimal() +
  labs(title = "Topic Modeling: Top words associated with selected topics")

ggsave("figures/ie_tm_beta_wc_s.pdf", width = 5, height = 4)


# Save documents' topic belonging (gamma) for downstream analysis ----

tm_out <- sum_tm_gamma %>%
  inner_join(topic_name, by = "topic") %>%
  group_by(doc_id, topic_name) %>%
  summarise(gamma = sum(gamma)) %>% # Merge different topics assigned with the same label
  pivot_wider(names_from = "topic_name", values_from = "gamma", names_prefix = "tm_")

write_csv(tm_out, "data_output/tm_output.csv")
