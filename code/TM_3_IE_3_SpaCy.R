#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Perform other Natural Language Processing analyses using SpaCy
# Input: Raw documents
# Output: Tokenized text with annotated features
#         lemma, named entities, part-of-speech tagging etc.
# Author: Haohan Chen
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())
# .rs.restartR()

# Load general-purpose packages ----

library(tidyverse)
library(tidytext)

# Load text mining package ----

# install.packages("spacyr")
library(spacyr)

# # Run the code below for first run
# # Before you do, set memory to 3G... Otherwise it will fail.
# # This is a complicated package consuming LOTS of computing resources...
# # That's why we did not start with it (and you don't need it in may occassions)
# spacy_install()

## https://spacy.io/universe/project/spacyr
## https://spacyr.quanteda.io/articles/using_spacyr.html

## Reminder: If you want to install this in your local machine:
## Intstalling this package might not be entirely straightforward
## Just install all packages (including miniconda) the system ask you to install.
## Look at the messages in the console

## SpaCy support the Chinese language as well.
### https://spacy.io/models/zh

### use spacy_download_langmodel() to download models you need
### Use with caution. Check if the models fit the type of text you work on

# Initialize SpaCy

spacy_initialize()


# Load documents ----

document_raw <- readRDS("data/document_raw.rds")

# Move text data into a vector
texts_raw <- document_raw$text
# Name elements of the vector so that spacyr can recognize document IDs
names(texts_raw) <- document_raw$doc_id

rm(document_raw)

# Remove the dataset of raw document from space

# Parse text (sample) ----

texts_parsed_sample <- texts_raw[1:10] %>%
  spacy_parse(
    lemma = TRUE,
    entity = TRUE,
    pos = TRUE,
    tag = TRUE
  ) %>%
  as_tibble()

texts_parsed_sample

## An overview of what you get: https://spacy.io/usage/linguistic-features

## What do "tag" mean: https://www.ling.upenn.edu/courses/Fall_2003/ling001/penn_treebank_pos.html
## What do "pos" mean: https://universaldependencies.org/u/pos/


# Parse text (in batch) ----

## To make the workload manageable by the system (instead of crowding the memory)
## you will need to process the data in batch.

BATCH_SIZE = 20

n_batches <- ceiling(length(texts_raw) / BATCH_SIZE)

text_parsed_b <- list()

# Create folder to store interim restuls
dir.create("data_output/spacy_temp/")

for (j in 1:n_batches){
  i_min <- (j - 1) * BATCH_SIZE + 1
  i_max <- min(j * BATCH_SIZE, length(texts_raw))
  
  temp <- texts_raw[i_min:i_max] %>%
    spacy_parse(
      lemma = TRUE,
      entity = TRUE,
      pos = TRUE
    ) %>%
    as_tibble()
  
  saveRDS(temp, sprintf("data_output/spacy_temp/%s.rds", j))
  
  message(j, " of ", n_batches) # Print progress so that you can keep track
}

# Put interim output into a single dataset ----

## Read the stored interim results ----
fname_ls <- list.files("data_output/spacy_temp", full.names = TRUE)

texts_parsed_ls <- list()
for (i in seq_along(fname_ls)){
  texts_parsed_ls[[i]] <- readRDS(fname_ls[i])
  message(i, " of ", length(fname_ls))
}

## Merge into one table ----
texts_parsed_out <- reduce(texts_parsed_ls, bind_rows)

### token_id is useless when you process data in batch
texts_parsed_out <- texts_parsed_out %>% select(-token_id)

## Tidy doc_id ----

### Change doc_id to numeric if applicable
texts_parsed_out <- texts_parsed_out %>% mutate(doc_id = as.integer(doc_id))

# Take a look at the output data ----

texts_parsed_out

# Save results ----

saveRDS(texts_parsed_out, "data/document_spacy_annotate.rds")
