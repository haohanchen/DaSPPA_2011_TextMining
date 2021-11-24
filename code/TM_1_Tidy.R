# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Text Mining Booster
# Part I: Text Data Tidying
# Input: Raw text
# Output: Diagnostic EDA and Document-Term Matrices
# Author: Haohan Chen
# Last update: 11/15/2021
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list=ls())

# Load packages ----

library(tidyverse)
library(tidytext)
library(janeaustenr)

## The packages we use for Stemming
library(SnowballC)
library(hunspell)

## The package we use for making word clouds
library(ggwordcloud)
# Github page: https://github.com/lepennec/ggwordcloud
# Tutorial: https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

## A package supporting the function "str_view_all"
library(htmlwidgets)

dir.create("data")

# Load raw text data ----

text_raw_0 <- austen_books() 

# Define your "documents" ----

## A "document" is a piece of text that you consider an "entity" in your text mining
## Documents are your *unit of analysis*
## All documents in your dataset make a "corpus"

## NOTE: What makes a "document" depends on your research questions.
## YOU have the flexibility to define what is a document for you

## In this example, I define each book chapter as a "document."

text_raw <- text_raw_0 %>%
  # # Take a sample for prototyping. You need to do this when your data are large to save time.
  # # Comment out this part when you finalize your code
  # slice(1:5000) %>%
  group_by(book) %>%
  # Identify chapters
  mutate(
    chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", ignore_case = TRUE)))
  ) %>%
  # Remove the parts identified as "chapter zero" those are the book meta data
  filter(!chapter == 0) %>%
  # # Create book-chapter identifier. str_pad -- just for easier sorting. Check what it does yourself.
  # mutate(
  #   book_chapter = str_c(book, " - ", str_pad(chapter, width = 2, side = "left", pad = "0"))
  # ) %>%
  # Combine lines in chapters into one text chunk
  group_by(book, chapter) %>%
  summarise(text = str_c(text, collapse = " ")) %>% # concatenate strings in the same chapter
  # Final clean-ups 1: Remove the procedural text "CHAPTER XX"
  mutate(text = str_replace(text, regex("^chapter [\\divxlc]+", ignore_case = TRUE), "")) %>%
  # Final clean-ups 2: Remove leading space and consecutive spaces
  mutate(text = str_replace_all(text, "^ +", "")) %>% # Remove leading spaces
  mutate(text = str_replace_all(text, " +", " ")) # Replaces multiple space by one space

# Finally, make UNIQUE document IDs 
# (Make sure that you have a UNIQUE identifier for each document)

text_raw <- text_raw %>% 
  ungroup() %>% # To be safe, ungroup before you generate any ID based on row_number.
  # This is because row_number is the row number in each GROUP. If the tibble is 
  # grouped by some column, the IDs will not be unique.
  mutate(doc_id = row_number()) %>% # Generate a column doc_id based on the row number
  select(doc_id, everything()) %>% # Put the new column doc_id in front of all other columns
  group_by(doc_id, book, chapter) %>%
  select(doc_id, text, everything())
  # Group by these columns to tell tidytext the three columns are ID/meta data and 
  # should be kept in the unnested dataset
  
## Recommendation: Save the cleaned raw text
saveRDS(text_raw, "data/document_raw.rds")
### This is a format readbable by only R. It is preferred because it saves data efficiently
### i.e. takes little space, read and write quickly in R

### How to load the saved data?
text_raw <- readRDS("data/document_raw.rds")

names(text_raw)


# Pre-tokenization summary statistics ----
# Probably only two

## How many documents are in your dataset?
sum_ndocs <- nrow(text_raw)

sum_ndocs

## Number of characters in each document

sum_nchar <- text_raw %>%
  transmute( # What does transmute do? Look it up yourself. Very similar to mutate :)
    n_characters = nchar(text)
    )

summary(sum_nchar)

### Plot the number of characters
sum_nchar %>%
  ggplot() + geom_histogram(aes(x = n_characters), bins = 40) +
  xlab("Number of characters") +
  labs(
    title = "Distribution of Document Lengths",
    subtitle = str_c("Number of documents = ", sum_ndocs)
  )



# Tokenization ----

text_tokenized <- text_raw %>%
  unnest_tokens(word, text, token = "words")

## Tokenization can be much more complicated. tidytext does the dirty job for us
## The less formal the text is (e.g., social media text), the harder the tokenization is

## For texts that do not naturally group characters into words (e.g., Chinese),
## tokenization is require extra steps.


# Diagnostic EDA of pre-wrangled tokenized text ----

## Count word frequencies ----
## This calcualtes how many word appear in each of the DOCUMENT (check what document means)

text_count <- text_tokenized %>%
  group_by(doc_id, word) %>%
  count()

text_count

## Construct a dictionary ----
## It gets a table of UNIQUE words appearing in the CORPUS (check what corpus means)

dictionary <- text_count %>%
  group_by(word) %>%
  summarise(
    term_frequency = sum(n), 
    document_frequency = n())

### Term frequency: How many times a word appear throughout all documents
### Document frequency: In how many documents a word appear
### Not the difference between the two. Consider this case: a word that appear 1000 times in 
### one document but it only appears in this document and nowhere else.
### Then its Term Frequency = 1000 (quite frequent), but Document Frequency = 1 (very infrequent)


## Get summary statistics of your pre-wrangling dictionary  ----
summary(dictionary$term_frequency)
summary(dictionary$document_frequency)

p_tf_1 <- dictionary %>%
  ggplot() + geom_histogram(aes(x = term_frequency), bins = 40) +
  xlab("Term Frequency") + 
  labs(
    title = "Distribution of Post-Wrangling Term Frequencies",
    subtitle = "Histogram"
  )

p_tf_1

p_df_1 <- dictionary %>%
  ggplot() + geom_histogram(aes(x = document_frequency), bins = 40) +
  xlab("Term Frequency") + 
  labs(
    title = "Distribution of Post-Wrangling Document Frequencies",
    subtitle = "Histogram"
  )

p_df_1


### Diagnostics 1: Pre-wrangled most frequent words ----
dictionary %>% 
  slice_max(term_frequency, n = 100) %>% 
  arrange(desc(term_frequency)) # Sort the rows by term frequency

dictionary %>% slice_max(term_frequency, n = 200) %>%
  ggplot(aes(label = word, size = term_frequency)) +
  geom_text_wordcloud() +
  labs(
    title = "Most Frequent Words (Pre-Wrangled)"
  ) +
  theme_minimal()

# OK... I have to use thie "with" function to make my code cleaner.
# Check what "with" does. It basically tells R that everything it referss to is within
# the object you specify at the beginning

### Diagnostics 2: Pre-wrangled least frequent words ----
dictionary %>% 
  slice_min(term_frequency, n = 1) %>% 
  arrange(term_frequency) %>% # Sort the rows by term frequency
  View()

dictionary %>% slice_min(term_frequency, n = 1) %>%
  slice(1:200) %>%
  ggplot(aes(label = word, size = term_frequency)) +
  geom_text_wordcloud() +
  scale_radius(range = c(1, 1), limits = c(0, NA)) +
  labs(
    title = "Least Frequent Words (Pre-Wrangled)"
  ) +
  theme_minimal()


## Most frequent words look fine -- only need to remove stop words

# Text data wrangling ----

## Tip 1: Do not touch the raw text data text_raw
## Tip 2: Create a new container of the cleaned tokenized text text_tokenized_t
## Tip 3: Make good use of regular expression. Refer to the cheatsheet
## Tip 4: Print out what you are removing before you remove.
## Tip 5: Document VERY CLEARLY what you do (for collaborators and your future self.)

## Two types of text wrangling
## Document-specific wrangling -- first thing to do
## Standardized wrangling -- second thing to do

## Create container of the post-wrangled tokenized text ----
## Copy text_tokenized as a start.
text_tokenized_t <- text_tokenized


## CORPUS-SPECIFIC WRANGLING ----

### Learning from the EDA above. This could be an iterative process
### Don't expect to get all things done in one pass

### Remove words that contain the formatting symbol _ ----

#### Something weired in the least frequent words? 
#### Yes. Check those words starting and ending with _
#### Start with the tokenized words

check_anomaly <- text_tokenized %>%
  filter(str_detect(word, "^_") | str_detect(word, "_$"))

nrow(check_anomaly) # 1415 of these words
unique(check_anomaly$book)
check_anomaly

#### Look at these anomaly in the raw text data

# Check Regular expression cheatsheet if you don't understand what the code below does
# https://evoldyn.gitlab.io/evomics-2018/ref-sheets/R_strings.pdf
with(
  text_raw_0 %>% filter(book == "Emma"),
  str_view_all(text, "_[A-Za-z']+_", match = TRUE)
)

##### Note: I think the leading and ending _ are for formatting. Italic font for emphasis.
##### You may come across documents using all these different weird formattings symbols.
##### Solution: Remove these leading and ending _'s

text_tokenized %>%
  filter(str_detect(word, "_")) %>%
  filter(!str_detect(word, "_[A-Za-z']+_"))

text_tokenized_t <- text_tokenized %>%
  mutate(
    word = ifelse(
      str_detect(word, "_[A-Za-z']+_") | # Both leading and ending _
        str_detect(word, "_[A-Za-z']+") | # Only leading _
        str_detect(word, "[A-Za-z']+_"),  # Only ending _
      str_replace_all(word, "_", ""), word))

# Check
text_tokenized_t %>%
  filter(str_detect(word, "_"))



### Remove tokens that contain numbers ----
text_tokenized %>%
  filter(str_detect(word, "[:digit:]"))

text_tokenized_t <- text_tokenized_t %>%
  filter(!str_detect(word, "[:digit:]"))

### Remove "'s" in the end of words ----
text_tokenized %>%
  filter(str_detect(word, "'s$"))

text_tokenized_t <- text_tokenized_t %>%
  mutate(
    word = ifelse(str_detect(word, "'s$"), str_replace(word, "'s$", ""), word)
  )


### Remove a customized set of words ----

## Remove whatever words YOU think are appropriate to remove
stop_words_customized <- tibble(
  word = c("MISS")
)

text_tokenized %>%
  semi_join(stop_words_customized, by = "word")

text_tokenized_t <- text_tokenized_t %>%
  anti_join(stop_words_customized, by = "word")



## STANDARDIZED CLEANING ----

### Remove stop words ----
stop_words <- stop_words %>% select(word) %>% distinct()

### There are many of them
text_tokenized %>%
  semi_join(stop_words, by = "word")

text_tokenized_t <- text_tokenized_t %>%
  anti_join(stop_words, by = "word")

text_tokenized_t %>%
  semi_join(stop_words, by = "word")


### Stemming ----
### Harmonize different forms of words

#### Example 1
stem_example <- c("love", "loved", "loves")
# Two methods for stemming. The first is more straightfoward
wordStem(stem_example)

hunspell_stem(stem_example)
sapply(hunspell_stem(stem_example), function(x) tail(x, 1)) # Choose the last form identified

#### Example 2
stem_example <- c("apple", "apples")
wordStem(stem_example)
sapply(hunspell_stem(stem_example), function(x) tail(x, 1))

#### Example 2
stem_example <- c("story", "stories")
wordStem(stem_example)
sapply(hunspell_stem(stem_example), function(x) tail(x, 1))

#### Example 3
stem_example <- c("your", "yours")
wordStem(stem_example)
sapply(hunspell_stem(stem_example), function(x) tail(x, 1))


### Example 4: Limitation (because it is rule based)
stem_example <- c("take", "took", "taken")
wordStem(stem_example)
sapply(hunspell_stem(stem_example), function(x) tail(x, 1))

### Let's use the wordStem function (implementing the "Poter Stemmer" is the most widely used)
### But feel free to use the hunspell_stem function. Although it's a little more complicated
text_tokenized_t <- text_tokenized_t %>%
  mutate(word = wordStem(word))

### Note: Stemmed words may not be real English words. For example -- "stori"

### Curious about how stemming works? Take a look at this. WARNING: Complicated
### http://snowball.tartarus.org/algorithms/porter/stemmer.html
### Want to know about implementation: https://smltar.com/stemming.html#how-to-stem-text-in-r



### Prune infrequent words ----

### If a word appear very infrequently in your data, then it might not be of interest
### for text mining.
### But HOW INFREQUENT is infrequent? Your call. My rule of thumb is 5% of the documents
### But it is subject to change based on the size of your data. 
### You can define in ways that make sense to you. i.e. 5% of the documents, 100 documents, etc.
### In general, the larger your data, the higher threshold you can afford.

## Re-make dictionary
text_count_t <- text_tokenized_t %>%
  group_by(doc_id, word) %>%
  count()

dictionary_t <- text_count_t %>%
  group_by(word) %>%
  summarise(
    term_frequency = sum(n), 
    document_frequency = n())
### Why re-make the dictionary? You want to base your decision on the post-wrangled text
### data by now. This should be the LAST task of your text wrangling.

### Calcualte your threshold
n_docs <- nrow(text_raw)
threshold <- round(n_docs * 0.05, 0)

### Get a dictionary of the words to remove.
### These are not really "stop words." I abuse the name "stop words" a bit here
### so that these datasets appear together in the "Environment" (listing objects in alphabetical order)
stop_words_infrequent <- dictionary_t %>% filter(document_frequency < threshold)

text_tokenized_t <- text_tokenized_t %>%
  anti_join(stop_words_infrequent, by = "word")

text_tokenized_t %>%
  semi_join(stop_words_infrequent, by = "word")


## Finalized dictionary ----
text_count_t <- text_tokenized_t %>%
  group_by(doc_id, word) %>%
  count()

dictionary_t <- text_count_t %>%
  group_by(word) %>%
  summarise(
    term_frequency = sum(n), 
    document_frequency = n())

## Get summary statistics of your post-wrangling dictionary  ----

summary(dictionary_t$term_frequency)
summary(dictionary_t$document_frequency)

p_tf_2 <- dictionary_t %>%
  ggplot() + geom_histogram(aes(x = term_frequency), bins = 40) +
  xlab("Term Frequency") + 
  labs(
    title = "Distribution of Post-Wrangling Term Frequencies",
    subtitle = "Histogram"
  )

p_tf_1
p_tf_2

p_df_2 <- dictionary_t %>%
  ggplot() + geom_histogram(aes(x = document_frequency), bins = 40) +
  xlab("Term Frequency") + 
  labs(
    title = "Distribution of Post-Wrangling Document Frequencies",
    subtitle = "Histogram"
  )

p_df_1
p_df_2


### Diagnostics 3: Post-wrangled most frequent words ----
dictionary_t %>% 
  slice_max(term_frequency, n = 100) %>% 
  arrange(desc(term_frequency)) # Sort the rows by term frequency

dictionary_t %>% slice_max(term_frequency, n = 200) %>%
  ggplot(aes(label = word, size = term_frequency)) +
  geom_text_wordcloud() +
  theme_minimal() +
  labs(
    title = "Most Frequent Words (Post-Wrangled)"
  )


### Diagnostics 4: Post-wrangled least frequent words ----
dictionary_t %>% 
  slice_min(term_frequency, n = 1) %>% 
  arrange(term_frequency) %>% # Sort the rows by term frequency
  View()

dictionary_t %>% slice_min(term_frequency, n = 200) %>%
  ggplot(aes(label = word, size = term_frequency)) +
  geom_text_wordcloud() +
  scale_radius(range = c(0, 3), limits = c(0, NA)) +
  theme_minimal() +
  labs(
    title = "Least Frequent Words (Post-Wrangled)"
  )


### Note: In this step, if you still spot any words in the list check_tf_top or check_tf_bottom
### (especially the former) that you feel necessary to remove, add the word to stop_words_customized 
### and re-run the whole code in the WHOLE "text wrangling" section.
### You can tune it as many time as you want until you are happy with the finalized 
### dictionary


# Calculate TF-IDF ----

# Definition of TF-IDF quick ref: https://en.wikipedia.org/wiki/Tf%E2%80%93idf

text_count_t
dictionary_t

text_count_t_tfidf <- text_count_t %>%
  inner_join(dictionary_t, by = "word") %>%
  mutate(
    tf = n / term_frequency,
    idf = log(n_docs / document_frequency)
  ) %>%
  mutate(tfidf = tf * idf) %>%
  select(doc_id, word, tfidf)


# Save post-wrangled dataset and move on to the next task ----

## Text datasets are HUGE. Keeping all in your memory can break your R session
## When you are done with wrangling, you should save the results and open a new
## session to conduct your analysis. Of course, you may need to come back to 
## redo the wrangling from time to time. But it is still good practice to separate
## wrangling of raw text from data analysis

saveRDS(text_tokenized_t, "data/document_tokenized.rds")
saveRDS(dictionary_t, "data/dictionary.rds")
saveRDS(text_count_t, "data/word_count.rds")
saveRDS(text_count_t_tfidf, "data/word_tfidf.rds")

## DTM is short for Document Term Matrix. 
## Go to the Wikipedia page: https://en.wikipedia.org/wiki/Document-term_matrix
## See the example under "General Concept" to understand what a DTM is.

## These two datasets are equivalent to Document Term Matrices, only in the long form. 

