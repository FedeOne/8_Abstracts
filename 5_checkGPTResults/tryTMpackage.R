
setwd("C:/Users/Federico/Desktop/8_Abstracts/5_checkGPTResults")
library(tm)
library(tidytext)
library(dplyr)

data <- readRDS("C:/Users/Federico/Desktop/8_Abstracts/4_FindWithGPT/fullResponsesCKDIronV1percentages.rds")
library(tidytext)
library(SnowballC)
library(caret)
library(stringr)


data2 <- data %>% 
  mutate(exposureGPT = str_extract(responses, "^[^;]*"),  # any character except semicolon, zero or more times
         responses2 = str_remove(responses, paste0(exposureGPT, ";") ),
         exposureGPT = str_remove(exposureGPT, ".*:"),
         
         measureType = str_extract(responses2, "^[^;]*"),
         responses3 = str_remove(responses2, paste0(measureType, ";") ),
         
         measureUnit = str_extract(responses3, "^[^;]*"),
         responses4 = str_remove(responses3, paste0(measureUnit, ";") ),
         
         populationGPT = str_extract(responses4, "^[^;]*"),
         confintGPT = str_extract(responses4, "\\[.*\\]"),
         populationGPT= str_remove(populationGPT, ".*:"),
         
           ) %>% 
  select(-c(responses2, responses3, responses4))
         

check <- data2 %>% filter(PMID =="21272704") %>% 
  select(fullPhrase, keyword, contains("GPT"))

#### It's not too bad, need to exclude when the percentage is related to an increase of the frequency ,

## need to take into account when the frequency is part of a range

# corpus <- Corpus(VectorSource(data$abstract))
# 
# # Check the number of documents in the corpus object
# length(corpus)
# inspect(corpus)


# for dataframeSource
# The first column must be named "doc_id" and contain a unique string identifier for each document.
# The second column must be named "text" and contain a "UTF-8" encoded string representing the document's content.

data2 <- data %>% select("doc_id" =pmid, "text"= abstract)


# Set the document names
names(corpus) <- as.numeric(data$pmid)

corpus <- Corpus(DataframeSource(data2))




# Check the document names
names(corpus)

# corpus <- tm_map(corpus, removeNumbers = FALSE) # Keep numbers
corpus <- tm_map(corpus, content_transformer(function(x, pattern) gsub("[[:punct:]]", " ", x))) # remove punctuation
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\.{2,}", " ", x))) # Remove ellipses
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\s+", " ", x))) # Remove extra white space
corpus <- tm_map(corpus, content_transformer(function(x) gsub("^\\s+|\\s+$", "", x))) # Remove leading/trailing white space
corpus <- tm_map(corpus, stemDocument) # Or use lemmatization instead

inspect(corpus) ## check what lemmatization does

tdm <- TermDocumentMatrix(corpus)

mydtm2 <-TermDocumentMatrix(corpus, control = list(ngrams = c(3)))

check <- as.data.frame(as.matrix(mydtm2)) %>% arrange(across(everything(),desc))


?arrange
?arrange_all

dtm <- DocumentTermMatrix(corpus)
checkDtm <- as.data.frame(as.matrix(dtm))

train <- as.data.frame(as.matrix(tdm))
train$class <- data$class

?TermDocumentMatrix()

# read in data (replace "filename.csv" with the actual name of your file)


# convert to a tidy format
data_tidy <- data %>% group_by(pmid) %>% 
  select(title, abstract, exposure1, prevalenceSingleValue1) %>%
  unnest_tokens(word, title) %>%
  unnest_tokens(word, abstract) %>%
  unnest_tokens(word, exposure1) %>%
  filter(!word %in% stop_words$word) %>%
  mutate(word = wordStem(word)) # use stemming to reduce words to their roots

# create an ngram model
ngram_model <- data_tidy %>%
  count(word, prevalenceSingleValue1) %>%
  group_by(prevalenceSingleValue1) %>%
  mutate(total_words = sum(n)) %>%
  filter(n > 1) %>%
  mutate(probability = n/total_words) %>%
  select(prevalenceSingleValue1, word, probability)

# search for information
search_terms <- c("iron deficiency anaemia", "anemia")
results <- ngram_model %>%
  filter(word %in% search_terms) %>%
  arrange(prevalenceSingleValue1, desc(probability))
