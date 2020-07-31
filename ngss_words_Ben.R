# load libraries
source("~/Documents/GitHub/ngss_words/functions.R")
library(tidyverse)
library(tidytext)
library(dplyr)
library(stringi)

# import dataset
full_data <- read.csv("~/Documents/GitHub/ngsschat-sentiment/NGSSchat_sentiment_states_revised.csv")
# sentiment_means <- read.csv("~/Documents/GitHub/ngsschat-sentiment/sentiment_means.csv")

# vector of words to search through
words <- c("cross-cutting",
           "patterns",
           "cause",
           "effect",
           "scale",
           "proportion",
           "quantity",
           "systems",
           "system models",
           "energy",
           "matter",
           "flows",
           "cycles",
           "conservation",
           "structure",
           "function",
           "stability",
           "change")
words

# top_words: most frequent words in all tweets
# could later group_by year, state, etc (like in sentiment.R)
data(stop_words)
tidy_data <- tibble(line = 1:nrow(full_data), text = full_data$text)
top_words <- tidy_data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
top_words

# finding frequencies from top_words and search list
words <- data.frame(words)
words <- left_join(words, top_words, by = c("words" = "word"))


