setwd("~/Documents/GitHub/ngss_words")

# load libraries
#source("~/Documents/GitHub/ngss_words/functions.R")
library(tidyverse)
library(tidytext)
library(dplyr)
library(stringi)
library(SnowballC)
library(textstem)

#### Data ####

full_data <- read.csv("~/Documents/GitHub/ngsschat-sentiment/NGSSchat_sentiment_states_revised.csv")
# sentiment_means <- read.csv("~/Documents/GitHub/ngsschat-sentiment/sentiment_means.csv")

full_data$state <- full_data$state %>%
  str_remove(":main") %>%
  str_remove(":south") %>%
  str_remove(":north") %>%
  str_remove(":long island") %>%
  str_remove(":manhattan") %>%
  str_remove(":whidbey island") %>%
  str_remove(":martha's vineyard")
full_data <- mutate(full_data, state = tools::toTitleCase(state))
unique(full_data$state)

full_data$year <- as.integer(substring(full_data$created_at, 0, 4))

# dataframe of words to search through
search_words <- tribble(
  ~word, ~principle,
  "cross-cutting", NA,
  "patterns", 1,
  "cause", 2,
  "effect", 2,
  "scale", 3,
  "proportion", 3,
  "quantity", 3,
  "systems", 4,
  "system models", 4,
  "energy", 5,
  "matter", 5,
  "flows", 5,
  "conservation", 5,
  "structure", 6,
  "function", 6,
  "stability", 7,
  "change", 7
)
search_words


#### Top Words ####

# top_words: most frequent words in all tweets
# could potentially group_by year, state, or status (or multiple)
data(stop_words)
tidy_data <- tibble(tweet = 1:nrow(full_data), 
                    text = full_data$text, 
                    year = full_data$year,
                    state = full_data$state)
top_words <- tidy_data %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = textstem::lemmatize_words(word)) %>%
  count(word, sort = TRUE)
top_words


#### Search Words Frequencies ####

# frequencies of search words from top_words
search_words <- left_join(search_words, top_words, by = "word")


#### Code from sentiment.R ####

# overall mean (standardized ratio = pos / total)
sentiment_means %>%
  summarise(sentiment_ratio = sum(nPos, na.rm = TRUE)/sum(total, na.rm = TRUE))

# mean by year (standardized ratio)
sentiment_means %>%
  group_by(year) %>%
  summarise(sentiment_ratio = sum(nPos, na.rm = TRUE)/sum(total, na.rm = TRUE))

# mean by state (standardized ratio)
sentiment_means %>%
  group_by(name) %>%
  summarise(sentiment_ratio = sum(nPos, na.rm = TRUE)/sum(total, na.rm = TRUE)) %>%
  arrange(desc(sentiment_ratio))

# by status (standardized ratio)
sentiment_means %>%
  rename(State = name) %>% 
  left_join(state_data) %>%
  group_by(key) %>% 
  summarise(sentiment_ratio = sum(nPos, na.rm = TRUE)/sum(total, na.rm = TRUE)) %>% 
  arrange(desc(sentiment_ratio)) %>% 
  filter(!is.na(key)) %>% 
  clipr::write_clip()

# for plot (standardized ratio)
state_means <- sentiment_means %>%
  group_by(name) %>%
  summarise(sentiment_ratio = sum(nPos, na.rm = TRUE)/sum(total, na.rm = TRUE)) %>% 
  arrange(desc(sentiment_ratio))

