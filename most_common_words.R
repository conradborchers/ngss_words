# Setup

rm(list=ls())

library(magrittr)

setwd("C:/Users/conra/Documents/GitHub/ngss_words")

load("NGSSchat_sentiment_states_revised.rda")

words <- strsplit(tweets_dl$text, split=" ") %>% unlist() %>% as.character()

common_words <- sort(table(words), decreasing=T) 

write.table(as.data.frame(common_words), "common_words.csv")
