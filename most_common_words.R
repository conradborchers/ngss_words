# Setup

rm(list=ls())

library(magrittr)
library(stopwords)
library(stringr)
library(rlist)

load("NGSSchat_sentiment_states_revised.rda")

words <- strsplit(tweets_dl$text, split=" ") %>% unlist() %>% as.character()

ind <- grep("^#", words)          # Remove hashtags
ind <- c(ind, grep("^@", words))  # Remove tagged usernames

words <- words[-ind]

words <- gsub('[[:punct:] ]+','', words) # remove punctuation
words <- tolower(words)                  # remove caps

swords <- stopwords(language = "en", source = "nltk")
swords <- c(swords, stopwords(language = "en",source = "smart"))
swords <- c(swords, stopwords(language = "en",source = "snowball"))
swords <- c(swords, stopwords(language = "en",source = "stopwords-iso"))

swords <- unique(swords)

words <- words[-which(words %in% swords)]

# Getting word stem

# ind <- grep(".ed$", words)  # takes several minutes to converge,
                              # only eliminates very few words

# for (i in 1:length(words)){
#  if (i %in% ind){
#    words[i] <- substr(words[i], 1, nchar(words[i]) - 2)
#  }
#}

#ind <- grep(".ing$", words)

#for (i in 1:length(words)){
#  if (i %in% ind){
#    words[i] <- substr(words[i], 1, nchar(words[i]) - 3)
#  }
#}

common_words <- sort(table(words), decreasing=T) 
head(common_words)

common_words <- as.data.frame(common_words)

common_words <- common_words[common_words$Freq >= 100,]

write.csv(common_words, "common_words.csv")
