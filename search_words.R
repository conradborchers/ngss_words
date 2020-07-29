# Setup

rm(list=ls())

setwd("C:/Users/conra/Documents/GitHub/ngss_words")

pakete <- list("stringr", "rlist", "magrittr")

for (paket in pakete){
		if(eval(bquote(!require(.(paket))))){
				install.packages(paket);
				eval(bquote(require(.(paket))))
}}

source("functions.R")

load("NGSSchat_sentiment_states_revised.rda")

# Testing
# Search whether tweets contain "thank you", "thank",
# or "you" and add 0;1 boolean variable to data frame

words <- list("thank you", "thank", "you")

test <- add_to_df(words, tweets_dl[1:100,]) # takes ~ 10 s per string for whole data set

test[,(ncol(test)-2):ncol(test)]

# Descriptives; n

sum(test$thank == 1)
sum(test$you == 1)
sum(test$thank.you == 1)
