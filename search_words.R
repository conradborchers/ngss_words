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

tweets <- tweets_dl$text
tweets <- gsub('[[:punct:] ]+',' ', tweets) # remove punctuation
tweets <- tolower(tweets)                   # remove caps

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

# Test "any"

words <- list("science", "math", "physics")
add_any(words, tweets[1:100], tweets_dl[1:100,])

test <- add_to_df_category(words, tweets[1:100], tweets_dl[1:100,], "STEM")

# Descriptives; n

sum(test$STEM == 1)

## Test of full lists

source("wordlist.R")

tweets_dl <- add_to_df_category(asking_qs, tweets, tweets_dl, "asking_qs")
tweets_dl <- add_to_df_category(models, tweets, tweets_dl, "models")
tweets_dl <- add_to_df_category(investigations, tweets, tweets_dl, "investigations")
tweets_dl <- add_to_df_category(ana_data, tweets, tweets_dl, "investigations")
tweets_dl <- add_to_df_category(math_comp, tweets, tweets_dl, "expl_sol")
tweets_dl <- add_to_df_category(expl_sol, tweets, tweets_dl, "expl_sol")
tweets_dl <- add_to_df_category(argument, tweets, tweets_dl, "argument")
tweets_dl <- add_to_df_category(information, tweets, tweets_dl, "information")

sum(tweets_dl$asking_qs == 1)
sum(tweets_dl$models == 1)
sum(tweets_dl$investigations == 1)
sum(tweets_dl$ana_data == 1)
sum(tweets_dl$math_comp == 1)
sum(tweets_dl$expl_sol == 1)
sum(tweets_dl$argument == 1)
sum(tweets_dl$information == 1)



  