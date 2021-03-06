rm(list=ls())

setwd("C:/Users/conra/Documents/GitHub/ngss_words")

library(magrittr)
library(lubridate)
library(rtweet)

load("NGSSchat_sentiment_states_revised.rda")

# ind <- grep("Welcome to #NGSSchat", tweets_dl$text)

# Select 1,000 most busy hours in data set

time <- tweets_dl$created_at
time <- format(time, format="%Y-%m-%d %H")

freq <- table(time) 
freq <- sort(freq, decreasing = T)
freq <- freq %>% head(1000) 

hours <- names(freq)

# Select tweets at the beginning and end of these 500 hours (+- 5 minutes around edge)

hours <- sapply(hours, paste, ":00:00 UTC", sep="")
hours <- as.character(hours)
hours <- as.POSIXct(hours, tz = "UTC")

# Add minutes of +- 3 minutes to "hours"

original <- hours

for (i in seq(60, 5*60, 60)){
  hours <- c(hours, original-i)
}

for (i in seq(60, 5*60, 60)){
  hours <- c(hours, original+i)
}

# Standardize tweet posting times to minutes

ptimes <- tweets_dl$created_at
ptimes <- round_date(ptimes, unit="1 minute")

possible <- tweets_dl[which(ptimes %in% hours), c("created_at", "text")]

# Grab chats with "Welcome to "NGSSchat"

ind <- grep("Welcome to #NGSSchat", possible$text)

sort_out <- possible$created_at[ind]

sort_out <- format(sort_out, format="%Y-%m-%d %H")

# Get hours of "Welcome to #NGSSchat"

sort_out <- sapply(sort_out, paste, ":00:00 UTC", sep="")
sort_out <- as.character(sort_out)
sort_out <- as.POSIXct(sort_out, tz = "UTC")

# Exclude these hours from all possible, most busy 500 hours

hours2 <- names(freq)
hours2 <- sapply(hours2, paste, ":00:00 UTC", sep="")
hours2 <- as.character(hours2)
hours2 <- as.POSIXct(hours2, tz = "UTC")

hours2 <- hours2[-which(hours2 %in% sort_out)]

# Iterate again and create +- 5 minutes

hours <- hours2

original <- hours

for (i in seq(60, 5*60, 60)){
  hours <- c(hours, original-i)
}

for (i in seq(60, 5*60, 60)){
  hours <- c(hours, original+i)
}

# Standardize tweet posting times to minutes

ptimes <- tweets_dl$created_at
ptimes <- round_date(ptimes, unit="1 minute")

# Again, take those tweets that fit the possible chat edges

possible <- tweets_dl[which(ptimes %in% hours), c("created_at", "text")]

# Exclude already ascertained days from possible chat starting points and +-1 day
# Because there are two chats in a period of three days (done because there is
# day overlap due to timezones)

possible <- possible[-which(date(possible$created_at) %in% date(sort_out)),]
possible <- possible[-which(date(possible$created_at) %in% (date(sort_out) + 1)),]
possible <- possible[-which(date(possible$created_at) %in% (date(sort_out) - 1)),]

# 337 possible tweets for manual lookup remain

write.csv(possible, "lookup.csv") # look up manually

# Grab additional chats from lookup

ind <- grep("Welcome to the first session of #NGSSchat", possible$text)
ind <- c(ind, grep("Our #nhed guest moderator for this evening is", possible$text))
ind <- c(ind, grep("Excited to learn and connect with my #NGSSchat community-- Join us-- happening", possible$text))

sort_out2 <- possible$created_at[ind]

sort_out2 <- format(sort_out2, format="%Y-%m-%d %H")

sort_out2 <- sapply(sort_out2, paste, ":00:00 UTC", sep="")
sort_out2 <- as.character(sort_out2)
sort_out2 <- as.POSIXct(sort_out2, tz = "UTC")

sort_out <- c(sort_out, sort_out2)
sort_out <- sort(sort_out)
sort_out <- sort_out %>% unique()

hours2 <- names(freq)
hours2 <- sapply(hours2, paste, ":00:00 UTC", sep="")
hours2 <- as.character(hours2)
hours2 <- as.POSIXct(hours2, tz = "UTC")

hours2 <- hours2[-which(hours2 %in% sort_out)]

# Iterate again

hours <- hours2

original <- hours

for (i in seq(60, 5*60, 60)){
  hours <- c(hours, original-i)
}

for (i in seq(60, 5*60, 60)){
  hours <- c(hours, original+i)
}

# Standardize tweet posting times to minutes

ptimes <- tweets_dl$created_at
ptimes <- round_date(ptimes, unit="1 minute")

possible <- tweets_dl[which(ptimes %in% hours), c("created_at", "text")]

# Exclude already ascertained days from possible chat starting points and +-1 day

possible <- possible[-which(date(possible$created_at) %in% date(sort_out)),]
possible <- possible[-which(date(possible$created_at) %in% (date(sort_out) + 1)),]
possible <- possible[-which(date(possible$created_at) %in% (date(sort_out) - 1)),]

# 233 possible tweets for manual lookup remain -> chat_frames_annotations.txt for discussion

length(sort_out) # 167 chat hours identified

# Create Variable "isChat", strict definition of chat as a 1 hour timeframe

time <- tweets_dl$created_at
time <- format(time, format="%Y-%m-%d %H")

hours <- sapply(time, paste, ":00:00 UTC", sep="")
hours <- as.character(hours)
hours <- as.POSIXct(hours, tz = "UTC")

ind <- which(hours %in% sort_out)

isChat <- rep(0, nrow(tweets_dl))

for (i in 1:length(isChat)){
  if (i %in% ind){
    isChat[i] <- 1
  }
}

tweets_dl$isChat <- isChat

# Setting isChat to 0 where hsNGSSchat is ongoing for possible overlap where isChat = 1

ind <- grep("#hsNGSSchat", tweets_dl[tweets_dl$isChat == 1,]$text)  

tweets_dl[tweets_dl$isChat == 1,]$created_at[ind]   # for overview on dates

# index 24,223: user tweeted #hsNGSSchat in a regular chat at random, no changing

tweets_dl[tweets_dl$isChat == 1,]$text[(ind[1]-20):(ind[1]+5)]

# index 25,169 to 25,400: a few mentiones of hsNGSSchat at the beginning of NGSSchat session, no changing

tweets_dl[tweets_dl$isChat == 1,]$text[25169:25200]

# index 26,271 to 26,402: yet few tweets are overlap with the end of hsNGSSchat and NGSSchat, no changing

tweets_dl[tweets_dl$isChat == 1,]$text[(26271-10):(26402+10)]

## ... taking a look at the dates of creation of the overlapping tweets, there seems to be only 
##     overlap at the beginning of NGSSchat sessions following right after hsNGSSchat

length(ind)   # only 33 mentions of hsNGSSchat in declared NGSSchat sessions

write_as_csv(tweets_dl, "CB2020_08_22_NGSSChat_chats.csv")
save(isChat, file="isChat.rda")
