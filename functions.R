addCaps <- function(words){

    for (element in words){
	    words <- list.append(words, str_to_sentence(element))
    }

    return(words)
}

addCapsEntry <- function(words){

    for (i in 1:length(words)){
	    words[i] <- paste(words[i], str_to_title(words[i]), sep="|")
    }

    return(words)
}

collapseOR <- function(words){

    q <- paste(words, sep="", collapse="\\>|\\<")
    q <- paste(c("\\<", q, "\\>"), sep="", collapse="")

    return(q)
}

any <- function(words, text){

    q <- addCaps(words)
    q <- collapseOR(q)

    return(grep(q, text))
}

every_entry <- function(words, text){

    q <- addCapsEntry(words)

    out <- list()

    for (i in 1:length(q)){
        out[[i]] <- grep(q[i], text)
    }

    names(out) <- words

    return(out)
}

convert2bool <- function(out, object){

    out <- unlist(out) %>% as.numeric()

    res <- rep(0, nrow(object))

    for (i in 1:nrow(object)){
        if (i %in% out){
            res[i] <- 1
        }
    }

    return(res)
}

add_search <- function(words, tweets_dl){
    
    out <- every_entry(words, tweets_dl$text)

    new_cols <- list()

    for (i in 1:length(out)){
        new_cols[[i]] <- convert2bool(out[i], tweets_dl)
    }

    names(new_cols) <- words

    return(new_cols)
}

add_to_df <- function(words, tweets_dl){

    new_cols <- add_search(words, tweets_dl)    

    return(cbind(tweets_dl, as.data.frame(new_cols)))
}

## For Wordcloud only

create_cloud <- function(text){
                 docs <- Corpus(VectorSource(text))
                 
                 toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
                      
                 # Replace chars with space
                 
                 docs <- tm_map(docs, toSpace, "/")
                 docs <- tm_map(docs, toSpace, "@")
                 docs <- tm_map(docs, toSpace, "\\|")
                 
                 # Convert the text to lower case
                 docs <- tm_map(docs, content_transformer(tolower))
                 # Remove numbers
                 docs <- tm_map(docs, removeNumbers)
                 # Remove english common stopwords
                 docs <- tm_map(docs, removeWords, stopwords("english"))
                 # Remove your own stop word
                 # specify your stopwords as a character vector
                 docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
                 # Remove punctuations
                 docs <- tm_map(docs, removePunctuation)
                 # Eliminate extra white spaces
                 docs <- tm_map(docs, stripWhitespace)
                 # Text stemming
                 # docs <- tm_map(docs, stemDocument)
                 
                 dtm <- TermDocumentMatrix(docs)
                 m <- as.matrix(dtm)
                 v <- sort(rowSums(m),decreasing=TRUE)
                 d <- data.frame(word = names(v),freq=v)
                 
                 return(d)
}

