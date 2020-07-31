# addCaps <- function(words){

#    for (element in words){
#	    words <- list.append(words, str_to_sentence(element))
#    }

#    return(words)
#}

#addCapsEntry <- function(words){

#    for (i in 1:length(words)){
#	    words[i] <- paste(words[i], str_to_title(words[i]), sep="|")
#    }

#    return(words)
#}

# Return search query string for grep()

collapseOR <- function(words){

    qu <- paste(words, sep="", collapse="\\>|\\<")
    qu <- paste(c("\\<", qu, "\\>"), sep="", collapse="")

    return(qu)
}

any <- function(words, text){

    qu <- collapseOR(words)

    return(grep(qu, text))
}

every_entry <- function(words, text){
    
    qu <- words

    out <- list()

    for (i in 1:length(qu)){
        out[[i]] <- grep(qu[i], text)
    }

    names(out) <- words

    return(out)
}

# Returns full vector of whether the search term is in the tweets

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

add_any <- function(words, tweets, tweets_dl){
    
    out <- any(words, tweets_dl$text)
    
    new_col <- convert2bool(out, tweets_dl)
    
    return(new_col)
}


add_to_df <- function(words, tweets_dl){

    new_cols <- add_search(words, tweets_dl)    

    return(cbind(tweets_dl, as.data.frame(new_cols)))
}

add_to_df_category <- function(words, tweets, tweets_dl, category_name="new_category"){
    
    new_col <- add_any(words, tweets, tweets_dl)   
    
    tweets_dl <- cbind(tweets_dl, as.data.frame(new_col))
    
    colnames(tweets_dl)[ncol(tweets_dl)] <- category_name
    
    return(tweets_dl)
}
