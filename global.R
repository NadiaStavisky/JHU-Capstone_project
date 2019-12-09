#setwd("C:/Users/nstavisky/OneDrive - Facebook/Study/R/Shiny 1.4 - Johns Hopkins University _ Coursera_files/Next_Word_I")


library("shiny")
library("wordcloud")
library("tidyverse")
library("readr")
library("textstem")
library("tm")
library("RColorBrewer") # color palettes

Unigramdf <- read_rds("./Unigramdf.Rdata")
Bigramdf <- read_rds("./Bigramdf.Rdata")
Trigramdf <- read_rds("./Trigramdf.Rdata")
Qgramdf <- read_rds("./Qgramdf.Rdata")

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
CleanInput <- function(input) {
    input <- iconv(input, "latin1", "ASCII", sub="")
    corpus_input <- VCorpus(VectorSource(input))
    # Convert the text to lower case
    corpus_input <- tm_map(corpus_input, content_transformer(tolower))
    # Remove numbers
    corpus_input <- tm_map(corpus_input, removeNumbers)
    # Remove english common stopwords
    #corpus_input <- tm_map(corpus_input, removeWords, stopwords("english"))
    # Remove punctuations
    corpus_input <- tm_map(corpus_input, removePunctuation)
    # Eliminate extra white spaces
    corpus_input <- tm_map(corpus_input, stripWhitespace)
    # removes any words with 3 or more repeated letters
    corpus_input <- tm_map(corpus_input, toSpace, "(.)\\1{2,}")
    # removes any remaining single letter words
    corpus_input <- tm_map(corpus_input, toSpace, "\\b(.)\\b")
    # removes any repeated prases
    corpus_input <- tm_map(corpus_input, toSpace, "(\\W|^)(.+)\\s\\2")
    # removes any words with numeric digits
    corpus_input <- tm_map(corpus_input, toSpace, "[[:digit:]]")
    corpus_input <- tm_map(corpus_input, lemmatize_strings)
    input_string<- as.character(corpus_input[[1]][1])
    #remove remaining " " after cleaning
    input_string<- gsub("(^[[:space:]]+|[[:space:]]+$)", "", input_string)
    
    return(input_string)
}

NextWord <- function(input, n){
    input <- CleanInput(input)
    input <- unlist(strsplit(input, split = " "))
    lenInput <- length(input)
    next_word <- NULL #next word based on previous 3,2,1 words
    if (lenInput >= 3 & length(next_word)==0) {
        next_word <- Qgramdf %>%
            separate(word, c("w1", "w2", "w3", "next_word"), sep = " ") %>%
            filter(w3 == input[lenInput] & w2 == input[lenInput-1] & w1 == input[lenInput-2]) %>%
            select(next_word, freq) %>%
            group_by( next_word) %>%
            summarize(n = sum(freq)) %>% 
            mutate(prt_freq = n/sum(n), weighted_part_freq = 4+n/sum(n),n_gramm = "Qgramdf_pred_model") %>% 
            arrange(desc(prt_freq))
        
    }
    if (lenInput >= 2 & length(next_word$next_word)<=3) {
        next_word3 <- Trigramdf %>%
            separate(word, c("w1", "w2", "next_word"), sep = " ") %>%
            filter((w2 == input[lenInput] & w1 == input[lenInput-1])) %>%
            select(next_word, freq) %>%
            group_by(next_word) %>%
            summarize(n = sum(freq)) %>% mutate(prt_freq = n/sum(n), weighted_part_freq = 3+n/sum(n), n_gramm = "Trigramdf_pred_model") %>% arrange(desc(prt_freq))
        next_word <- rbind(next_word, next_word3) %>% 
            group_by(next_word) %>%
            summarize(n = n[which(weighted_part_freq == max(weighted_part_freq))],
                      prt_freq = prt_freq[which(weighted_part_freq == max(weighted_part_freq))],
                      weighted_part_freq = max(weighted_part_freq),
                      n_gramm = n_gramm[which(weighted_part_freq == max(weighted_part_freq))]) %>% 
            arrange(desc(weighted_part_freq))
    }
    if (lenInput >= 1 & length(next_word$next_word)<=3) {
        next_word2 <- Bigramdf %>%
            separate(word, c("w1", "next_word"), sep = " ") %>%
            filter(w1 == input[lenInput] ) %>%
            select(next_word, freq) %>%
            group_by(next_word) %>%
            summarize(n = sum(freq)) %>% mutate(prt_freq = n/sum(n), weighted_part_freq = 2+n/sum(n), n_gramm = "Bigramdf_pred_model") %>% arrange(desc(prt_freq))
        next_word <- rbind(next_word, next_word2) %>% 
            group_by(next_word) %>%
            summarize(n = n[which(weighted_part_freq == max(weighted_part_freq))],
                      prt_freq = prt_freq[which(weighted_part_freq == max(weighted_part_freq))],
                      weighted_part_freq = max(weighted_part_freq),
                      n_gramm = n_gramm[which(weighted_part_freq == max(weighted_part_freq))]) %>% 
            arrange(desc(weighted_part_freq))
    }
    if (length(next_word$next_word)<=3) {
        next_word1 <- Unigramdf %>%
            mutate(next_word = word) %>%
            select(next_word, freq) %>%
            group_by(next_word) %>%
            summarize(n = sum(freq)) %>% mutate(prt_freq = n/sum(n), weighted_part_freq = 1+n/sum(n), n_gramm = "Unigramdf_pred_model") %>% arrange(desc(prt_freq))
        next_word <- rbind(next_word, next_word1) %>% 
            group_by(next_word) %>%
            summarize(n = n[which(weighted_part_freq == max(weighted_part_freq))],
                      prt_freq = prt_freq[which(weighted_part_freq == max(weighted_part_freq))],
                      weighted_part_freq = max(weighted_part_freq),
                      n_gramm = n_gramm[which(weighted_part_freq == max(weighted_part_freq))]) %>% 
            arrange(desc(weighted_part_freq))
        
    }
    return(next_word %>% 
        arrange(desc(weighted_part_freq)) %>%
            head(n))
    
}
