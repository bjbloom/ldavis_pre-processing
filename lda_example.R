rm(list = ls())

library(dplyr)
library(lda)
library(LDAvisData)
library(stringr)
library(tm)

data(reviews, package = "LDAvisData")

stop_words <- stopwords("SMART")

#Lower and remove non-words
main_text <- reviews %>% 
        str_to_lower %>% 
        str_replace_all('[^a-z]', ' ')

#Tokenize on space and output as list
main_textsplit <- main_text %>% strsplit("\\s+")

#Find low freq words and remove them
word_freq_df <- c(sapply(unlist(main_textsplit), unlist)) %>% 
        data_frame(word=.) %>% 
        count(word) %>% 
        arrange(desc(n))

low_freq_words <- word_freq_df %>% 
        filter(n < 50) %>% 
        getElement('word')

remove_words <- function(wordlist) {
        low_freq_matched <- !is.na(match(wordlist, low_freq_words))
        stop_matched <- !is.na(match(wordlist, stop_words))
        new_list <- wordlist[!(low_freq_matched | stop_matched)]
        return(new_list)
}

main_textsplitremoved <- lapply(main_textsplit, remove_words)

#Create list of matrices for LDA Package
vocab <- main_textsplitremoved %>% 
        unlist %>% 
        unique %>% 
        sort

vocab <- vocab[vocab != '']

vocab_df <- data_frame(words=vocab) %>% 
        mutate(idx = as.integer(row_number() - 1))

create_term_matrix <- function(txt, vocab) {
        
        word_counts <- data_frame(words=txt) %>% count(words)
        
        full_counts <- vocab_df %>% 
                left_join(word_counts, by='words') %>% 
                mutate(n = as.integer(ifelse(is.na(n), 0, n))) %>% 
                select(-words) %>% 
                as.matrix %>% 
                t
}

documents <- lapply(main_textsplitremoved, create_term_matrix, vocab_df)

#Topic model
alpha <- 0.1
eta <- 0.05

topic_model <- lda.collapsed.gibbs.sampler(documents = documents,
                                           K = 20,
                                           vocab = vocab,
                                           alpha = alpha,
                                           eta = eta,
                                           num.iterations = 1000
                                           )

#Create list needed for LDAVis JSON
theta <- t(apply(topic_model$document_sums) + alpha, 2, function(x) x/sum(x))
phi <- t(apply(topic_model$topics) + eta, 2, function(x) x/sum(x))
doc_length <- sapply(documents, function(x) sum(x[2, ]))
term_freq <- comment_textsplitremoved %>% 
        unlist %>% 
        table %>% 
        as.integer

comment_model <- list(phi = phi,
                      theta = theta,
                      doc_length = doc_length,
                      vocab = comment_vocab,
                      term_freq = term_freq)

