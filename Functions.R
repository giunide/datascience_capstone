#Packages Used

library(tidyverse)
library(tidytext)

#Functions used

#Data Cleaning function
cleanText <- function(text, ngram){
        
        bad_words <- read_lines("full-list-of-bad-words_text-file_2018_07_30.txt", skip = 13)
        bad_words <- tibble(word = bad_words)
        
        if (ngram == 1){
                
                text <- text %>% filter(
                        !Next_Word %in% bad_words$word,  #remove any bad word 
                        !str_detect(Next_Word,"[[:digit:]]"),  #remove any number 
                        !str_detect(Next_Word, "[[:punct:]]"),  #remove any punctuation symbol
                        !str_detect(Next_Word, "(.)\\1{2,}"))  #remove with three or more repeated letters
                
        }
        
        else if (ngram == 2){
                
                text <- text %>% filter(
                        !word1 %in% bad_words$word,  #remove any bad word in word 1
                        !Next_Word %in% bad_words$word,  #remove any bad word in Next_Word
                        !str_detect(word1,"[[:digit:]]"),  #remove any number in word 1
                        !str_detect(Next_Word,"[[:digit:]]"),  #remove any number in Next_Word
                        !str_detect(word1, "[[:punct:]]"),  #remove any punctuation symbol in word 1
                        !str_detect(Next_Word, "[[:punct:]]"), #remove any punctuation symbol in Next_Word
                        !str_detect(word1, "(.)\\1{2,}"), #remove with three or more repeated letters in word 1
                        !str_detect(Next_Word, "(.)\\1{2,}"))  #remove with three or more repeated letters in Next_Word
                
        }
        
        else if (ngram == 3){
                
                text <- text %>% filter(
                        !word1 %in% bad_words$word,  #remove any bad word in word 1
                        !word2 %in% bad_words$word,  #remove any bad word in word 2
                        !Next_Word %in% bad_words$word,  #remove any bad word in Next_Word
                        !str_detect(word1,"[[:digit:]]"),  #remove any number in word 1
                        !str_detect(word2,"[[:digit:]]"),  #remove any number in word 2
                        !str_detect(Next_Word,"[[:digit:]]"),  #remove any number in Next_Word
                        !str_detect(word1, "[[:punct:]]"),  #remove any punctuation symbol in word 1
                        !str_detect(word2, "[[:punct:]]"), #remove any punctuation symbol in word 2
                        !str_detect(Next_Word, "[[:punct:]]"), #remove any punctuation symbol in Next_Word
                        !str_detect(word1, "(.)\\1{2,}"), #remove with three or more repeated letters in word 1
                        !str_detect(word2, "(.)\\1{2,}"), #remove with three or more repeated letters in word 2
                        !str_detect(Next_Word, "(.)\\1{2,}"))  #remove with three or more repeated letters in Next_Word
        }
        else if (ngram == 4){
                
                text <- text %>% filter(
                        !word1 %in% bad_words$word,  #remove any bad word in word 1
                        !word2 %in% bad_words$word, #remove any bad word in word 2
                        !word3 %in% bad_words$word, #remove any bad word in word 3
                        !Next_Word %in% bad_words$word,  #remove any bad word in Next_Word
                        !str_detect(word1,"[[:digit:]]"),  #remove any number in word 1
                        !str_detect(word2,"[[:digit:]]"),  #remove any number in word 2
                        !str_detect(word3,"[[:digit:]]"),  #remove any number in word 3
                        !str_detect(Next_Word,"[[:digit:]]"),  #remove any number in Next_Word
                        !str_detect(word1, "[[:punct:]]"),  #remove any punctuation symbol in word 1
                        !str_detect(word2, "[[:punct:]]"), #remove any punctuation symbol in word 2
                        !str_detect(word3, "[[:punct:]]"), #remove any punctuation symbol in word 3
                        !str_detect(Next_Word, "[[:punct:]]"), #remove any punctuation symbol in Next_Word
                        !str_detect(word1, "(.)\\1{2,}"), #remove with three or more repeated letters in word 1
                        !str_detect(word2, "(.)\\1{2,}"), #remove with three or more repeated letters in word 2
                        !str_detect(word3, "(.)\\1{2,}"), #remove with three or more repeated letters in word 3
                        !str_detect(Next_Word, "(.)\\1{2,}"))  #remove with three or more repeated letters in Next_Word
        }
        else print("please chose an n equal to 1, 2, 3 or 4")
}

#Word Prediction function
NextWord <- function(x = "", y = "", z = "", n = 1){
        
        if(length(x) == 0 & length(y) == 0 & length(z) == 0) print("Please insert some Text")
        
        else if (x %in% quadgram$word1 & y %in% quadgram$word2 & z %in% quadgram$word3){
                quadgram %>% filter(x == word1, y == word2, z == word3) %>% select(Next_Word, Probability) %>% head(n)
        }
        else if (x %in% trigram$word1 & y %in% trigram$word2){
                trigram %>% filter(x == word1, y == word2) %>% select(Next_Word, Probability) %>% head(n)
        }
        else if (x %in% bigram$word1){
                bigram %>% filter(x == word1) %>% select(Next_Word, Probability) %>% head(n)
        }
        else unigram[1:n,]
}

#Perprlexity function
perplexity <- function(probabilities) {
        return(exp(-sum(log(probabilities)) / length(probabilities)))
}