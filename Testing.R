#Sourcing Packages and Functions

source("Functions.R")

#Load the Dictionaries

unigram <- readRDS("unigram.rds")
bigram <- readRDS("bigram.rds")
trigram <- readRDS("trigram.rds")
quadgram <- readRDS("quadgram.rds")
test <- readRDS("test.rds")

#Perplexity of the dictionaries

perplexity_table <- tibble(Bigram = perplexity(bigram$Probability) %>% round(2),
                      Trigram = perplexity(trigram$Probability) %>% round(2),
                      Quadgram = perplexity(quadgram$Probability) %>% round(2))

saveRDS(perplexity_table, "perplexity_table.rds")

#Accuracy test for bigrams

set.seed(123)
bigram_test <- test %>% unnest_tokens(output = bigram,
                                     input = lines,
                                     token = "ngrams",
                                     n = 2) %>%
        separate(col = bigram, into = c("word1", "Next_Word"), sep = " ") %>%
        cleanText(ngram = 2) %>% 
        sample_n(14000) 

pred <- mapply(NextWord, bigram_test$word1)
rate_bi <- sum(if_else(bigram_test$Next_Word %in% unlist(pred), 1, 0))/nrow(bigram_test)

#Accuracy test for trigrams

set.seed(123)
trigram_test <- test %>% unnest_tokens(output = trigram,
                                       input = lines,
                                       token = "ngrams",
                                       n = 3) %>% 
        separate(col = trigram, into = c("word1", "word2", "Next_Word"), sep = " ") %>%
        cleanText(ngram = 3) %>%
        sample_n(14000)

pred <- mapply(NextWord, trigram_test$word1, trigram_test$word2)
rate_tri <- sum(if_else(trigram_test$Next_Word %in% unlist(pred), 1, 0))/nrow(trigram_test) 

#Accuracy test for quadgrams

set.seed(123)
quadgram_test <- test %>% unnest_tokens(output = quadgram,
                                       input = lines,
                                       token = "ngrams",
                                       n = 4) %>% 
        separate(col = quadgram, into = c("word1", "word2", "word3", "Next_Word"), sep = " ") %>%
        cleanText(ngram = 4) %>%
        sample_n(14000)

pred <- mapply(NextWord, quadgram_test$word1, quadgram_test$word2, quadgram_test$word3)
rate_quad <- sum(if_else(quadgram_test$Next_Word %in% unlist(pred), 1, 0))/nrow(quadgram_test) 

accuracy_table <- tibble(Bigram = rate_bi,
                         Trigram = rate_tri,
                         Quadgram = rate_quad)

accuracy_table %>% saveRDS("accuracy_table.rds")
