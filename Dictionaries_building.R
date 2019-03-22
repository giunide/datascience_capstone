#Sourcing Packages and Functions

source("Functions.R")

#Check if the dataset is in the PC, if negative then download it

if(!file.exists("capstone_proyect.zip")){
        download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
                      "capstone_proyect.zip")
}

#Load the data into R

if(!dir.exists("final")){
        unzip("capstone_proyect.zip")
}

blogs <- read_lines("final/en_US/en_US.blogs.txt") 
blogs <- tibble(lines = blogs)

twitter <- read_lines("final/en_US/en_US.twitter.txt") 
twitter <- tibble(lines = twitter)

news <- read_lines("final/en_US/en_US.news.txt") 
news <- tibble(lines = news)

#Sampling 15 % the original data

set.seed(123) #setting seed for replicability
all <- bind_rows(sample_frac(blogs, size = 0.10),
                 sample_frac(twitter, size = 0.10),
                 sample_frac(news, size = 0.10))

#Split into training data (70 %) and testing data (30 %)

set.seed(123) #setting seed for replicability
inTrain <- sample(nrow(all), size = nrow(all) * 0.7) %>% sort()
training <- all[inTrain,]
test <- all[-inTrain,]

test %>% saveRDS("test.rds") #saving as an RDS for future use

rm(all, blogs, twitter, news, inTrain, test) #remove unused objects

# Get the unigrams

unigram <- training %>% unnest_tokens(output = Next_Word, #tokenization
                                      input = lines) %>% 
        cleanText(ngram = 1) %>% 
        count(Next_Word, name = "n1") %>% #counting the words
        mutate(Probability = n1/sum(n1)) %>% #calulating probabilities
        arrange(desc(Probability))   

unigram %>% select(-n1) %>% saveRDS(file =  "unigram.rds") #saving as an RDS

# Get the bigrams

bigram <- training %>% unnest_tokens(output = bigram,  #tokenization
                                     input = lines,
                                     token = "ngrams",
                                     n = 2) %>% 
        count(bigram, name = "n2") %>%  #counting the bigrams
        separate(col = bigram, into = c("word1", "Next_Word"), sep = " ") %>% #separating bigrams into two words
        cleanText(ngram = 2)  %>% 
        left_join(unigram, by = c("word1" ="Next_Word")) %>% #getting the counts for word1
        mutate(Probability = n2/n1) %>% #calulating probabilities
        select(-n1) %>%
        arrange(desc(Probability)) 

bigram %>% select(-n2) %>% saveRDS(file =  "bigram.rds") #saving as an RDS

# Get the trigrams
bigram <- bigram %>% select(-Probability) %>% unite(word1,Next_Word, col = "bigram", sep = " " )

trigram <- training %>% unnest_tokens(output = trigram, #tokenization
                                      input = lines,
                                      token = "ngrams",
                                      n = 3) %>% 
        count(trigram, name = "n3") %>% #counting the trigrams
        separate(col = trigram, into = c("word1", "word2", "Next_Word"), sep = " ") %>% #separating trigrams into three words
        cleanText(ngram = 3) %>%
        unite(word1,word2, col = "bigram", sep = " ") %>% #uniting first two words in order to join with bigram count
        left_join(bigram, by ="bigram") %>% #getting the counts for first two words
        separate(col = bigram, into = c("word1", "word2"), sep = " ") %>%
        mutate(Probability = n3/n2) %>% #calulating probabilities
        select(-n2) %>%
        arrange(desc(Probability)) 

trigram %>% select(-n3) %>% saveRDS(file =  "trigram.rds") #saving as an RDS

# Get the quadgrams
trigram <- trigram %>% select(-Probability) %>% unite(word1, word2,Next_Word, col = "trigram", sep = " " )

quadgram <- training %>% unnest_tokens(output = quadgram, #tokenization
                                       input = lines,
                                       token = "ngrams",
                                       n = 4) %>% 
        count(quadgram, name = "n4") %>%
        separate(col = quadgram, into = c("word1", "word2", "word3", "Next_Word"), sep = " ") %>% #separating quadgrams into four words
        cleanText(ngram = 4) %>%
        unite(word1,word2, word3 , col = "trigram", sep = " ") %>% #uniting first three words in order to join with trigram count
        left_join(trigram, by ="trigram") %>% #getting the counts for first three words
        separate(col = trigram, into = c("word1", "word2", "word3"), sep = " ") %>%
        mutate(Probability = n4/n3) %>% #calulating probabilities
        select(-n4, -n3) %>%
        arrange(desc(Probability)) 

quadgram %>% saveRDS("quadgram.rds") #saving as an RDS