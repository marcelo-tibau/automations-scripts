# setwd("~/Documents/FLAGCX/Projects/mkay")

# Load the libraries
library(twitteR)
library(httr)
library(SchedulerR) 
library(SnowballC)

# OAuth protocol
api_key <- "xxxxxxxxxxxxxxxxxxxxxx"
api_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

# Extracting tweets using user timeline

MK_tweets <- userTimeline("marykaybrasil", n = 3200)
tweetsmk.df <- twListToDF(MK_tweets)

# Preparing data to be saved

## As .RData
date <- Sys.Date()
date <- as.character(date)
name <- paste(date, ".RData")

save(tweetsmk.df, file = name)
save(tweetmk.df, file = "mk.R")

## As csv
write.csv(tweetsmk.df, file = "mkTweets.csv")

mkTwetsDF <- read.csv("mkTweets.csv", header=TRUE, sep=",")

# Text analysis
library(tm)

# Tidy data process

# build a corpus and specify the source to be character vectors

corpusMK <- Corpus(VectorSource(tweetsmk.df$text))
corpusMKTwo <- Corpus(VectorSource(mkTwetsDF$text))


# with stringr package
library('stringr')
clean_tweet <- corpusMKTwo

# removing retweets, references to screen names, hashtags, spaces, numbers, 
# punctuations, urls
clean_tweet <- gsub("&amp", "", unclean_tweet)
clean_tweet <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", clean_tweet)
clean_tweet <- gsub("@\\w+", "", clean_tweet)
clean_tweet <- gsub("[[:punct:]]", "", clean_tweet)
clean_tweet <- gsub("[[:digit:]]", "", clean_tweet)
clean_tweet <- gsub("http\\w+", "", clean_tweet)
clean_tweet <- gsub("[ \t]{2,}", "", clean_tweet)
clean_tweet <- gsub("^\\s+|\\s+$", "", clean_tweet) 

#get rid of unnecessary spaces
clean_tweet <- str_replace_all(clean_tweet," "," ")
# Get rid of URLs
clean_tweet <- str_replace_all(clean_tweet, "http://t.co/[a-z,A-Z,0-9]*{8}","")
# Take out retweet header, there is only one
clean_tweet <- str_replace(clean_tweet,"RT @[a-z,A-Z]*: ","")
# Get rid of hashtags
clean_tweet <- str_replace_all(clean_tweet,"#[a-z,A-Z]*","")
# Get rid of references to other screennames
clean_tweet <- str_replace_all(clean_tweet,"@[a-z,A-Z]*","")  
# Get rid of the funny question mark
clean_tweet <- str_replace_all(clean_tweet,"�[a-z,A-Z]*","")

# remove punctuation

corpusMK <- tm_map(corpusMK, removePunctuation)
corpusMKTwo <- tm_map(corpusMKTwo, removePunctuation)

# convert to lower case 

corpusMK <- tm_map(corpusMK, content_transformer(tolower))
corpusMKTwo <- tm_map(corpusMKTwo, content_transformer(tolower))

corpusMKthree <- clean_tweet
corpusMKthree <- Corpus(VectorSource(corpusMKthree))
corpusMKthree <- tm_map(corpusMKthree, content_transformer(tolower))

# remove stopwords
corpusMKthree <- tm_map(corpusMKthree, removeWords, stopwords("portuguese"))

corpusMKthree <- tm_map(corpusMKthree, removeWords, c("xedxaxbdxedxbx","xedxaxbdxedxbxb","xedxaxbdxedxbxd "))


# keep a copy of corpus to use later as a dictionary for stemming and lemmatization
corpusMKthreeCopy <- corpusMKthree

# inspect the first documents (code below is used for to make text fit for paper width)
inspect(corpusMKthree[1:5])
for (i in 1:5) {
  cat(paste("[[", i, "]]", sep = ""))
  writeLines(as.character(corpusMKthree[[i]]))
}

# Tidying documents to create a tdm
library(quanteda)
library(dplyr)
library(tidytext)

# create a term document matrix (tdm)
tdm <- TermDocumentMatrix(corpusMKthree, control = list(wordLengths = c(1, Inf)))

## Frequency words and Association
idx <- which(dimnames(tdm)$Terms == "s")

# inspect(tdm[idx + (0:5), 101:110])
(freq.terms <- findFreqTerms(tdm, lowfreq=5))

termF <- rowSums(as.matrix(tdm))
termF <- subset(termF, termF >= 5)
dftF <- data.frame(term = names(termF), freq = termF)

termFtwo <- subset(termF, termF >= 50)
dftFtwo <- data.frame(term = names(termFtwo), freq = termFtwo)

termFthree <- subset(termF, termF >= 100)
dftFthree <- data.frame(term = names(termFthree), freq = termFthree)


# Exploratory plots

# Terms frequency
library(ggplot2)
ggplot(dftF, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()

ggplot(dftFthree, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()

# which words are associated with 'mary kay'?
findAssocs(tdm, "marykay", 0.2)

# which words are associated with 'mk'?
findAssocs(tdm, "mk", 0.25)

# Terms connections by frequency

# source("https://bioconductor.org/biocLite.R")
# biocLite("graph")

# source("https://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")

library(graph)
library(Rgraphviz)
plot(tdm, term = termFthree, corThreshold = 0.12, weighting = T)

plot(tdm, term = termF, corThreshold = 0.12, weighting = T)

plot(dftF, term = termF, corThreshold = 0.12, weighting = T)

# Wordcloud
library(wordcloud)
sb <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
wordFreq <- sort(rowSums(sb), decreasing = T)
wordcloud(words = names(wordFreq), freq = wordFreq, min.freq = 3, random.order = F)
