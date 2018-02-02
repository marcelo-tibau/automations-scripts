# setwd("~/Documents/FLAGCX/Projects/Automation/automations-scripts")

# Load the libraries
library(twitteR)
library(httr)
library(SchedulerR) 

# OAuth protocol
api_key <- "xxxxxxxxxxxxxxxxxxxxxx"
api_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
access_token_secret <- "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


# Extracting tweets using user timeline
NFL_tweets <- userTimeline("NFL", n = 3200)
tweetsnfl.df <- twListToDF(NFL_tweets)
dim(tweetsnfl.df)

# Preparing data to be saved

## As .RData
date <- Sys.Date()
date <- as.character(date)
name <- paste(date, ".RData")
save(tweetsnfl.df, file = name)

## As csv
write.csv(tweetsnfl.df, file = "nflTweets.csv")

## As Excel
library(xlsx)

write.xlsx(tweetsnfl.df, file = "nflTweetsExcel.xlsx")

# Text analysis
library(tm)

# Tidy data process
# general unicode conversion
library(stringi)
corpusNFL <- stri_trans_general(corpusNFL, "latin-ascii") # check other methods
corpusNFL <- Corpus(VectorSource(corpusNFL))

# build a corpus and specify the source to be character vectors
corpusNFL <- Corpus(VectorSource(tweetsnfl.df$text))

# remove punctuation
corpusNFL <- tm_map(corpusNFL, removePunctuation)


# convert to lower case 
#corpusNFL <- tm_map(corpusNFL, content_transformer(tolower))

corpusNFL <- tm_map(corpusNFL, tolower)

# remove numbers
corpusNFL <- tm_map(corpusNFL, removeNumbers)

# Create function to remove URLs
removeURL <- function(x)
  gsub("http[[:alnum:]]*", "", x)

# remove URLs
corpusNFL <- tm_map(corpusNFL, content_transformer(removeURL))

# remove stopwords
corpusNFL <- tm_map(corpusNFL, removeWords, stopwords("english"))

# keep a copy of corpus to use later as a dictionary for stemming and lemmatization
corpusNFLCopy <- corpusNFL

# stem words
corpusNFL <- tm_map(corpusNFL, stemDocument)

# inspect the first documents (code below is used for to make text fit for paper width)
inspect(corpusNFL[1:5])
for (i in 1:5) {
  cat(paste("[[", i, "]]", sep = ""))
  writeLines(as.character(corpusNFL[[i]]))
}

# Stem completion
corpusNFL <- tm_map(corpusNFL, content_transformer(stemCompletion), dictionary = corpusNFLCopy, lazy = TRUE)

# exploring jtimberlak in the dictionary
jtimberlakCases <- tm_map(corpusNFLCopy, grep, pattern = "jtimberlak") 
# sum(unlist(jtimberlakCases))
unlist(jtimberlakCases)

inspect(corpusNFLCopy[[3:5]])
for (i in 3:5) {
  cat(paste("[[", i, "]]", sep = ""))
  writeLines(as.character(corpusNFLCopy[[i]]))
}

inspect(corpusNFLCopy[[41]])
for (i in 41) {
  cat(paste("[[", i, "]]", sep = ""))
  writeLines(as.character(corpusNFLCopy[[i]]))
}

corpusNFL <- tm_map(corpusNFL, gsub, pattern = "jtimberlak", replacement = "jtimberlake")

inspect(corpusNFLtidy[[2]])
for (i in 2) {
  cat(paste("[[", i, "]]", sep = ""))
  writeLines(as.character(corpusNFLtidy[[i]]))
}

# Tidying documents to create a tdm
library(quanteda)
library(dplyr)
library(tidytext)

corpusNFLtidy <- tidy(corpusNFLtidy) # 2

# Create function to remove doc [2]
removeDoc2 <- function(x)
  gsub("������", "", x)

# remove doc [2]
corpusNFLtidy <- tm_map(corpusNFL, content_transformer(removeDoc2))

# steeler CamHeyward WizardOfBoz tell BAD joke ������
#corpusNFL <- tm_map(corpusNFL, gsub, pattern = "jtimberlak", replacement = "jtimberlake")
corpusNFLtidy <- tm_map(corpusNFLtidy, gsub, pattern = "steeler CamHeyward WizardOfBoz tell BAD joke ������", replacement = "steeler CamHeyward WizardOfBoz tell BAD joke")

corpusNFLtidy <- stri_trans_general(corpusNFLtidy, "latin-ascii") # 1
corpusNFLtidy <- Corpus(VectorSource(corpusNFLtidy)) # 3

# create a term document matrix (tdm)
tdm <- TermDocumentMatrix(corpusNFLtidy, control = list(wordLengths = c(1, Inf)))

## Frequency words and Association
idx <- which(dimnames(tdm)$Terms == "s")

# inspect(tdm[idx + (0:5), 101:110])
(freq.terms <- findFreqTerms(tdm, lowfreq=5))

termF <- rowSums(as.matrix(tdm))
termF <- subset(termF, termF >= 5)
dftF <- data.frame(term = names(termF), freq = termF)

# Exploratory plots

# Terms frequency
library(ggplot2)
ggplot(dftF, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()

# Terms connections by frequency

# source("https://bioconductor.org/biocLite.R")
# biocLite("graph")

# source("https://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")

library(graph)
library(Rgraphviz)
plot(tdm, term = termF, corThreshold = 0.12, weighting = T)

plot(dftF, term = termF, corThreshold = 0.12, weighting = T)

# Wordcloud
library(wordcloud)
sb <- as.matrix(tdm)
# calculate the frequency of words and sort it by frequency
wordFreq <- sort(rowSums(sb), decreasing = T)
wordcloud(words = names(wordFreq), freq = wordFreq, min.freq = 3, random.order = F)
