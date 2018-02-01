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

# build a corpus and specify the source to be character vectors
corpusNFL <- Corpus(VectorSource(tweetsnfl.df$text))

# remove punctuation
corpusNFL <- tm_map(corpusNFL, removePunctuation)

# remove numbers
corpusNFL <- tm_map(corpusNFL, removeNumbers)

# Create function to remove URLs
removeURL <- function(x)
  gsub("http[[:alnum:]]*", "", x)

# remove URLs
corpusNFL <- tm_map(corpusNFL, content_transformer(removeURL))



# convert to lower case 
#corpusNFL <- tm_map(corpusNFL, content_transformer(tolower))
corpusNFL <- tm_map(corpusNFL, tolower)




# add two extra stop words: 'available' and 'via'
myStopwords <- c(stopwords("english"), "available", "via")
# remove 'r' and 'big' from stopwords
myStopwords <- setdiff(myStopwords, c("r", "big"))
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
#
#￼# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)

