# setwd("~/Documents/FLAGCX/Projects/Automation/googlePlay")

library(tm)

# dataset setup
dataGooglePlay <- read.csv("dataGooglePlay.csv", header = TRUE, sep = ",")

# create corpus and clean-up
corpusGooglePlay <- Corpus(VectorSource(dataGooglePlay$comments.GooglePlay))
corpusGooglePlay <- tm_map(corpusGooglePlay, tolower)
corpusGooglePlay <- tm_map(corpusGooglePlay, removePunctuation)
corpusGooglePlay <- tm_map(corpusGooglePlay, removeNumbers)
corpusGooglePlay <- tm_map(corpusGooglePlay, function(x) removeWords(x, stopwords("portuguese")))
#corpusGooglePlay <- tm_map(corpusGooglePlay, stemDocument, language = "portuguese")

inspect(corpusGooglePlay)

# create Term Document Matrix
tdmGooglePlay <- as.matrix(TermDocumentMatrix(corpusGooglePlay))

# count words and create frequency table
words <- sort(rowSums(tdmGooglePlay), decreasing = TRUE)

freqTable <- data.frame(word = names(words), freq = words)

# create wordcloud
library(wordcloud)

wordcloud(words = freqTable$word, freq = freqTable$freq, min.freq = 1, random.order = FALSE,
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))

