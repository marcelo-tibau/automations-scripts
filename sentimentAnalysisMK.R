# setwd("~/Documents/FLAGCX/Projects/mkay/dataset-master")
# Use with script twitterAnalysisMK.R
# Portuguese lexicom clean-up
library('stringr')
palavrasPositivas <- read.table("palavrasPositivas.txt", header=FALSE)
p1 <- gsub("[[]]", "", as.character(palavrasPositivas$V1))
p2 <- as_data_frame(p1)

# Lexicom as dictionary

nice <- scan("palavrasPositivas.txt", what = 'character', comment.char = '')
notNice <- scan("palavrasNegativas.txt", what = 'character', comment.char = '')

# Scoring sentiment
sentiments <- function(words, niceText, notNiceText) {
  positive = match(words, niceText)
  negative = match(words, notNiceText)
  
  positive = !is.na(positive)
  negative = !is.na(negative)
  
  score = sum(positive) - sum(negative)
  
  return(score)
}

# sentiment plot
sentiments(dftF$term, nice, notNice)

sentimentsDF <- data_frame(Score = sentiments(dftF$term, nice, notNice), Twitter = "Mary Kay")

ggplot(sentimentsDF) +
  geom_col(aes(y = Score, x = Twitter, fill = Score)) +
  theme_bw()

