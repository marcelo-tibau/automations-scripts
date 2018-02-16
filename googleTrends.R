# setwd("~/Documents/FLAGCX/Projects/mkay")

library(dplyr)
library(zoo)
library(ggplot2)

# time series based on Google Trends keywords
googleTrends <- read.csv("multiTimeline.csv", row.names = NULL)
names(googleTrends) <- c("Semana", "CosmeticosMaryKayBrasil")
googleTrends <- googleTrends[-c(1), ]

googleTrends2018 <- googleTrends[c(48:53), ]
googleTrends2018 <- googleTrends2018[-c(6), ]

# rating
cleanGoogleTrends <- googleTrends2018 %>%
  mutate(ind = row_number()) %>%
  group_by(ind) %>%
  mutate(dates = strsplit(Semana, " - "),
         start = dates[[1]][1] %>% strptime("%Y-%m-%d") %>% as.character()) %>%
  filter(!is.na(start) | is.na(end)) %>%
  ungroup() %>%
  select(-c(ind, dates, Semana)) %>%
  mutate(start = as.Date(start),
         score = as.numeric(CosmeticosMaryKayBrasil))

# ploting
ggplot(aes(x = start, y = score), data = cleanGoogleTrends) +
  geom_line(size = 0.5)


# related topics
entities <- read.csv("relatedEntities.csv", row.names = NULL)
names(entities) <- c("Top", "Count")
entities <- entities[-c(1:2), ]

write.csv(entities, file = "entities.csv")
df <- read.csv("entities.csv")