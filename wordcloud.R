database <- data.frame(Name = c('Aalto Fellows II', 'Aalto introduction to Services'),
                       Description = c('This course is a lot of words I do not know.','Service economy, whatever it does mean.'),
                       LearningOutcomes = c(as.character(worst_news), as.character(best_news)), stringsAsFactors = FALSE)

database
# cool packages
library(tidytext)
library(dplyr)

# here the text transformations for titles
title <- tibble(line = 1:nrow(database), text = database$Name) %>%        # as tibble
                unnest_tokens(word, text)%>%                                     # remove punctuations, lowercase, put words in column
                anti_join(stop_words, by = c("word" = "word")) %>%               # remove stopwords
                group_by(line) %>% summarise(title = paste(word,collapse =' '))  # now all in a row!

# here the text transformations for descriptions
description <- tibble(line = 1:nrow(database), text = database$Description) %>%
                unnest_tokens(word, text) %>%  
                anti_join(stop_words, by = c("word" = "word"))  %>%
                group_by(line) %>% summarise(title = paste(word,collapse =' '))

# here the text transformations for learning outcomes
learningoutcomes <- tibble(line = 1:nrow(database), text = database$LearningOutcomes) %>% 
                      unnest_tokens(word, text) %>%
                      anti_join(stop_words, by = c("word" = "word"))  %>%
                      group_by(line) %>% summarise(title = paste(word,collapse =' '))

# now the full dataset
database2 <- title %>% left_join(description, by = 'line') %>% left_join(learningoutcomes, by = 'line')
colnames(database2) <- c("line","Name","Description","LearningOutcomes")
database2


ws_ns <- database2[1, 4]
print(ws_ns)




# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
clean.text(bigcorp)

# get worst news
worst_news <- df.news[1091, 4]
worst_news

# get best news
best_news <- df.news[424, 4]
best_news


# Load the data as a corpus
docs <- VCorpus(VectorSource(as.character(ws_ns)))

inspect(docs)

# text transforming
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, "“")
docs <- tm_map(docs, toSpace, "’s")
docs <- tm_map(docs, toSpace, "’re")

# Cleaning the text

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
docs

# Remove numbers
docs <- tm_map(docs, removeNumbers)

# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("will","can","new", "also","said", "now", "just", "one", "’re", "might", "get", "’s", "“")) 

# Remove punctuations
docs <- tm_map(docs, removePunctuation)
docs <- gsub("[[:punct:][:blank:]]+", " ", docs)
docs[1]
# Eliminate extra white spaces
docs <- tm_map(docs[1], stripWhitespace)

# Text stemming
#docs <- tm_map(docs, stemDocument)


#
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# wordcloud
set.seed(1234)
wc_worst <- wordcloud(words = d$word, freq = d$freq, min.freq = 1,
                      max.words=200, random.order=FALSE, rot.per=0.35, 
                      colors=brewer.pal(8, "Dark2"))
ggsave(wc_worst, path = "plots/", filename = "wordcloud.png")



findFreqTerms(dtm, lowfreq = 4)
#findAssocs(dtm, terms = "freedom", corlimit = 0.3)

barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")



