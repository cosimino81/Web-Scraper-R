# load in the libraries we'll need
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)

# get a list of the files in the input directory
df <-read.csv("covid19_guardian_news.csv", header=TRUE, sep=",")
str(df)
head(df)
newsBody <- df$Body[10]
newsBody



# get rid of any sneaky trailing spaces
fileName <- trimws(newsBody)
fileName

# read in the new file
fileText <- glue(fileName)
fileText

# remove any dollar signs (they're special characters in R)
fileText <- gsub("\\$", "", fileText) 

# tokenize
tokens <- data_frame(text = fileText) %>% unnest_tokens(word, text)
tokens


# Now that we have a list of tokens, we need to compare them against a list of words with either positive or negative sentiment.
# get the sentiment from the first text: 
senti.table <- tokens %>%
                        inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
                        count(sentiment) %>% # count the # of positive & negative words
                        spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
                        mutate(sentiment = positive - negative) # # of positive words - # of negative owrds

senti.table




