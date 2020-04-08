# load in the libraries we'll need
library(tidyverse)
library(tidytext)
library(tidyr)
library(glue)
library(stringr)

# get a list of the files in the input directory
df.news <-read.csv("partial_covid19_news.csv", header=TRUE, sep=",")
df.news$X <- NULL

str(df.news)

head(df.news)

any(is.na(df.news$Date))

# create new empty columns
df.news$negative <- rep(NA, length(rownames(df.news)))
df.news$positive <- rep(NA, length(rownames(df.news)))
df.news$sentiment <- rep(NA, length(rownames(df.news)))

i <- 1
for (rw in df.news$Body) {
        
      
      newsBody <- df.news$Body[i]
      newsBody
      
      # get rid of any sneaky trailing spaces
      fileName <- trimws(newsBody)
      
      # read in the new file
      fileText <-  fileName #glue(fileName)
      
      # remove any dollar signs (they're special characters in R)
      fileText <- gsub("\\$", "", fileText) 
      
      # tokenize
      tokens <- tibble(text = fileText) %>% unnest_tokens(word, text)
      print(tokens)
      
      if (tokens$word[1] == "na") {
        
          last.tokens <- data_frame("word" = rep("hello", 10))
          print(last.tokens)
          
      } else{
        last.tokens <- tokens
      }
      
      print(last.tokens)
      
      # Now that we have a list of tokens, we need to compare them against a list of words with either positive or negative sentiment.
      # get the sentiment from the first text: 
      senti.table <- last.tokens %>% inner_join(get_sentiments("bing")) %>% count(sentiment) 
      senti.table
      
      if (is.null(senti.table$positive)) {
        
        senti.table$positive <- 0
        
      }  else if (is.null(senti.table$negative)) {
        
        senti.table$negative <- 0
      }                         
      
      
      senti.table <- spread(senti.table, sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative) 
      
      print(i)
      print(senti.table)
      
      
      df.news[i, 5] <- senti.table$negative
      df.news[i, 6] <- senti.table$positive
      df.news[i, 7] <- senti.table$sentiment
      
      i <- i+1
      
}


head(df.news)

library(ggplot2)

ggplot(data = df.news, aes(x = Date, y = sentiment)) +
       geom_col(stat="identity", fill="steelblue")




