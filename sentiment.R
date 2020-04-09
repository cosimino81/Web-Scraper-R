# load in the libraries we'll need
library(tidyverse)
library(tidytext)
library(tidyr)
library(glue)
library(stringr)
library(ggplot2)

# get a list of the files in the input directory
df.news <-read.csv("partial_covid19_news.csv", header=TRUE, sep=",")
df.news$X <- NULL

str(df.news)

head(df.news)
tail(df.news)

any(is.na(df.news$Date))

# create new empty columns
df.news$negative <- rep(NA, length(rownames(df.news)))
df.news$positive <- rep(NA, length(rownames(df.news)))
df.news$sentiment <- rep(NA, length(rownames(df.news)))


i <- 1
for (rw in df.news$Body) {
        
      print(i)
      newsBody <- df.news$Body[i]
      
      # get rid of any sneaky trailing spaces
      fileName <- trimws(newsBody)
      
      # read in the new file
      fileText <-  fileName #glue(fileName)
      
      # remove any dollar signs (they're special characters in R)
      fileText <- gsub("\\$", "", fileText) 
      
      # tokenize                                                      
      tokens <- tibble(text = fileText) %>% unnest_tokens(word, text) %>% count(word, sort = TRUE)
      
      if (tokens$word[1] == "na") {
        
          last.tokens <- data_frame("word" = rep("hello", 10))
          print(last.tokens)
          
      } else{
        last.tokens <- tokens
      }
      
      print(last.tokens)
      
      
      # Now that we have a list of tokens, we need to compare them against a list of words with either positive or negative sentiment.
      # get the sentiment from the first text: 
      senti.table <- last.tokens %>% inner_join(get_sentiments("bing")) 
      
      
      #
      if (!("positive" %in% senti.table$sentiment)){
        
          senti.table <- senti.table %>% add_row(word = "hello", n = 1, sentiment = "positive")
      }
      
      if (!("negative" %in% senti.table$sentiment)){
        
        senti.table <- senti.table %>% add_row(word = "hello", n = 1, sentiment = "negative")
      }
      
      
      #
      senti.table <- spread(senti.table, sentiment, n, fill = 0) %>% mutate(sentiment = positive - negative) 
      
      #
      last.senti.table <- senti.table %>% summarise(negative = sum(negative),
                                                    positive = sum(positive),
                                                    sentiment = sum(sentiment))
      # 
      print(last.senti.table)
      
      
      df.news[i, 5] <- last.senti.table$negative
      df.news[i, 6] <- last.senti.table$positive
      df.news[i, 7] <- last.senti.table$sentiment
      
      i <- i+1
      
}

# check the df
class(df.news)
str(df.news)
head(df.news)
tail(df.news)
df.news[1091, ]

#######  --------- Visualization -------------------
df_senti <- df.news %>% summarise(negative = sum(negative),
                                  positive = sum(positive))


df_senti
class(df_senti)

# transpose df
df_senti <- as.data.frame(t(df_senti))
df_senti
colnames(df_senti) <- "total"
df_senti

# reindexing the df
df_senti$sentiment <- rownames(df_senti)
rownames(df_senti) <- 1: length(rownames(df_senti))
df_senti

# barplot df_senti total words
bp <- ggplot(data =  df_senti, aes( x = sentiment, y = total, label = total)) +
                         geom_bar(stat="identity", width=.5, fill="blue") +
                         geom_text(color="black", size=5) +
                         labs(title="Sentiment by bing method", 
                         subtitle="Count of positive and negative words") +
                         ylab("Number of words")
ggsave(bp, filename = "01_barplot.png", path = "plots/")


# add new column to the df.news
head(df.news)
df.news$lineindex <- 1:length(rownames(df.news))


# set coordinates for annotation
zero_date <- df.news$Date[1]
one_date <- df.news$Date[1000]
two_date <- df.news$Date[2000]
three_date <- df.news$Date[3000]
four_date <- df.news$Date[4000]
five_date <- tail(df.news$Date, n=1)



# sentiment for every news
st <- ggplot(data= df.news, aes(x = lineindex, y = sentiment)) +
                      geom_line(color = "red", size = 0.1) +
                      labs(title = "Sentiment on Single News", subtitle = "On release date") +
                      xlab("Number of news") +
                      theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
                      annotate(geom="text", x=10,   y=50, label= zero_date, color="black", size = 3.5) +
                      annotate(geom="text", x=1000, y=50, label= one_date, color="black", size = 3.5) +
                      annotate(geom="text", x=2000, y=50, label= two_date, color="black", size = 3.5) +
                      annotate(geom="text", x=3000, y=50, label= three_date, color="black", size = 3.5) +
                      annotate(geom="text", x=4000, y=50, label= four_date, color="black", size = 3.5) +
                      annotate(geom="text", x=4700, y=50, label= five_date, color="black", size = 3.5) 

ggsave(st, filename = "02_lineplot.png", path = "plots/")




# News grouped by date
df.grouped <- df.news %>%
                        group_by(Date) %>%
                        summarise(sentiment = sum(sentiment),
                                  n.news = n())
head(df.grouped)
tail(df.grouped)
str(df.grouped)
df.grouped$sentiment
news_numb <- df.grouped$n.news

gp <- ggplot(data = df.grouped, aes(x = Date, y = sentiment)) +
                          geom_col(fill = "red") +
                          labs(title = "Sentiment on Aggregated News", 
                               subtitle = "Each bar has number of releases") +
                          xlab("Release Date") +
                          ylab("Number of Negative Words")+
                          theme(axis.text.x = element_text(angle=85, vjust=0.6)) +
                          annotate(geom="text", x=1:89,   y=0, label= news_numb, color="black", size = 1.5)
 
ggsave(gp, filename = "03_barplot.png", path = "plots/", width = 10)               


# df sorted
df_sorted <- df.news[ , c(2,3,5,6,7)]
df_sorted <- df_sorted[order(df_sorted$sentiment, decreasing = T), ]
df_sorted_bad <- tail(df_sorted, 10)
df_sorted_good <- head(df_sorted, 10)



# most negative news
ng <- ggplot(df_sorted_bad, aes(x= Title, y= sentiment, label = sentiment)) + 
              geom_point(stat='identity', size=7, color = "blue")  +
              geom_text(color="white", size=3) +
              labs(title="Most Negative News", 
                   subtitle="Diverging Dot Plot") + 
              coord_flip()

ggsave(ng, filename = "04_dotplot.png", path = "plots/", width = 10)



# most positive news
ps <- ggplot(df_sorted_good, aes(x= Title, y= sentiment, label = sentiment)) + 
                      geom_point(stat='identity', size=7, color = "red")  +
                      geom_text(color="white", size=3) +
                      labs(title="Most Positive News", 
                           subtitle="Diverging Dot Plot") + 
                      coord_flip()

ggsave(ps, filename = "05_barplot.png", path = "plots/", width = 10)
