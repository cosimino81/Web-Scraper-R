library(janeaustenr)
library(dplyr)
library(stringr)

tidy_books <- austen_books() %>%
                                group_by(book) %>%
                                mutate(linenumber = row_number(),
                                       chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]", 
                                                                               ignore_case = TRUE)))) %>%
                                ungroup() %>%
                                unnest_tokens(word, text)

print(tidy_books)



nrc_joy <- get_sentiments("nrc") %>%  filter(sentiment == "joy")
print(nrc_joy)

tidy_books %>%
              filter(book == "Sense & Sensibility") %>%
              inner_join(nrc_joy) %>%
              count(word, sort = TRUE)

print(tidy_books)


library(tidyr)

jane_austen_sentiment <- tidy_books %>%
                                    inner_join(get_sentiments("bing")) %>%
                                    count(book, index = linenumber %/% 80, sentiment) %>%
                                    spread(sentiment, n, fill = 0) %>%
                                    mutate(sentiment = positive - negative)

print(jane_austen_sentiment)


