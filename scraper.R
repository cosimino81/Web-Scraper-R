

#Loading the rvest package
library('rvest')
library('stringr')

#Specifying the url for desired website to be scraped (The New Your Times)
ny_times_url <- 'https://www.nytimes.com/section/health'
gardian_url <- 'https://www.theguardian.com/world/coronavirus-outbreak'

#Reading the HTML code from the website
#webpage <- read_html(gardian_url)

list_of_pages <- str_c(gardian_url, '?page=', 1:100)
list_of_pages

# open first link link
webpage <- read_html(list_of_pages[1])
webpage

titoli <- html_nodes(webpage, '.fc-item__title')
titoli
titoli <- html_text(titoli)
titoli

news <- html_nodes(webpage, '.fc-item__content ')
news

news_title <- html_text(news)
news_title

link.news <- html_nodes(news, 'a')
link.news

link_all_news <- html_attr(link.news, 'href')
link_all_news <- unique(link_news)
link_all_news



url_vect = c()
date_vect = c()
title_vect = c()
body_vect = c()


for (link in link_all_news) {
  
  print(link)
  
  # open each news link
  article_webpage <- read_html(link)
  
  # get the news title
  news.title <- html_nodes(article_webpage, '.content__headline')[1]
  news.title <- html_text(news.title)
  print(news.title)
  
  # get the date
  news.date <- html_nodes(article_webpage, '.content__dateline')
  news.date <- html_text(news.date)
  print(news.date)
  
  # get the news body 
  news.body <- (html_text(html_nodes(article_webpage, '.js-article__body')))
  
  if (identical(news.body, character(0))) {
    news.body <- ("No text")
    print(news.body)
  } else{
    print(news.body) 
  }
  print("-------------------")
  
  # # create a vector
  url_vect <- c(url_vect, link)
  date_vect <- c(date_vect, news.date)
  title_vect <- c(title_vect, news.title)
  body_vect <- c(body_vect, news.body)
  
}


df <- data.frame(url_vect, date_vect, title_vect, body_vect)
df


write.csv(df, "dati.csv")
### --------- SCRAPING -----------------
#Using CSS selectors to scrape news date
news_date_html <- html_nodes(webpage, '')
news_date_html
news_date_html <- html_attr(news_date_html,'span')
news_date_html

news_date_html <- html_attr(news_date_html, 'href')
news_date_html

news_date <- vector()
i <- 1
for (el in news_date_html) {
        data <- strsplit(el, '/')[[1]][2:4]
        
        data <- (paste(data, collapse = "/"))
        news_date[i] <- data
        i <- i+1
}
length(news_date)

#Using CSS selectors to scrape news title
news_title_html <- html_nodes(webpage,'.e1xfvim30')
news_title_html

#Using CSS selectors to scrape news body
news_body_html <- html_nodes(webpage, '.e1xfvim31')
news_body_html

### -------- CONVERTING TEXT ----------------

#Converting the date to text
data_to_text <- html_text(news_date_html)
data_to_text

#Converting the title to text
title_to_text <- html_text(news_title_html)
title_to_text

#Converting body to text
body_to_text <- html_text(news_body_html)
body_to_text

#Let's have a look at the rankings
news_date
head(title_to_text)
head(body_to_text)

