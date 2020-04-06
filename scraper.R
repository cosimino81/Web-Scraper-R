

#Loading the rvest package
library('rvest')
library('stringr')
library('xml2')

#Specifying the url for desired website to be scraped (The New Your Times)
ny_times_url <- 'https://www.nytimes.com/section/health'
gardian_url <- 'https://www.theguardian.com/world/coronavirus-outbreak'

#Reading the HTML code from the website
list_of_pages <- str_c(gardian_url, '?page=', 1:100)
list_of_pages

# set vector for df columns
url_vect = c()
date_vect = c()
title_vect = c()
body_vect = c()

#list_of_pages[1:2]

for (lk in list_of_pages[1:10]) {
  
  # open first link link
  main_webpage <- read_html(lk)
  
  # extract link for every title
  link_news <- html_nodes(main_webpage, '.fc-item__header')
  link_news <- html_nodes(link_news, '.fc-item__link')
  link_news <- html_attr(link_news, 'href')
  #print(link_news)
  
  # extract news title
  news.t <- html_nodes(main_webpage, '.fc-item__title')
  news.t <- unique(html_text(news.t))
  #print(news.t)
  
  for (link in link_news) {
    
    print(link)
    
    # open each news link
    article_webpage <- read_html(link)
    
    # get the news title
    news.title <- html_nodes(article_webpage, '.content__headline')[1]
    news.title <- html_text(news.title)
    print(news.title)
    
    # get the date
    pub.date <- html_nodes(article_webpage, '.content__dateline') 
    pub.date <- html_nodes(pub.date, 'time')
    pub.date <- html_attr(pub.date, 'datetime')[1]
    print(pub.date)
    
    if (identical(pub.date, character(0))) {
       news.date <- "No date"
    } else{
      news.date <- pub.date
    }
    print(news.date)
    
    # get sharing
    # news.sharing <- html_nodes(article_webpage, '.meta__extras')
    # print(news.sharing)
    # 
    # news.sharing <- html_nodes(news.sharing, '.meta_numbers')
    # print(news.sharing)
    # news.sharing <- html_nodes(news.sharing, '.meta__number')
    # print(news.sharing)
    
    # get the news body 
    news.content <- (html_text(html_nodes(article_webpage, '.js-article__body')))
    
    if (identical(news.content, character(0))) {
      news.body <- ("No text")
      #print(news.body)
    } else{
      news.body <- news.content 
    }
    print(news.body)
    print("-------------------")
    
    # append new data to the vector
    url_vect <- c(url_vect, link)
    date_vect <- c(date_vect, news.date)
    #title_vect <- c(title_vect, news.title)
    body_vect <- c(body_vect, news.body)
    
    
  }
  
  title_vect <- c(title_vect, news.t)
  
}

length(url_vect)
length(date_vect)
length(title_vect)
length(body_vect)

url_vect
date_vect
title_vect
body_vect


df <- data.frame(url_vect, date_vect, title_vect, body_vect)
df


write.csv(df, "dati.csv")





