

#Loading the rvest package
library('rvest')
library('stringr')
library('xml2')

#Specifying the url for desired website to be scraped (The New Your Times)
#ny_times_url <- 'https://www.nytimes.com/section/health'
guardian_url <- 'https://www.theguardian.com/world/coronavirus-outbreak'

first_page <- read_html(guardian_url)

# page_number <- html_nodes(first_page, '.u-cf')
# print(page_number)
# page_number <- html_nodes(page_number, '.pagination__list')
# print(html_node(page_number, '.is-active'))
# print(html_text(page_number))

#Reading the HTML code from the website
list_of_pages <- str_c(gardian_url, '?page=', 1:302)
list_of_pages

# set vector for df columns
url_vect = c()
date_vect = c()
title_vect = c()
body_vect = c()

#list_of_pages[1:2]

for (lk in list_of_pages[1]) {
  
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
    #print(pub.date)
    
    if (identical(pub.date, character(0))) {
       news.date <- "No date"
    } else{
      news.date <- pub.date
    }
    print(news.date)
    
    # get sharing
    # news.sharing <- html_nodes(article_webpage, '.meta__numbers') #'.sharecount__value--short'
    # print(news.sharing)
    # news.sharing <- html_nodes(news.sharing, xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "sharecount__value--short", " " ))]')
    # print(news.sharing)
    # print(html_name(news.sharing))
    # 
    # news.sharing <- html_nodes(news.sharing, '.meta__numbers')
    # print(news.sharing)
    # 
    
    
    # get the news body 
    news.content <- html_nodes(article_webpage, '.js-article__body')
    news.content <- html_nodes(news.content, 'p')
    news.content <- html_text(news.content)
    news.content <- paste(news.content, collapse = '')
    #print(news.content)
    
    if (news.content == "") {
      news.body <- ("NA")
      #print(news.body)
    } else{
      news.body <- news.content 
    }
    #print(news.body)
    print("-------------------")
    
    # append new data to the vector
    url_vect <- c(url_vect, link)
    date_vect <- c(date_vect, news.date)
    #title_vect <- c(title_vect, news.title)
    body_vect <- c(body_vect, news.body)
    
    
  }
  
  title_vect <- c(title_vect, news.t)
  
}

# check the vectors length
length(url_vect)
length(date_vect)
length(title_vect)
length(body_vect)



# create the df with the news
df_partial <- data.frame(Url = as.character(url_vect), 
                 Date = as.Date(date_vect), 
                 Title = as.character(title_vect), 
                 Body = as.character(body_vect), stringsAsFactors = FALSE)



# remove NA values
df_partial <- dfdf_partial[-which(df_partial$Body == "NA"), ]
str(df_partial)

# check if there is any NA 
is.na.data.frame(df_partial)

# write csv file
write.csv(df_partial, "partial_covid19_news.csv")




