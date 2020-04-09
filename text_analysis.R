
############################
# TEXTUAL ANALYSIS
#############################

library(tm)
df_pt <-read.csv("partial_covid19_news.csv", header=TRUE, sep=",")
head(df_pt) 

# get worst news
worst_news <- df.news[1091, 4]
worst_news

# get best news
best_news <- df.news[424, 4]
best_news


docs<-VCorpus(VectorSource(worst_news))
docs

inspect(docs)
#carico il documento
#VERIFICO COSA C'è SCRITTO NEL DOC 140
#inspect(docs[[140]])


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|") 

docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("english"))

docs <- tm_map(docs, removeWords, c("will","can", "also","said", "now", "just", "one", "’re", "might", "get"))
#stopwords("italian")
#docs <- tm_map(docs, removeWords, c("xxx", "xxx")) 

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
dtm


# <<TermDocumentMatrix (terms: 31719, documents: 2107)>>
#   Non-/sparse entries: 613829/66218104
# Sparsity           : 99%#  1-(613829/66218104+613829)
# Maximal term length: 94
# Weighting          : term frequency (tf)##IMPORTANTE!!!





m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

docs <- tm_map(docs, removeWords, c("will","can", "also","said", "now", "just", "one", "’re", "might", "get", "new")) 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
wdf_pt <- data.frame(word = names(v),freq=v)
rownames(wdf_pt) <- 1:length(rownames(wdf_pt))
head(wdf_pt, 10)

png("plots/wn_frequent_word.png")
barplot(wdf_pt[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="blue", main ="Worst News Freqnent Words",
        ylab = "frequenza delle parole")

dev.off()

# word cloud
library(wordcloud)       
library(RColorBrewer)

set.seed(1234)
png("plots/wordcloud.png")
wordcloud(words = wdf_pt$word, freq = d$freq, scale=c(3, .2),min.freq = 3, 
          max.words=Inf, random.order=FALSE, rot.per=0.20, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))

dev.off()





dtm_clean <- removeSparseTerms(dtm, 0.8)
dtm_clean        
m1 <- as.matrix(dtm_clean)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1<- data.frame(word = names(v1),freq=v1)
head(d1, 10)
barplot(d[1:10,]$freq,  names.arg = d[1:10,]$word,
        col ="lightblue", main ="Parole più frequenti",
        ylab = "frequenza delle parole")


library(wordcloud)       
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 0,
          max.words=200, random.order=FALSE, 
          colors=brewer.pal(8, "Dark2"))          



findf_ptreqTerms(dtm, lowfreq = 4)#parole che si veriifcano almeno 4 volte  
findAssocs(dtm_clean, terms = "italy", corlimit = 0.3)#associazione tra parole 

inspect(DocumentTermMatrix(docs,
                           list(dictionary = c("coronavirus", "quarantine", "italy"))))
#come si trova il documento 52?
inspect(docs[[52]]) 