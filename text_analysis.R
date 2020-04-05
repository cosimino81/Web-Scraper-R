
############################
# TEXTUAL ANALYSIS
#############################
getwd()
install.packages("tm")
library(tm)
all<-read.csv("guardian_coronavirus.csv", header=TRUE, sep=";")

docs<-VCorpus(VectorSource(all$body))
#carico il documento
#VERIFICO COSA C'è SCRITTO NEL DOC 140
inspect(docs[[140]])


toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|") 

docs <- tm_map(docs, content_transformer(tolower))

docs <- tm_map(docs, removeNumbers)

docs <- tm_map(docs, removeWords, stopwords("english"))
#stopwords("italian")
#docs <- tm_map(docs, removeWords, c("xxx", "xxx")) 

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)

<<TermDocumentMatrix (terms: 31719, documents: 2107)>>
  Non-/sparse entries: 613829/66218104
Sparsity           : 99%#  1-(613829/66218104+613829)
Maximal term length: 94
Weighting          : term frequency (tf)##IMPORTANTE!!!





m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

docs <- tm_map(docs, removeWords, c("will","can", "also","said", "now")) 
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Parole più frequenti",
        ylab = "frequenza delle parole")
library(wordcloud)       
library(RColorBrewer)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, scale=c(3, .2),min.freq = 3,
          max.words=100, random.order=FALSE, rot.per=0.15, use.r.layout=FALSE, colors=brewer.pal(8,"Dark2"))


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



findFreqTerms(dtm, lowfreq = 4)#parole che si veriifcano almeno 4 volte  
findAssocs(dtm_clean, terms = "italy", corlimit = 0.3)#associazione tra parole 

inspect(DocumentTermMatrix(docs,
                           list(dictionary = c("coronavirus", "quarantine", "italy"))))
#come si trova il documento 52?
inspect(docs[[52]]) 