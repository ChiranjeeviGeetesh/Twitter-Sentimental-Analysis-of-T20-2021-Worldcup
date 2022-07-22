library("openssl")
library("httpuv")
library("twitteR")
library("base64enc")
consumer_key<-'PAxGjpoell9F6LRAcIrA6cgym'
consumer_secret<-'bWGrbXyMLXH7qizsSiKLCcWMup8Tq7v2Mo7XXAyzYdShiKjIAB'
access_token<-'1509821233428598788-6ZdR5sVvGP3hnyWg7F6R4k06GKaZM4'
access_secret<-'tH2fxeAUiE3d31oLDme5VlnffquIhd3J1jnWNRbcqwhJt'

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
origop <- options("httr_oauth_cache")
options(httr_oauth_cache = TRUE)

worldcup<-read.csv('T20_Worldcup_tweets.csv')
install.packages('tm')
library(tm)
corpus<-iconv(worldcup$text,to="utf-8")
corpus<-Corpus(VectorSource(corpus))
inspect(corpus[1:5])

corpus<-tm_map(corpus,tolower)
inspect(corpus[1:5])
corpus<-tm_map(corpus,removePunctuation)
inspect(corpus[1:5])
cleanset<-tm_map(corpus,removeWords,stopwords('english'))
inspect(cleanset[1:5])

removeURL<-function(x) gsub('http[[:alnum:]]*','',x)
cleanset<-tm_map(cleanset,content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset<-tm_map(cleanset,removeWords,c('t20worldcup','\n'))
cleanset<-tm_map(cleanset,stripWhitespace)
tdm<-TermDocumentMatrix(cleanset)
tdm
tdm<-as.matrix(tdm)
tdm[1:10,1:20]

w<-rowSums(tdm)
w<-subset(w,w>=25)
barplot(w,las=2,col=rainbow(50))

library(wordcloud)
w<-sort(rowSums(tdm), decreasing=TRUE)
set.seed(222)
wordcloud(words=names(w),freq=w,max.words=450,random.order=F,
          min.freq=5,colors=brewer.pal(8,'Dark2'))


library(syuzhet)
library(lubricate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

scores<-get_nrc_sentiment(cleanset[1:10000])

barplot(colSums(scores),las=2,col=rainbow(10),
        ylab='Count',main='Sentiment Scores For T20 Worldcup 2021 tweets')