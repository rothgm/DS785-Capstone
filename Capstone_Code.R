###Gathering Tweets
#loading and installing libraries
require(rtweet)
require(tidyverse)
require(syuzhet)
require(plotly)
library(rtweet)
library(tidyverse)
library(syuzhet)
library(plotly)

#setting up Twitter authentication
consumer_key  <- 'JsIUmRUYN9v0ESpYEUAroWXMh'
consumer_secret  <- 'vGSES0VcbYGDDRewZnA4aftsHQG89cgareoBWwTqKraMNhqUff'
access_token  <- '872828840-lUBngKg5i5E1ayZxzcfEXCNUMsGX2nEsV8aj5wph'
access_secret  <- 'VNiWKM1bmDZAompPzfNiTX6ekFnoJsdYIMS2raD5tp4w5'
auth = rtweet_bot(consumer_key, consumer_secret, access_token, access_secret)
auth_save(auth, "Capstone")

#pulling tweets
raw_tweets <- search_tweets("@JohnFetterman", n = 5000, include_rts = TRUE, retryonratelimit = TRUE, token = auth)
df_tweets <- within(raw_tweets, rm('entities','metadata','coordinates','place','retweeted_status','geo','quoted_status'))

#saving tweets to CSV
write_as_csv(df_tweets, "C:/Users/grrot/Dropbox/Grad School/DS785/Raw_Data/raw_11_12.csv", prepend_ids = TRUE, na = "", fileEncoding = "UTF-8")

###Cleaning Tweets
#loading CSVs
tweets_10_03 <- read.csv("C:/Users/grrot/Dropbox/Grad School/DS785/Raw_Data/raw_10_03.csv")
tweets_10_10 <- read.csv("C:/Users/grrot/Dropbox/Grad School/DS785/Raw_Data/raw_10_10.csv")
tweets_10_17 <- read.csv("C:/Users/grrot/Dropbox/Grad School/DS785/Raw_Data/raw_10_17.csv")
tweets_10_24 <- read.csv("C:/Users/grrot/Dropbox/Grad School/DS785/Raw_Data/raw_10_24.csv")
tweets_10_31 <- read.csv("C:/Users/grrot/Dropbox/Grad School/DS785/Raw_Data/raw_10_31.csv")
tweets_11_12 <- read.csv("C:/Users/grrot/Dropbox/Grad School/DS785/Raw_Data/raw_11_12.csv")

#create function to remove extra columns and clean text
clean_tweets <- function(x){
  #remove excess columns
  x <- x[c('created_at','full_text','retweet_count','favorite_count','lang')]
  #only look at tweets in English
  x <- x[x$lang == 'en',]
  ##clean text
  #remove URLs
  x$full_text <- str_remove_all(x$full_text, " ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)")
  #remove mentions
  x$full_text <- str_remove_all(x$full_text,"@[[:alnum:]_]{4,}")
  #remove hashtags
  x$full_text <- str_remove_all(x$full_text,"#[[:alnum:]_]+")
  #replace ampersand character reference with "and"
  x$full_text <- str_replace_all(x$full_text,"&amp;", "and")
  #remove punctuation
  x$full_text <- str_remove_all(x$full_text,"[[:punct:]]")
  #remove emojis
  x$full_text <- str_remove_all(x$full_text, '[:emoji:]')
  #remove "RT: " from beginning of retweets
  x$full_text <- str_remove_all(x$full_text,"^RT:? ")
  #Turn all words lowercase
  x$full_text <- str_to_lower(x$full_text)
  #remove stop words
  x$full_text <- gsub(paste0('\\b',tm::stopwords("en"), '\\b', collapse = '|'),'',x$full_text)
  #replace any newline characters with a space
  x$full_text <- str_replace_all(x$full_text,"\\\n", " ")
  #remove any excess whitespace
  x$full_text <- str_trim(x$full_text,"both")
  #return cleaned data
  return(x)
}

#apply function to tweet data
cleaned_10_03 <- clean_tweets(tweets_10_03)
cleaned_10_10 <- clean_tweets(tweets_10_10)
cleaned_10_17 <- clean_tweets(tweets_10_17)
cleaned_10_24 <- clean_tweets(tweets_10_24)
cleaned_10_31 <- clean_tweets(tweets_10_31)
cleaned_11_12 <- clean_tweets(tweets_11_12)

###Analyzing Tweets

#build function to get sentiment scores
sentiment_function <- function(x){
  #get sentiment using 4 different lexicons
  syu <- get_sentiment(x$full_text, method="syuzhet")
  bing <- get_sentiment(x$full_text, method="bing")
  afinn <- get_sentiment(x$full_text, method="afinn")
  nrc <- get_sentiment(x$full_text, method="nrc")
  #add scores to data frame
  x$syu <- syu
  x$bing <- bing
  x$afinn <- afinn
  x$nrc <- nrc
  #return updated data frame
  return(x)
}

#apply sentiment function
cleaned_10_03 <- sentiment_function(cleaned_10_03)
cleaned_10_10 <- sentiment_function(cleaned_10_10)
cleaned_10_17 <- sentiment_function(cleaned_10_17)
cleaned_10_24 <- sentiment_function(cleaned_10_24)
cleaned_10_31 <- sentiment_function(cleaned_10_31)
cleaned_11_12 <- sentiment_function(cleaned_11_12)

#calculate percentage of positive tweets
positive_percentage <-function(x){
  #get percentage of positive tweets for each file
  positive_10_03 <- nrow(cleaned_10_03[cleaned_10_03[x]>0,])/nrow(cleaned_10_03)
  positive_10_10 <- nrow(cleaned_10_10[cleaned_10_10[x]>0,])/nrow(cleaned_10_10)
  positive_10_17 <- nrow(cleaned_10_17[cleaned_10_17[x]>0,])/nrow(cleaned_10_17)
  positive_10_24 <- nrow(cleaned_10_24[cleaned_10_24[x]>0,])/nrow(cleaned_10_24)
  positive_10_31 <- nrow(cleaned_10_31[cleaned_10_31[x]>0,])/nrow(cleaned_10_31)
  positive_11_12 <- nrow(cleaned_11_12[cleaned_11_12[x]>0,])/nrow(cleaned_11_12)
  #create data frame
  x_pos_tweets <- data.frame(dates = c("10/03","10/10","10/17","10/24","10/31","11/08"),
                             positive_percentage = c(positive_10_03, positive_10_10, positive_10_17, positive_10_24, positive_10_31, positive_11_12))
  #return data frame
  return(x_pos_tweets)
}

syu_percentage <- positive_percentage("syu")
bing_percentage <- positive_percentage("bing")
afinn_percentage <- positive_percentage("afinn")
nrc_percentage <- positive_percentage("nrc")

#load traditional polling data and final election results
traditional_polling <- data.frame(dates = c("10/03","10/10","10/17","10/24","10/31","11/08"), 
                                  pollster = c("Monmouth University","Fabrizio, Lee & Associates","SSRS","YouGov","Monmouth University","Final Results"),
                                  favorable = c(0.49, 0.48, 0.51, 0.51, 0.48, 0.51))

#calculate correlation between percentage of positive tweets and traditional polling data
syu_cor <- cor(traditional_polling$favorable, syu_percentage$positive_percentage)
bing_cor <- cor(traditional_polling$favorable, bing_percentage$positive_percentage)
afinn_cor <- cor(traditional_polling$favorable, afinn_percentage$positive_percentage)
nrc_cor <- cor(traditional_polling$favorable, nrc_percentage$positive_percentage)

#calculate R^2 of correlation
syu_R2 <- (syu_cor)^2
bing_R2 <- (bing_cor)^2
afinn_R2 <- (afinn_cor)^2
nrc_R2 <- (nrc_cor)^2

#create data frame that shows correlation and R^2 of each sentiment analysis method
sentiment_method_df <- data.frame(method = c("syuzhet","bing","afinn","nrc"),
                                  correlation = c(syu_cor, bing_cor, afinn_cor, nrc_cor),
                                  R2 = c(syu_R2, bing_R2, afinn_R2, nrc_R2))
#sort by correlation
sentiment_method_df <- sentiment_method_df[order(sentiment_method_df$correlation,decreasing = TRUE),]

#build function to create emotion data frame
emotion_function <- function(x){
  #get emotions using NRC dictionary
  emotions <- get_nrc_sentiment(x)
  #count number of emotions
  emo_bar = colSums(emotions)
  #turn type and number of emotions into data frame
  emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
  #turn emo_sum$emotion into factor
  emo_sum$emotion = factor(emo_sum$emotion, levels = emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])
  #return emotion data frame
  return(emo_sum)
}

#use emotion_function
emotion_10_03 <- emotion_function(cleaned_10_03$full_text)
emotion_10_10 <- emotion_function(cleaned_10_10$full_text)
emotion_10_17 <- emotion_function(cleaned_10_17$full_text)
emotion_10_24 <- emotion_function(cleaned_10_24$full_text)
emotion_10_31 <- emotion_function(cleaned_10_31$full_text)
emotion_11_12 <- emotion_function(cleaned_11_12$full_text)

#plot emotion dataframe as bar graph
plot_ly(emotion_10_03, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for 10/03 Tweets")

plot_ly(emotion_10_10, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for 10/10 Tweets")

plot_ly(emotion_10_17, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for 10/17 Tweets")

plot_ly(emotion_10_24, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for 10/24 Tweets")

plot_ly(emotion_10_31, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for 10/31 Tweets")

plot_ly(emotion_11_12, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Distribution of emotion categories for 11/12 Tweets")

##average interactions
#create combined variable that is sum of favorites and retweets
cleaned_10_03$combined <- cleaned_10_03$favorite_count + cleaned_10_03$retweet_count
cleaned_10_10$combined <- cleaned_10_10$favorite_count + cleaned_10_10$retweet_count
cleaned_10_17$combined <- cleaned_10_17$favorite_count + cleaned_10_17$retweet_count
cleaned_10_24$combined <- cleaned_10_24$favorite_count + cleaned_10_24$retweet_count
cleaned_10_31$combined <- cleaned_10_31$favorite_count + cleaned_10_31$retweet_count
cleaned_11_12$combined <- cleaned_11_12$favorite_count + cleaned_11_12$retweet_count


#get average number of favorites, retweets, and combined per tweet
avg_favorites_df <- data.frame(dates = c("10/03","10/10","10/17","10/24","10/31","11/08"),
                            avg_favorites = c(summary(cleaned_10_03$favorite_count)[4],summary(cleaned_10_10$favorite_count)[4],summary(cleaned_10_17$favorite_count)[4],summary(cleaned_10_24$favorite_count)[4],summary(cleaned_10_31$favorite_count)[4],summary(cleaned_11_12$favorite_count)[4]))

avg_retweets_df <- data.frame(dates = c("10/03","10/10","10/17","10/24","10/31","11/08"),
                            avg_retweets = c(summary(cleaned_10_03$retweet_count)[4],summary(cleaned_10_10$retweet_count)[4],summary(cleaned_10_17$retweet_count)[4],summary(cleaned_10_24$retweet_count)[4],summary(cleaned_10_31$retweet_count)[4],summary(cleaned_11_12$retweet_count)[4]))

avg_combined_df <- data.frame(dates = c("10/03","10/10","10/17","10/24","10/31","11/08"),
                              avg_combined = c(summary(cleaned_10_03$combined)[4],summary(cleaned_10_10$combined)[4],summary(cleaned_10_17$combined)[4],summary(cleaned_10_24$combined)[4],summary(cleaned_10_31$combined)[4],summary(cleaned_11_12$combined)[4]))

#plot interactions by week
barplot(avg_favorites_df$avg_favorites, names.arg = avg_favorites_df$dates, xlab="Date", ylab="Avg # of Favorites", main="Avg # of Favorites by Date",ylim =c(0,120))
barplot(avg_retweets_df$avg_retweets, names.arg = avg_retweets_df$dates, xlab="Date", ylab="Avg # of Retweets", main="Avg # of Retweets by Date",ylim=c(0,30))
barplot(avg_combined_df$avg_combined, names.arg = avg_combined_df$dates, xlab="Date", ylab="Avg # of Total Interactions", main="Avg # of Total Interactions by Date", ylim=c(0,140))
