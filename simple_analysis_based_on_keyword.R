#twitter sentiment
library(twitteR)
library(dplyr)
library(lubridate)
library(RSQLite)
# library(streamgraph)
# library(htmlwidgets)
path = "C:/Users/baby/OneDrive/Documents/others/stock_sentiment/"
setwd(path)

key_file = file(paste0(path, "keys.txt"))
keystr = strsplit(readLines(key_file, n=1), ",")[[1]]
consumer_key = keystr[1]
consumer_secret=keystr[2]
access_token=keystr[3]
access_secret=keystr[4]
close(key_file)

setup_twitter_oauth(consumer_key, consumer_secret, access_token=access_token, access_secret=access_secret)
register_sqlite_backend("stock_sentiment")

#db_name = "JMEI_tweets"
#n = search_twitter_and_store("$JMEI", db_name)

# db_name = "KORS_tweets"
# n = search_twitter_and_store("$KORS", db_name)

db_name = "CCL_tweets"
n = search_twitter_and_store("$CCL", db_name)

tweets <- load_tweets_db(as.data.frame = T, table_name=db_name)
dim(tweets)

#bashtags_list <- regmatches(tweets$text, gregexpr("#[[:alnum:]]+", tweets$text))

# sentiment function that outputs positive or negtive number of twitts
sentiment <- function(text, deeplink=T){
   #  tweets = filter(t, t$created >= format(Sys.time()-time_frame*3600, " %Y-%m-%d %H:%M:%S"))
   #  text = tweets$text
#  if grepl("$") {}
  
  pos_text <-  (grepl("buy", text, perl=TRUE, ignore.case=TRUE) | 
                  grepl("bull", text, perl=TRUE, ignore.case=TRUE) |
                    grepl("upside", text, perl=TRUE, ignore.case=TRUE) |
                    grepl("upgrade", text, perl=TRUE, ignore.case=TRUE) |
                    grepl("overweight", text, perl=TRUE, ignore.case=TRUE) |
                    grepl("out-*perform", text, perl=TRUE, ignore.case=TRUE)|
                  grepl("good", text, perl=TRUE, ignore.case=TRUE)) 
  neg_text <-  (grepl("sell", text, perl=TRUE, ignore.case=TRUE) | 
                   grepl("bear", text, perl=TRUE, ignore.case=TRUE) |
                   grepl("downside", text, perl=TRUE, ignore.case=TRUE) |
                   grepl("downgrade", text, perl=TRUE, ignore.case=TRUE) |
                   grepl("underweight", text, perl=TRUE, ignore.case=TRUE) |
                   grepl("under-*perform", text, perl=TRUE, ignore.case=TRUE) |
                   grepl("bad", text, perl=TRUE, ignore.case=TRUE)) 
  
#   m = regexpr("target\\s*\\w*of\\$*(\\d+\\.*\\d*)", text, perl=TRUE, ignore.case=TRUE)
#   if(m$match.length!=-1)
#     price_target= regmatches(text, m)
#   else
#     m = regexpr("\\$*(\\d+\\.*\\d*) target", text, perl=TRUE, ignore.case=TRUE)
#     if(m$match.length!=-1) price_target= regmatches(text, m)
#   
#   if (as.double(price_target) > curr_price)
#     pos_text=1
#   else
#     neg_text=1
    
  binary_score = pos_text-neg_text
  
#   # try to dig into the links
#   if (deeplink=T && binary_score==0) 
#   
  return(pos_text-neg_text)
}


tweets_sentiment <- tweets %>% 
                mutate(date = paste0(format(as.Date(created), format="%Y-%m-%d"), "")) %>%
                mutate(sentiment = sentiment(text)) 
  
library(ggplot2)
qplot(date, data=tweets_sentiment, geom="bar", fill=factor(sentiment))

#devtools::install_github("hrbrmstr/streamgraph")
# require(streamgraph)

senti_byday <- tweets_sentiment %>% group_by(date, sentiment) %>%
                summarise(n(), 
                          sentiment_score = sum(sentiment*(1+favoriteCount))
                          ) 
# %>%streamgraph("sentiment", "n", "date", offset = "zero") %>% sg_legend(show=TRUE, label="sentiment: ")


