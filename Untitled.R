#twitter sentiment
library(twitteR)
library(dplyr)
library(lubridate)
# library(streamgraph)
# library(htmlwidgets)

setup_twitter_oauth("3WPz008PGXzSXeSYU6fN5CWmM", "kPLkxUBD4sYUDhgPYXYYAFTGVeHrX8zmeMT2FugGyE2LH4NFFZ", access_token="166190693-Kuc7uBiSUcKYd5zU9YFeSlPbwyDfxkcAlCTBSdIS", access_secret="zyyMDBzzvAxTU2iKMclycNUcWmlq5Pwzk4JH075kreazu")
register_sqlite_backend("~/sqlit_file")

db_name = "JMEI_tweets"
n = search_twitter_and_store("$JMEI", db_name)
tweets <- load_tweets_db(as.data.frame = T, table_name=db_name)
dim(tweets)

#bashtags_list <- regmatches(tweets$text, gregexpr("#[[:alnum:]]+", tweets$text))

# sentiment function that outputs positive or negtive number of twitts
sentiment <- function(text){
#  tweets = filter(t, t$created >= format(Sys.time()-time_frame*3600, " %Y-%m-%d %H:%M:%S"))
#  text = tweets$text
  positive_t <-  (grepl("BUY", text, perl=TRUE, ignore.case=TRUE) | 
                    grepl("upside", text, perl=TRUE, ignore.case=TRUE) |
                    grepl("upgrade", text, perl=TRUE, ignore.case=TRUE) |
                   grepl("gain", text, perl=TRUE, ignore.case=TRUE) |
                    grepl("out-*perform", text, perl=TRUE, ignore.case=TRUE)) 
  negtive_t <-  (grepl("sell", text, perl=TRUE, ignore.case=TRUE) | 
                    grepl("downside", text, perl=TRUE, ignore.case=TRUE) |
                    grepl("downgrade", text, perl=TRUE, ignore.case=TRUE) |
                    grepl("lose", text, perl=TRUE, ignore.case=TRUE) |
                   grepl("under-*perform", text, perl=TRUE, ignore.case=TRUE))  
#  print(length(text))
  return(sum(positive_t)) #(c(sum(positive_t), sum(negtive_t) , length(text)))
}

get_url<- function(text){
  m <- gregexpr("http://[[:alnum:]]+.[[:alnum:]]*/*[[:alnum:]]*", text, perl=TRUE)
  return(regmatches(text, m))
}

# get the page content if there is link
for i in 1:length(tweets$text){
  if (length(get_url(tweets$text[i])[[1]])>0) {
    for url in get_url(tweets$text[i])[[1]] tweets$text[i] = paste(tweets$text[i], readLines(url))
  }
}
readLines(x[[1]][1])

tweets_sentiment <- tweets %>% 
                # group by month
                mutate(year_month = paste0(format(as.Date(created), format="%Y-%m"), "-01")) %>%
                group_by(year_month) %>%
                summarise(sumstats = sentiment(text))