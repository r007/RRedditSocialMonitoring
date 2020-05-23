library(curl)
library(jsonlite)

get_latest_reddit_post_id <- function() {
   resp <- fromJSON("https://www.reddit.com/r/all/new/.json?limit=1")
   latest <- resp[['data']][['children']][1,][['data']][['id']]
   strtoi(latest, base = 36L)
}

get_latest_reddit_post_id()