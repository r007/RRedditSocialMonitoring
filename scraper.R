library(BBmisc)
library(curl)
library(jsonlite)

# Get latest ID in decimal format
get_latest_reddit_post_id <- function() {
   resp <- fromJSON("https://www.reddit.com/r/all/new/.json?limit=1")
   latest <- resp[['data']][['children']][1,][['data']][['id']]
   strtoi(latest, base = 36L)
}

# Transform decimal ID to base 36 and add t3_ before it
decimalIdToBase36 <- function(elem) {
   paste("t3_", itostr(elem, base = 36), sep = "")
}

# Generate the list of post IDs to query
generate_ids <- function(latest_id, batches = 20, posts = 100) {
   cat(paste("Starting id:", latest_id, "\n"))

   # 2000 posts, spread out over 20 batches of 100 posts/each
   ids <- matrix(
       (latest_id - ( batches * posts - 1 ) ):latest_id,
       nrow = batches
   )
   # Apply transformation function to the whole matrix
   ids <- apply(ids, 2, decimalIdToBase36)
   ids
}

id <- get_latest_reddit_post_id()
print(generate_ids(id))
