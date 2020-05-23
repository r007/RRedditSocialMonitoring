library(magrittr)
library(BBmisc)
library(curl)
library(dplyr)
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

# Generate the list for curl query
generate_urls <- function(ids) {
   paste("https://api.reddit.com/api/info.json?id=", apply(ids, 1, paste, collapse = ","), sep = "")
}

results <- list()

# Callback in case of success
success <- function(x) {
   json <- fromJSON(rawToChar(x$content))
   results <<- append(results, list(json))
   print(paste("Response:", x$status_code, "Time:", x$times[6], "Post count:", count(json[['data']][['children']])))
}

# Callback in case of failure
failure <- function(x) {
   cat(paste("Failed request:", str), file = stderr())
}

# Execute the query, run and save the results
exec_reddit_query <- function(urls) {
   for (url in urls) {
      reddit_handle <- new_handle(url = url)
      multi_add(reddit_handle, done = success, fail = failure)
   }
   multi_run()
}

get_latest_reddit_post_id() %>%
   generate_ids() %>%
   generate_urls() %>%
   exec_reddit_query()

