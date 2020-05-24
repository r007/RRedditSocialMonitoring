library(magrittr)
library(BBmisc)
library(curl)
library(dplyr)
library(jsonlite)
library(stringi)

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

search <- c('can', 'have', 'had', 'with', 'without', 'probably', 'makes sense', 'think', 'wonderful', 'impressive', 'callback', 'dropshipping', 'marketing', 'china', 'ecommerce', 'coronavirus', 'can I', 'should I', 'introducing', 'shopify', 'blogging', 'affiliate', 'default value', 'the command line', 'optimizing graphics', 'how it works', 'for creating', 'for writing', 'which respects', 'should give a', 'can be used to', 'can be set to', 'not mentioned here', 'is an important', 'breaking news', 'lockdown', 'virus', 'trump', 'voting', 'promotes', 'racist', 'keep up the good', 'memorial day', 'hong kong', 'is just the start', 'our best', 'inequality', 'smarter', 'protect kids', 'reduce risk', 'more than just', 'same as before')
results <- data.frame(matrix(ncol = length(search), nrow = 0))
colnames(results) <- search
results_row <- 1

# Callback in case of success
success <- function(x) {
   children <- fromJSON(rawToChar(x$content))[['data']][['children']]

   # Extract the variables we need
   for (post in children['data']) {
      # Loop over data frame rows
      for (row in 1:nrow(post)) {
         title <- post[row, "title"]
	 text <- post[row, "selftext"]

         # Search for match in title
         matched_title <- sapply(search, function(x) stri_detect_fixed(title, x, case_insensitive = TRUE), simplify = TRUE)
         # Search for match in text
         matched_text <- sapply(search, function(x) stri_detect_fixed(text, x, case_insensitive = TRUE), simplify = TRUE)
	 
         # See if we have any matches
	 combined_results <- matched_title | matched_text
	 if (sum(combined_results)) {
	    # Add post ID to results table
            post_id <- rep(post[row, "id"], length(search))
	    post_id[!combined_results] <- NA
	    results[results_row,] <<- post_id
	    # Increment the current row counter
	    results_row <<- results_row + 1
	 }
      }
   }

   print(paste("Response:", x$status_code, "Time:", x$times[6], "Post count:", count(children), "Total matched results:", nrow(results)))
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

colSums(!is.na(results))