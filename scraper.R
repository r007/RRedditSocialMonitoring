suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(BBmisc))
suppressPackageStartupMessages(library(curl))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(stringi))
suppressPackageStartupMessages(library(paws.database))

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

# Configure access to DynamoDB database
svc <- dynamodb(
  config = list(
    credentials = list(
      creds = list(
        access_key_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
        secret_access_key = Sys.getenv("AWS_SECRET_ACCESS_KEY")
      ),
      profile = Sys.getenv("AWS_PROFILE")
    ),
    endpoint = Sys.getenv("AWS_ENDPOINT"),
    region = Sys.getenv("AWS_DEFAULT_REGION")
  )
)

# Query latest keywords from the database
scanResult <- svc$scan(
  ExpressionAttributeNames = list(
    `#ID` = "id",
    `#NA` = "name"
  ),
  ProjectionExpression = "#ID, #NA",
  TableName = Sys.getenv("AWS_TABLE_NAME")
)

results <- data.frame(matrix(ncol = length(scanResult$Items), nrow = 0))
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
	      combined <- paste(title, text, sep = ". ")

         # Search for match in title and in text
         matches <- sapply(scanResult$Items, function(x) stri_detect_fixed(combined, x$name$S, case_insensitive = TRUE), simplify = TRUE)
	 
         # See if we have any matches
	      if (sum(matches)) {
	         # Add post ID to results table
            post_id <- rep(post[row, "id"], length(scanResult$Items))
	         post_id[!matches] <- NA
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