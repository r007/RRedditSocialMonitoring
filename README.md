# RRedditSocialMonitoring

This is the backend scraper script to monitor submissions with specific words or phrases. To see mentions the moment they're made. Tracker search all of Reddit by default.

## How it works?

It scraps every post from Reddit in real time. Reddit gets about 500,000 new posts and 4,000,000 new comments every day. Combined, that's about 50/second. The posts don't come in at a contant rate, though, so we'd better be able to handle a few hundred a second.

Reddit offers a nice JSON API. We'll just use that to grab the most recent posts. It's at https://www.reddit.com/r/all/new/.json, and I recommend you open that up in your browser to follow along.