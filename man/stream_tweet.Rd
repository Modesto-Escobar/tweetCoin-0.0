\name{stream_tweet}
\alias{stream_tweet}
\title{
Download live stream Twitter data
}
\description{
Download public live stream Twitter data in real-time according to the selected criteria.
}

\usage{
stream_tweet(search, timeout = 120,
                parse = TRUE, token = NULL,
                output_file_name = search, 
                verbose = TRUE, dir = NULL)
}
\arguments{
  \item{search}{Query to be searched, used to filter and select tweets to return from Twitter's REST API. See details.}
  \item{timeout}{Numeric scalar, specifying the amount of time in seconds to leave the connection open while streaming tweets (Default = 120). To stream indefinetely, use timeout = FALSE.}
  \item{parse}{Logical, indicating whether to return parsed data.frame if true, or nested list if false. By default, parse = TRUE in order to clean the data extracted from Twitter.}
  \item{token}{File containing our Twitter tokens (Check \code{\link[rtweet]{search_tweets}} for more info).}
  \item{output_file_name}{Name of the output writting file (Default = "stream").}
  \item{verbose}{Check whether or not to include output processing/retrieval messages (Default = TRUE).}
  \item{dir}{Name of directory in which json files should be written. The default = NULL will create a timestamped "stream" folder in the current working directory.}}
\details{
This function allows you to stream public tweets from the Twitter platform in real-time, so you can see and download those that fit your search terms.

There are four possible methods to select the query ("search" argument) used to select and customize the collection method: 

(1) Random sample: the default, search = "", returns a small random sample of all publicly available Twitter statuses.

(2) By keyword: use a comma separated character string with the desired phrase(s) and keyword(s) to filter by keyword (up to 400 keywords). 

(3) By user: track users by providing a comma separated list of users (up to 5000 user_ids).

(4) By geo coordinates: use four latitude/longitude bounding box points to stream by geo location. This must be provided via a vector of length 4, i.e., c(-125, 26, -65, 49).  (1-360 degree location boxes).

It is recomended to stream with hardwired reconnection method to ensure timeout integrity.
}
\value{
Tweets data returned as data.frame with users data as attribute.
}
\author{
Jose L.A. Berrocal,  University of Salamanca. See \url{http://berrocal.usal.es/}.
}
\examples{
## Not run:
## Export twitter credentials:

# token<- credentials("credentials.txt")

## Example extracting a random sample of tweets for 30 seconds:

# stream_tweet(search = "", timeout = 30,
#                output_file_name = "sample")
                
## Export data using load_tweets() function:             
# sample <- load_tweets("sample", "streaming")                
                

## Example streaming tweets using the hashtag #datascience as keyword for 90 seconds:
# stream_tweet("datascience", timeout = 90,
#               output_file_name = "datascience")
                
## Export the data frame and plot tweet frequency (if needed): 
# datascience <- load_tweets(name = "datascience", 
#                      type = "streaming")
#ts_plot(datascience, "secs")

## Example streaming tweets by user (@BBCWorld) for 10 minutes: 
## (Warning! It's possible that they don't tweet anything during that time!)

# userid<-lookup_users("BBCWorld", parse = TRUE, token = token)$user_id
# stream_tweet(userid, timeout = 600,
#               output_file_name = "BBCWorld")

## Export data:
# BBCWorld <- load_tweets("BBCWorld", "streaming")

## Example streaming by geo coordinates for 60 seconds:
# stream_tweet(search = c(-125, 26, -65, 49), timeout = 60, output_file_name = "geotweets")

## Export data:

# geotweets<- load_tweets("geotweets", "streaming")
## End (Not run)
}
