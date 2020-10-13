\name{user_tweet}
\alias{user_tweet}
\title{
Tweets Extraction
}
\description{
Extract and download the tweets from a Twitter user or users.
}
\usage{
user_tweet (user, maxtweets = 100, 
                  home = FALSE, parse = TRUE, 
                  check = TRUE, token = NULL, 
                  include_rts = FALSE, 
                  output_file_name = NULL,
                  format = "binary")
}
\arguments{
  \item{user}{Twitter user(s) identificator (target).}
  \item{maxtweets}{Integer specifying the total number of desired tweets to return (Default = 100). See details for more information.}
  \item{home}{Indicate whether to return a user-timeline or home-timeline. By default, home is = FALSE (returns only tweets posted by the given user). Set home = FALSE to return a user's home timeline feed (returns tweets posted by accounts followed by a user).}
  \item{parse}{Indicates whether to return parsed data.frame if true, or nested list if false. By default, parse = TRUE in order to clean the data extracted from Twitter.}
  \item{check}{Logical indicating whether to remove check available rate limit. Ensures the request does not exceed the maximum remaining number of calls. Defaults = TRUE.}
  \item{token}{File containing our Twitter tokens (Check \code{\link[rtweet]{search_tweets}} for more info).}
  \item{include_rts}{Include rtweets made by the user (default = FALSE).}
  \item{output_file_name}{Name of the output writting file.}
  \item{format}{Manage the extensions of the file. If format = "binary", the extension will be ".dat", if format = "delimited", the extension will be ".csv". Default = "binary".}
}
\details{
This function extracts the tweets published by a Twitter user. All the tweets are saved into a .csv file. If there is a file with the same name, it reads it, the extracted tweets are concadenated sequentially and overwrites it. If the archive file name is not specified, a default name is created.

The number of tweets to return per timeline should not exceed 3200 as Twitter limits returns to the most recent 3,200 statuses posted or retweeted by each user. 
}
\value{
A .csv or .dat file with users data attributes.
}
\author{
Jose L.A. Berrocal,  University of Salamanca. See \url{http://berrocal.usal.es/}
}
\examples{
## Not run:

## Example extracting the tweets from 
## twitter account @github:

# credentials("credentials.txt")
# user_tweet("github")

## And now the .csv file can be upload 
## to our environment:

# github <- load_tweets("github", type = "timeline")
## End (Not run)
}
