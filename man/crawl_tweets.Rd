\name{crawl_tweets}
\alias{crawl_tweets}
\title{Get tweets from a set of Twitter users}
\description{
This function downloads the tweets from a given set of Twitter profiles.
}
\usage{
crawl_tweets(file = "users.txt", maxtweets = 100, home = FALSE, parse = TRUE, check = TRUE, token = NULL, 
             include_rts = FALSE, output_file_name = NULL, format = c("binary", "delimited"))
}
\arguments{
  \item{file}{A .txt file containing the names of the Twitter users.}
  \item{maxtweets}{Integer specifying the total number of desired tweets to return (Default = 100). See details for more information.}
  \item{home}{Indicate whether to return a user-timeline or home-timeline. By default, home is = FALSE (returns only tweets posted by the given user). Set home = FALSE to return a user's home timeline feed (returns tweets posted by accounts followed by a user).}
  \item{parse}{Indicates whether to return parsed data.frame if true, or nested list if false. By default, parse = TRUE in order to clean the data extracted from Twitter.}
  \item{check}{Logical indicating whether to remove check available rate limit. Ensures the request does not exceed the maximum remaining number of calls. Defaults = TRUE.}
  \item{token}{File containing our Twitter tokens (Check \code{\link[rtweet]{search_tweets}} for more info).}
  \item{include_rts}{Include rtweets made by the user (default = FALSE).}
  \item{output_file_name}{Name of the output writting file.}
  \item{format}{It could be "binary"" or "delimited" (plain text).}
}
\details{
This function extracts the tweets published by a set of Twitter users. All the tweets are saved into a .csv or .dat file. If there is a file with the same name, it reads it, the extracted tweets are concadenated sequentially and overwrites it. If the archive file name is not specified, a default name is created.

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
## The function followers_user returns a list of user IDs for the accounts following specified user. 
## Set your directory and run this code to retrieve the user IDs of those who follow your desired user.
A <- followers_user("jack", page = "-1", 
                    retryonratelimit = TRUE, 
                    parse = TRUE, verbose = TRUE, 
                    token = NULL)

userid <- A$user_id
write.table(userid, file = "users.txt")

## Once you have the user IDs in a .txt file, you can run this function to retrieve tweets from the given profiles.
B <- crawl_tweets(file = "users.txt", maxtweets = 100, home = FALSE, parse = TRUE, check = TRUE, token = NULL, 
             include_rts = FALSE, output_file_name = NULL, format = "binary")

## End (Not run)
}