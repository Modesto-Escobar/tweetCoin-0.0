\name{join_tweets}
\alias{join_tweets}
\title{Join datasets}
\description{
This function joins two datasets containing tweets obtained through the different download functions from tweetCoin.
}
\usage{
join_tweets(names, type = c("user", "search", "stream"), format = c("binary","delimited")
}
\arguments{
   \item{names}{Name of CSV file containing the data.}
  \item{type}{Type of data to be readed. It can be "user" (for a user profile), "stream" (for streaming tweets) or search (for queries and/or hashtags). See details.}
  \item{format}{Format of the file, "binary" for binary format (.dat), "delimited" for delimited text file (.csv).}
}
\details{
Use type = "user" when using \code{\link[tweetCoin]{user_tweet}} function (tweets and information from concrete users), type = "search" when using \code{\link[tweetCoin]{search_tweet}} function (tweets containing a specific word or set of words) and use "stream" when using \code{\link[tweetCoin]{stream_tweet}} function (for a live stream recollection of Twitter data). See examples.
}

\value{
A data frame of Twitter data.
}
\author{
Jose L.A. Berrocal,  University of Salamanca. See \url{http://berrocal.usal.es/}.
}
\examples{
## This function is useful when ypu have several datasets 
## with the same query, as for example:
search_tweet(search= "stats", maxtweets = 50, format = "binary")

##Use join_tweets function to parse them in the same dataset, as:
join_tweets("stats", type="search", format="binary")
}