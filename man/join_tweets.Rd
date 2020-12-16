\name{join_tweets}
\alias{join_tweets}
\title{Join two or more datasets of Twitter data}
\description{
This function joins two or more datasets containing tweets obtained through the different download functions from tweetCoin.
}
\usage{
join_tweets(names, type = c("user", "search", "stream"), 
            format = c("binary","delimited")
}
\arguments{
   \item{names}{Names of the files containing the data.}
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
## Not run: 

## This function is useful when you have several 
## datasets with the same query, as for example:
credentials()
search_tweet(search = "stats", maxtweets = 50, format = "binary")
search_tweet(search = "rstats", maxtweets = 50, format = "binary")

##Use join_tweets function to parse them in the same dataset, as here:
names <- c("stats", "rstats")
A<- join_tweets(names = names, type = "search", format = "binary")

## End(Not run)
}