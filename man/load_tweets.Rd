\name{load_tweets}
\alias{load_tweets}
\title{Import CSV Twitter data}
\description{
Reads Twitter data that was previously saved as a .CSV file and converts it into a data.frame.
}
\usage{
load_tweets(name, type = c("user","search","stream"), format = c("binary","delimited"))
}
\arguments{
  \item{name}{Name of CSV file containing the data.}
  \item{type}{Type of data to be readed. Default = "user" (for a user profile), other options are "stream" (for streaming tweets) and search (for queries and/or hashtags). See details.}
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
\dontrun{

credentials()

## When using "user_tweet" function:
user_tweet("github", maxtweets=100, format="binary")
github <- load_tweets("github", type = "user", format="binary")

## When using "search_tweets":
search_tweet("data science", maxtweets = 300, 
              type = "recent", format= "delimited")
datascience <- load_tweets(name= "data science", 
             type = "search", format= "delimited")

## When using "stream_tweet":
stream_tweet(search = "", timeout = 30,
               output_file_name = "sample", format= "delimited")
sample <- load_tweets("sample", "stream", "delimited")                                       
}                    
}
