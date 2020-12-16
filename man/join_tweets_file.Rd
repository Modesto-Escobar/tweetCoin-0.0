\name{join_tweets_file}
\alias{join_tweets_file}
\title{Join two ore more datasets through a .txt file.}
\description{
This function joins two or more datasets containing tweets obtained through the different download functions from tweetCoin using a .txt file.
}
\usage{
join_tweets_file(file = "users.txt",
                type = c("user", "search", "stream"),
                format = c("binary","delimited"))
}
\arguments{
  \item{file}{A .txt file containing the names of the files to be joined.}
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

## In this example we are going to join two datasets
## containing user's data: 
credentials()
user_tweet("rstudio", maxtweets = 20, format = "binary")
user_tweet("rstudiotips", maxtweets = 20, format = "binary")

## We need and .txt file with the names of the files
## with the data:
users<-c("rstudio", "rstudiotips")
write.table(users, file = "users.txt")

## Once we have the .txt file, we only need to specify
## the type and format of the data:
A<-join_tweets_file(file = "users.txt", type ="user", format = "binary")

## End(Not run)
}