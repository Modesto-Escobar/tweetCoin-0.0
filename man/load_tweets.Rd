\name{load_tweets}
\alias{load_tweets}
\title{Import CSV Twitter data}
\description{
Reads Twitter data that was previously saved as a .CSV file and converts it into a data.frame.
}
\usage{
load_tweets(name, type = "user", format = "binary")
}
\arguments{
  \item{name}{Name of CSV file containing the data.}
  \item{type}{Type of data to be readed. Default = "timeline" (for a user profile), other options are "streaming" (for streaming tweets) and search (for queries and/or hashtags). See details.}
  \item{format}{Manage the extensions of the file. If format = "binary", the extension will be ".dat", if format = "delimited", the extension will be ".csv". Default = "binary".}
}
\details{
Use type = "timeline" when using \code{\link[tweetCoin]{user_tweet}} function (tweets and information from concrete users), type = "streaming" when using \code{\link[tweetCoin]{search_tweet}} function (tweets containing a specific word or set of words) and use "search" when using \code{\link[tweetCoin]{stream_tweet}} function (for a live stream recollection of Twitter data). See examples.
}
\value{
A data frame of Twitter data.
}
\author{
Jose L.A. Berrocal,  University of Salamanca. See \url{http://berrocal.usal.es/}.
}
\examples{

## Not run:

# credentials()

## When using "extraction_tweets" function:

# user_tweet("github")
# github <- load_tweets("github", type = "timeline")

## When using "buscar_tweets":

# search_tweet("data science", maxtweets = 300, 
#              type = "recent")
# datascience <- load_tweets(name= "data science", 
#             type = "search")

## When using "streaming_tweets":

# stream_tweet("#DEVCommunity", timeout = 120,
#               output_file_name = "stream")
# DEVCommunity <- load_tweets(name = "stream", 
#                 type = "stream")
                        
## End(Not run)                      
}