\name{prepare}
\alias{prepare}
\title{
Clean and prepare your Twitter data.
}
\description{
This function performs the standard manipulation need to perform an analysis of data extracted from Twitter using TweetCoin.}

\usage{
prepare(data = data, 
        info = c("fields", "cotweets", "inform"),
        original = TRUE, quote = FALSE, 
        retweet = FALSE)
}
\arguments{
  \item{data}{A data frame with at least one vector of names.}
  \item{info}{Variables to be extracted. See details.}
  \item{original}{Logical indicating whether to return original messages or not. By default = TRUE.}
  \item{quote}{Logical indicating whether to return quoted messages or not. By default = FALSE.}
  \item{retweet}{Logical indicating whether to return retweeted messages or not. By default = FALSE.}  
}
\details{

This function performs the standard manipulation need to perform an analysis of data extracted from Twitter using TweetCoin. It converts the data obtained by the functions \code{\link[tweetCoin]{user_tweet}}, \code{\link[tweetCoin]{search_tweet}} or \code{\link[tweetCoin]{stream_tweet}} into a format legible by the rest of \code{\link[tweetCoin]{tweetCoin-package}} functions, specially those that allows to visualice the networks as \code{\link[tweetCoin]{mention}}, \code{\link[tweetCoin]{cotweet}} or \code{\link[tweetCoin]{retweet}}.

The argument \code{info} has different options:

   if \code{info} = "fields", the variables to be inclued will be "screen_name","text","created_at","is_quote" and "is_retweet".
  
   if \code{info} = "cotweets", the variables to be inclueded will be "quoted_screen_name" and "retweet_screen_name".
   
   if \code{info} = "inform", the variables to be inclueded will be "followers_count", "friends_count" and "statuses_count".
   
There is the possibility of selecting all three options if needed.
}


\value{
A cleaner data.frame with only the selected variables with a format legible by \code{\link[tweetCoin]{tweetCoin-package}} functions.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## Extracting tweets using the hastag #sociology as an example:
# search_tweet("#sociology", maxtweets= 100)

## Loading into our directory:
# sociology<- load_tweets("#sociology", type = "search")

## Now we can prepare this dataset for future work, maybe just selecting the original tweets:

# clean_sociology <- prepare( data = sociology, 
#       info = c("fields", "cotweets", "inform"),
#       original = TRUE, quote = FALSE, 
#       retweet = FALSE)
}
