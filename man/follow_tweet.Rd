\name{follow_tweet}
\alias{follow_tweet}
\title{Get user IDs for Twitter accounts following an user}
\description{
Returns a list of user IDs for the accounts following specified user. 
}
\usage{
follow_tweet(user, page = "-1", 
                    retryonratelimit = TRUE, 
                    parse = TRUE, verbose = TRUE, 
                    token = NULL)
}
\arguments{
  \item{user}{Twitter user(s) identificator (target).}
  \item{page}{Default page = -1 specifies first page of JSON results. If parse = TRUE then the cursor value can be extracted from the return object by using the next_cursor function.}
  \item{retryonratelimit}{Indicates whether to wait and retry when rate limited. This argument is only relevant if the designed user has more than 75,000 followers and you'd like to retrieve all in a single call (Default = FALSE).}
  \item{parse}{Indicates whether to return parsed vector if true, or nested list if false. By default, parse = TRUE in order to clean the data extracted from Twitter.}
  \item{verbose}{Check whether or not to include output processing/retrieval messages (Default = TRUE). Only relevant when retryonratelimit = TRUE.}
  \item{token}{File containing our Twitter tokens (Check \code{\link[rtweet]{search_tweets}} for more info).}
}
\details{
Do not use the at symbol (@) when refering to a twitter user, just the identificator name.
To return more than 75,000 user IDs in a single call (the rate limit maximum), set "retryonratelimit" to TRUE.
}

\value{
A .csv file containg a Twitter user's follower IDs (a column named "user_id").
}

\author{
Jose L.A. Berrocal,  University of Salamanca. See \url{http://berrocal.usal.es/}
}
\examples{
## 
#a <- follow_tweet("jack")
}