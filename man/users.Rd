\name{users}
\alias{users}
\docType{data}
\title{
A collection of twitter users, "#cuentalo"
}
\description{
Data frame with a colection of twitter users from hashtag "cuentalo".
}
\usage{data("users")}
\format{
  A data frame with 51 observations and 7 variables:
  \describe{
    \item{\code{name}}{user name}
    \item{\code{followers}}{user followers}
    \item{\code{following}}{user followings}
    \item{\code{statuses}}{user status}
    \item{\code{Tweets}}{user number of tweets}
    \item{\code{Retweets}}{user number of retweets}
    \item{\code{Retweeted}}{user number of retweeted}
    }
}
\references{
#cuentalo (2018), \url{https://twitter.com/}.
}
\examples{
data(users)
head(users,10)
}
\keyword{datasets}
