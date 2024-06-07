\name{Shakespeare}
\alias{Shakespeare}
\docType{data}
\title{
A collection of tweets, "#Shakespeare"
}
\description{
Data frame with a colection of tweets from hashtag "Shakespeare".
}
\usage{data("Shakespeare")}
\format{
  A data frame with 962 observations and 8 variables:
  \describe{
    \item{\code{author}}{tweet author}
    \item{\code{text}}{tweet content}
    \item{\code{date}}{tweet date}
    \item{\code{followers}}{author's followers}
    \item{\code{following}}{author's followers}
    \item{\code{statuses}}{author's status}
    \item{\code{location}}{tweet location}
    \item{\code{status_id}}{status id}
    }
}
\references{
#Shakespeare (2020), \url{https://twitter.com/}.
}
\examples{
data(Shakespeare)
head(Shakespeare,10)
}
\keyword{datasets}
