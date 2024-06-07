\name{cuentalo}
\alias{cuentalo}
\docType{data}
\title{
A collection of tweets, "#cuentalo"
}
\description{
Data frame with a colection of tweets from hashtag "cuentalo".
}
\usage{data("cuentalo")}
\format{
  A data frame with 1000 observations and 8 variables:
  \describe{
    \item{\code{date}}{tweet date}
    \item{\code{author}}{tweet author}
    \item{\code{text}}{tweet content}
    \item{\code{followers}}{author's followers}
    \item{\code{following}}{author's followers}
    \item{\code{statuses}}{author's status}
    \item{\code{location}}{tweet location}
    \item{\code{status_id}}{status id}
    }
}
\references{
#cuentalo (2018), \url{https://twitter.com/}.
}
\examples{
data(cuentalo)
head(cuentalo,10)
}
\keyword{datasets}
