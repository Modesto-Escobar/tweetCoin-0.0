\name{rShakespeare}
\alias{rShakespeare}
\docType{data}
\title{
A collection of retweets, "#Shakespeare"
}
\description{
Data frame with a colection of retweets from hashtag "Shakespeare".
}
\usage{data("rShakespeare")}
\format{
  A data frame with 531 observations and 7 variables:
  \describe{
    \item{\code{author}}{tweet author}
    \item{\code{text}}{tweet content}
    \item{\code{date}}{tweet date}
    \item{\code{followers}}{author's followers}
    \item{\code{following}}{author's followers}
    \item{\code{statuses}}{author's status}
    \item{\code{location}}{tweet location}
    }
}
\references{
#Shakespeare (2020), \url{https://twitter.com/}.
}
\examples{
data(rShakespeare)
head(rShakespeare,10)
}
\keyword{datasets}
