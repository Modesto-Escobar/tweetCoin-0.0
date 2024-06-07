\name{cuenta}
\alias{cuenta}
\docType{data}
\title{
A collection of tweets, "#cuentalo"
}
\description{
Data frame with a colection of tweets from hashtag "cuentalo" (date, author and text).
}
\usage{data("cuenta")}
\format{
  A data frame with 625 observations and 3 variables:
  \describe{
    \item{\code{date}}{tweet date}
    \item{\code{author}}{tweet author}
    \item{\code{text}}{tweet content}
    }
}
\references{
#cuentalo (2018), \url{https://twitter.com/}.
}
\examples{
data(cuenta)
head(cuenta,10)
}
\keyword{datasets}
