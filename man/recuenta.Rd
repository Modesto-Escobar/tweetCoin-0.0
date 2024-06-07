\name{recuenta}
\alias{recuenta}
\docType{data}
\title{
A collection of retweets, "#cuentalo"
}
\description{
Data frame with a colection of retweets from hashtag "cuentalo" (date, author and text).
}
\usage{data("recuenta")}
\format{
  A data frame with 1000 observations and 3 variables:
  \describe{
    \item{\code{date}}{retweet date}
    \item{\code{author}}{retweet author}
    \item{\code{text}}{retweet content}
    }
}
\references{
#cuentalo (2018), \url{https://twitter.com/}.
}
\examples{
data(recuenta)
head(recuenta,10)
}
\keyword{datasets}
