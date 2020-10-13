\name{prepareqda}
\alias{prepareqda}
\title{Clean and prepare your Twitter data from QDA.}
\description{
This function performs the standard needed manipulation to data from Twitter extracted using QDA Miner in order to perform an analysis using tweetCoin.}

\usage{
prepareqda(data = data)
}

\arguments{
  \item{data}{A data frame of Twitter data from QDA Miner.}
}


\value{
A cleaner data.frame with only the selected variables with a format legible by \code{\link[tweetCoin]{tweetCoin-package}} functions.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## Load the prepared QDA dataset:

# data(test)
# prepareqda(test)

}
