\name{retweet}
\alias{retweet}
\title{Static netCoin graph of retweets}
\description{
This function produces a static network object from a data frame or matrix of retweets (RTs or shared messages) conecting profiles and retweets.
}
\usage{
retweet(data, sender = "author", 
            text = "text", 
            language = "en", nodes=NULL, ...)
}
\arguments{
  \item{data}{A data frame with at least one vector of names.}
  \item{sender}{Variable containing the profiles (users' names) }
  \item{text}{Name of the variable with the text or message.}
  \item{language}{Select language of the variables' name (es=spanish; en=english; ca=catalan).}
  \item{nodes}{A data frame with at least one vector of names.}
  \item{...}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument can be used in order to specify the network produced (See examples).
}
\value{
This function creates a \code{\link[netCoin]{netCoin}} object and, if stated, a folder in the computer with an HTML document named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## This example uses a sample of retweets using 
## the hashtag #cuentalo:

data(recuenta)
retweet( recuenta, "author", "text")
# plot(retweet( recuenta, "author", "text"),
#                   dir = "~/temp")

## You can add all kinds of netCoin and/or 
## allNet arguments:

# plot(retweet( recuenta, "author", "text", 
#                   zoom = .5, repulsion = 8.5, 
#                   size = "degree", showArrows = TRUE), 
#                   dir = "~/temp")
}