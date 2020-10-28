\name{retweet}
\alias{retweet}
\title{Static netCoin graph of retweets}
\description{
This function produces an interactive netCoin network from a data frame or matrix of retweets (RTs or shared messages) conecting profiles and retweets.
}
\usage{
retweet(data, sender = "author", 
            text = "text", 
            language = "en", nodes=NULL, ...)
}
\arguments{
  \item{data}{A data frame with at least one vector of names.}
  \item{sender}{Vector name from data containing the profiles (users' names).}
  \item{text}{Vector name from data containing the messages (tweets).}
  \item{language}{Select language of the variables' name (es=spanish; en=english; ca=catalan).}
  \item{nodes}{A data frame with at least one vector of names.}
  \item{...}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument can be used in order to specify the network produced. See \code{\link{netCoin}}.
}
\value{
This function creates a netCoin object, which can be ploted with the plot() function, creating a folder in the computer with an HTML file named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## Not run: 

## This example uses a sample of tweets using 
## the hashtag #Shakespeare:

# data(Shakespeare)
# retweet( Shakespeare, "author", "text")
# plot(retweet( Shakespeare, "author", "text"),
                   dir = "~/temp")

## You can add all kinds of netCoin and/or 
## allNet arguments:

# R <- retweet (Shakespeare, "author", "text")
# plot(netCoin(R, size = "degree", distance = 15, 
#              repulsion = 25, zoom = .2, 
#              main= "Graph of retweets using #Shakespeare:"), 
#              dir = "~/temp")

## End(Not run)
}