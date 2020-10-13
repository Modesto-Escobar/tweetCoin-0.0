\name{mention}
\alias{mention}
\title{Static netCoin graph of original authors and mentions}
\description{
This function produces a static network object from a data frame or matrix of tweets (original messages) conecting authors and mentions (@).
}
\usage{
mention(data, author = "author", text = "text", ...)
}
\arguments{
  \item{data}{A matrix or data.frame of tweets and users.}
  \item{author}{Variable from data containing the author (user).}
  \item{text}{Variable from data containing the messages (tweets).}
  \item{...}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
This function works using netCoin package "dichotomize" function. Is intented to be used with original messages (thus excluding retweets from the data frame, "original=TRUE"). The argument "others" is part of netCoin's dichotomize function: check \code{\link[netCoin]{dichotomize}}'s how to use if needed.

Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument can be used in order to specify the network produced. 
}
\value{
This function creates a \code{\link[netCoin]{netCoin}} object and, if stated, a folder in the computer with an HTML document named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## This example uses a sample of original tweets using 
## the hashtag #cuentalo:

data(cuenta)
mention(cuenta, "author", "text")

## You can include all type of arguments from netCoin 
## and/or allNet And visualize the correspondent graph:

G <- mention(data = cuenta, author = "author", 
                  text = "text")

# plot(netCoin(G, size = "degree", distance = 16.4, 
#              repulsion = 13.2, zoom = 1), 
#              dir = "~/temp")
#  
}