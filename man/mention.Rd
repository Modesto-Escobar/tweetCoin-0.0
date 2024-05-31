\name{mention}
\alias{mention}
\title{Static netCoin graph of original authors and mentions}
\description{
This function produces an interactive netCoin network from a data frame or matrix of tweets conecting authors and mentions (@).
}
\usage{
mention(data, author="author", text="text", allAuthors=FALSE, ...)
}
\arguments{
  \item{data}{A matrix or data.frame of tweets and users.}
  \item{author}{Vector name from data containing the profiles (users' names).}
  \item{text}{Vector name from data containing the messages (tweets).}
  \item{allAuthors}{Should keep all authors? Default: FALSE.}
  \item{...}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
This function works using netCoin package "dichotomize" function. Is intented to be used with original messages (thus excluding retweets from the data frame, "original=TRUE"). The argument "others" is part of netCoin's dichotomize function: check \code{\link[netCoin]{dichotomize}}'s how to use if needed.

Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument can be used in order to specify the network produced. See \code{\link{netCoin}}.
}
\value{
This function creates a netCoin object, which can be ploted with the plot() function, creating a folder in the computer with an HTML file named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## This example uses a sample of tweets using 
## the hashtag #Shakespeare:

data(Shakespeare)
m <- mention(Shakespeare, "author", "text")

## You can include all type of arguments from netCoin 
## and/or allNet And visualize the correspondent graph:

library(netCoin)
m2<- addNetCoin(m, size = "degree", distance = 12.8, 
            repulsion = 9.08, zoom = .42, main= "Mention graph of #Shakespeare")
}
