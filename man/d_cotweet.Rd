\name{d_cotweet}
\alias{d_cotweet}
\title{Dynamic netCoin graph of original tweets}
\description{
This function produces a dynamic and interactive netCoin network from a data frame or matrix of tweets (original messages) coincidendes, conecting mentions and hashtags.
}
\usage{
d_cotweet(Tuits, author="author", text="text", 
          date="date", 
          fields=c("followers", "following", "stauses", "location"),
          searchers="@#", beginDate = NULL,
          endDate= NULL, interval= 3600, 
          tzone = "Europe/Paris", limits=NULL, nFrames = Inf,
          seed=1, n = min(5000,nrow(Tuits)/2), 
          minlabel=5, ...)
}
\arguments{
  \item{Tuits}{	A data frame with at least one vector of names.}
  \item{author}{Variable containing the profiles (users' names).}
  \item{text}{Variable from data containing the messages (tweets).}
  \item{date}{Variable from data containing the date when the message was written.}
  \item{fields}{Vector containing "followers" (number of people who follows a user), "following" (people followed by user), "stauses" (number of tweets written) and "location" (from where is the user).}
  \item{searchers}{Selected symbols to use as match key}
  \item{beginDate}{Date of first available message.}
  \item{endDate}{Date of last available message.}
  \item{interval}{The end-points of the interval (default = 3600).}
  \item{tzone}{Time zone (i.e. "Europe/Paris").}
  \item{limits}{Date vector with the cutpoints for dynamic graphs.}  
  \item{nFrames}{The number of frames to render (default = Inf).}
  \item{seed}{A single value, interpreted as an integer, or NULL.}
  \item{n}{Sample the network if the number of nodes is greater than 5000 or n.}
  \item{minlabel}{Avoid label of nodes with a frequency less than #minlabel (default = 5)..}
  \item{...}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
This function works using netCoin package "dichotomize" function. Is intented to be used with original messages (thus excluding retweets from the data frame, "original=TRUE"). The argument "others" is part of netCoin's dichotomize function: check \code{\link[netCoin]{dichotomize}}'s how to use if needed.

Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument can be used in order to specify the network produced.See \code{\link{netCoin}}.
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

data(cuentalo)
d_cotweet(Tuits=cuentalo,"author", "text", date= "date")

## You can include all type of arguments from netCoin 
## and/or allNet:

# plot(d_cotweet(Tuits = cuentalo, author= "author", 
#                text = "text", date = "date",
#                nodes = nodes, size = "degree",
#                zoom = 2, repulsion = 32, 
#                distance= 18), dir = "~/temp")
}