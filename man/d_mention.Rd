\name{d_mention}
\alias{d_mention}
\title{Dynamic netCoin graph of original authors and mentions}
\description{
This function produces a dynamic and interactive netCoin network from a data frame or matrix of tweets (original messages) conecting authors and mentions (@) that allows to recreate the timeline of messages and interactions on Twitter.
}
\usage{
d_mention(Tuits, author = "author", text = "text", 
          date = "date", imagedir= NULL, ext="png",
          fields = c("followers", "following", "statuses", "location"), 
          beginDate = NULL, endDate= NULL, 
          interval= 3600, tzone = "Europe/Paris", 
          limits=NULL, nFrames = Inf, seed=1, 
          n= min(5000,nrow(Tuits)/2), minlabel = 5, ...)
}
\arguments{
  \item{Tuits}{	A data frame with at least one vector of names.}
  \item{author}{Variable containing the profiles (users' names).}
  \item{text}{Variable from data containing the messages (tweets).}
  \item{date}{Variable from data containing the date when the message was written.}
  \item{imagedir}{Directory name where images (if there are) are located.}
  \item{ext}{Image files extension (.png by default).}
  \item{fields}{Vector containing "followers" (number of people who follows a user), "following" (people followed by user), "statuses" (number of tweets written) and "location" (from where is the user).}
  \item{beginDate}{Date of first available message.}
  \item{endDate}{Date of last available message.}
  \item{interval}{The lenght of the time intervals (default = 3600 seconds).}
  \item{tzone}{Time zone (i.e. "Europe/Paris").}
  \item{limits}{Date vector with the cutpoints for dynamic graphs.}
  \item{nFrames}{The number of frames to render.}
  \item{seed}{A single value, interpreted as an integer, or NULL.}
  \item{n}{Sample the network if the number of nodes is greater than 5000 or n.}
  \item{minlabel}{Avoid label of nodes with a frequency less than #minlabel.}
  \item{...}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
This function works using netCoin package "dichotomize" function. Is intented to be used with original messages (thus excluding retweets from the data frame, "original=TRUE"). The argument "others" is part of netCoin's dichotomize function: check \code{\link[netCoin]{dichotomize}}'s how to use if needed.

Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument can be used in order to specify the network produced. 
}
\value{
This function creates a \code{\link[netCoin]{netCoin}} object and, if stated, a folder in the computer with an HTML document named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly. See \code{\link{netCoin}}.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{


## This example uses a sample of original tweets using 
## the hashtag #cuentalo:

# data(cuentalo)
              
# d_mention(Tuits = cuentalo, author = "author", text = "text", date = "date")              


## You can include all type of arguments from netCoin 
## and/or allNet and visualize the correspondent graph:

# d_mention(Tuits = cuentalo, author = "author", text = "text", date = "date",
#          zoom = 4.66, distance = 13, repulsion = 7.12)
}