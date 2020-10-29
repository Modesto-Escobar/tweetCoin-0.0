\name{d_mention}
\alias{d_mention}
\title{Dynamic netCoin graph of original authors and mentions through time}
\description{
This function produces a dynamic and interactive netCoin network from a data frame or matrix of tweets (original messages) conecting authors and mentions (@) that allows to recreate the timeline of messages and interactions on Twitter.
}
\usage{
d_mention(Tweets, author = "author", text = "text", 
          date = "date", imagedir= NULL, ext="png",
          fields = c("followers", "following", "statuses", "location"), 
          beginDate = NULL, endDate= NULL, 
          interval= 3600, tzone = "Europe/Paris", 
          limits=NULL, nFrames = Inf, seed=1, 
          n= min(5000,nrow(Tweets)), minlabel = 5, ...)
}
\arguments{
  \item{Tweets}{	A data frame with at least one vector of names.}
  \item{author}{Vector name from data containing the profiles (users' names).}
  \item{text}{Vector name from data containing the messages (tweets).}
  \item{date}{Vector name from data containing the date when the messages were written.}
  \item{imagedir}{Directory name where images (if any) are located.}
  \item{ext}{Image files extension (.png by default).}
  \item{fields}{Vector containing "followers" (number of people who follows a user), "following" (people followed by user), "statuses" (number of tweets written) and "location" (from where is the user).}
  \item{beginDate}{Date of first available message.}
  \item{endDate}{Date of last available message.}
  \item{interval}{The lenght of the time intervals (default = 3600 seconds).}
  \item{tzone}{Time zone (e.g., "Europe/Paris").}
  \item{limits}{Date vector with the cutpoints for dynamic graphs.}
  \item{nFrames}{The number of frames to render.}
  \item{seed}{A single value, interpreted as an integer, or NULL.}
  \item{n}{Sample the network if the number of nodes is greater than 5000 or n.}
  \item{minlabel}{Avoid label of nodes with a frequency less than #minlabel.}
  \item{...}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
This function works using netCoin package "dichotomize" function. Is intented to be used with original messages (thus excluding retweets from the data frame, "original=TRUE"). The argument "others" is part of netCoin's dichotomize function: check \code{\link[netCoin]{dichotomize}}'s documentation if needed.

Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument can be used in order to specify the network produced. 
}
\value{
This function creates a list containing three objects: "authors" (a data.frame containing names, links, Tweets and Mentions), "messages" (a data.frame containing dates, authors and texts) and "nets", (a set of \code{\link[netCoin]{netCoin}} objects). 

This set of netCoin objects can be ploted with netCoin's \code{\link[netCoin]{multigraphCreate}} function, creating a folder in the computer with an HTML document named index.html which contains the produced graph(s). This file can be directly opened with your browser and sent to a web server to work properly.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## Not run: 

## This example uses a sample of original tweets using 
## the hashtag #Shakespeare:

# data(Shakespeare)
# e <- d_mention(Shakespeare, author = "author", text = "text", date = "date")              
# do.call(multigraphCreate, e$nets)

## You can include all type of arguments from netCoin 
## and/or allNet and visualize the correspondent graph:

# plot(d_mention(Tweets = Shakespeare, min=5,
#    author = "author", text = "text", 
#    date = "date", zoom = .8, distance = 13, 
#    repulsion = 7)$nets)

## End(Not run)
}