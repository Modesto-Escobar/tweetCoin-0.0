\name{d_retweet}
\alias{d_retweet}
\title{Dynamic netCoin graph of retweets.}
\description{
This function produces a dynamic and interactive netCoin network that allows to recreate the timeline of messages and interactions on Twitter.
}
\usage{
d_retweet(Tuits, author="author", text="text", 
        date="date", fields=c("followers", 
        "following", "stauses", "location"),
        beginDate = NULL, endDate= NULL, 
        interval= 3600, tzone = "Europe/Paris",
        nFrames = Inf, seed=1, n = min(5000, 
        nrow(Tuits)/2), minlabel=5, ...)
}
\arguments{
  \item{Tuits}{ A data frame with at least one vector of names.}
  \item{author}{Variable containing the profiles (users' names).}
  \item{text}{Name of the variable with the text or message.}
  \item{date}{Variable containing the date when the message was send.}
  \item{fields}{Vector containing "followers" (number of people who follows a user), "following" (people followed by user), "stauses" (number of tweets written) and "location" (from where is the user).}
  \item{beginDate}{Date of first available message.}
  \item{endDate}{Date of last available message.}
  \item{interval}{The end-points of the interval.}
  \item{tzone}{Time zone (i.e. "Europe/Paris").}
  \item{nFrames}{The number of frames to render (default = Inf).}
  \item{seed}{A single value, interpreted as an integer, or NULL.}
  \item{n}{Sample the network if the number of nodes is greater than 5000 or n.}
  \item{minlabel}{Avoid label of nodes with a frequency less than # minlabel.}
  \item{...}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
This function produces a dynamic netCoin network that allows to recreate the timeline of messages and interactions on Twitter.

Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument can be used in order to specify the network produced. 
}
\value{
This function creates a \code{netCoin} object and, if stated, a folder in the computer with an HTML document named index.html which contains the produced graph. This file can be directly opened with your browser and sent to a web server to work properly. See \code{\link{netCoin}}.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## Example using a sample of tweets and retweets with 
## the hashtag #cuentalo:
data(cuentalo)
#d_retweet(Tuits = cuentalo)

## You can use all kind of netCoin arguments:
# d_retweet( Tuits = cuentalo, zoom = 2.5, repulsion= 15)
}