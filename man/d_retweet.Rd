\name{d_retweet}
\alias{d_retweet}
\title{Dynamic netCoin graph of retweets through time}
\description{
This function produces a dynamic and interactive netCoin network that allows to recreate the timeline of messages and interactions on Twitter.
}
\usage{
d_retweet(Tweets, author = "author", text = "text", 
        date = "date", fields = c("followers", 
        "following", "statuses", "location"),
        beginDate = NULL, endDate = NULL, 
        interval= 3600, tzone = "Europe/Paris",
        tCutpoints=NULL, nFrames = Inf, seed=1, n = min(5000, 
        nrow(Tweets)), minlabel = 5, ...)
}
\arguments{
  \item{Tweets}{A data frame with at least one vector of names.}
  \item{author}{Vector name from data containing the profiles (users' names).}
  \item{text}{Vector name from data containing the messages (tweets).}
  \item{date}{Vector name from data containing the date when the messages were written.}
  \item{fields}{Vector containing "followers" (number of people who follows a user), "following" (people followed by user), "statuses" (number of tweets written) and "location" (from where is the user).}
  \item{beginDate}{Date of first available message.}
  \item{endDate}{Date of last available message.}
  \item{interval}{The end-points of the interval.}
  \item{tzone}{Time zone (e.g., "Europe/Paris").}
  \item{tCutpoints}{Date vector with the cutpoints for dynamic graphs.}    \item{nFrames}{The number of frames to render (default = Inf).}
  \item{seed}{A single value, interpreted as an integer, or NULL.}
  \item{n}{Sample the network if the number of nodes is greater than 5000 or n.}
  \item{minlabel}{Avoid label of nodes with a frequency less than # minlabel.}
  \item{...}{Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument.}
}
\details{
This function produces a dynamic netCoin network that allows to recreate the timeline of messages and interactions on Twitter. Any \code{\link[netCoin]{netCoin}} and/or \code{\link[netCoin]{allNet}} argument can be used in order to specify the network produced. 
}
\value{
This function creates a list containing three objects: "authors" (a data.frame containing names, links, Tweets and Mentions), "messages" (a data.frame containing dates, authors and texts) and "nets", (a \code{\link[netCoin]{multigraphCreate}} object).

This object can be plotted with the plot() function, creating a folder in the computer with an HTML document named index.html which contains the produced graph(s). This file can be directly opened with your browser and sent to a web server to work properly.
}
\author{
Modesto Escobar, Department of Sociology and Communication, University of Salamanca. See \url{https://sociocav.usal.es/blog/modesto-escobar/}
}
\examples{
## Example using a sample of tweets and retweets with 
## the hashtag #Shakespeare:

data(Shakespeare)
g <- d_retweet(Shakespeare)

## You can use all kind of netCoin arguments:

g2 <- d_retweet(Shakespeare, distance= 3, repulsion= 3,
                 main = "Dynamic  graph of retweets using #Shakespeare")
}
